%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Unit tests for the auth-validation subsystem's pure modules: semaphore,
%% ARN-resolution lock, registry, the LDAP backend's input parsing, and the
%% LDAP query DSL parser (incl. upstream parity). The live bind/connect/TLS
%% path lives in aws_auth_validate_ldap_SUITE; the HTTP pipeline lives in
%% aws_auth_validate_mgmt_SUITE.
-module(aws_auth_validate_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Semaphore
%%--------------------------------------------------------------------

semaphore_test_() ->
    {foreach,
        fun() ->
            {ok, Pid} = aws_auth_validate_semaphore:start_link(#{max => 2}),
            Pid
        end,
        fun stop/1, [
            {"acquire/release sequence", fun() ->
                {ok, R1} = aws_auth_validate_semaphore:acquire(),
                {ok, R2} = aws_auth_validate_semaphore:acquire(),
                ?assertEqual({error, full}, aws_auth_validate_semaphore:acquire()),
                ok = aws_auth_validate_semaphore:release(R1),
                {ok, R3} = aws_auth_validate_semaphore:acquire(),
                ok = aws_auth_validate_semaphore:release(R2),
                ok = aws_auth_validate_semaphore:release(R3),
                ?assertEqual(0, aws_auth_validate_semaphore:current())
            end}
        ]}.

semaphore_crashed_holder_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = aws_auth_validate_semaphore:start_link(#{max => 1}),
            Pid
        end,
        fun stop/1, fun(_) ->
            Self = self(),
            Worker = spawn(fun() ->
                {ok, _Ref} = aws_auth_validate_semaphore:acquire(),
                Self ! acquired,
                receive
                    die -> exit(boom)
                end
            end),
            receive
                acquired -> ok
            after 1_000 -> ?assert(false)
            end,
            ?assertEqual({error, full}, aws_auth_validate_semaphore:acquire()),
            Worker ! die,
            wait_until_zero(50),
            [?_assertMatch({ok, _}, aws_auth_validate_semaphore:acquire())]
        end}.

%%--------------------------------------------------------------------
%% ARN resolution lock
%%--------------------------------------------------------------------

%% The lock is now a global:trans/4 lock with no server process, so there is
%% nothing to start or stop -- with_lock/1 is callable directly. These cases
%% exercise OUR wrapper's contract: it returns the closure's value, propagates
%% an exception to the caller, and releases the lock even when the closure
%% crashes.
%%
%% We deliberately do NOT unit-test that concurrent callers are serialized.
%% Serialization is a property of global:trans/4 (OTP stdlib), which rabbit
%% itself relies on in production (feature-flag state changes, stream
%% coordinator startup) without unit-testing it. A timing/overlap probe of it
%% is non-deterministic across the OTP matrix -- it passed on OTP 28 but
%% failed intermittently on OTP 27 in CI (global's scheduling of competing
%% set_lock waiters is not guaranteed in wall-clock terms), testing the
%% library rather than our 3-line wrapper. See [[ci-failure-gotchas]]:
%% timing-based concurrency assertions do not belong in this suite.
arn_lock_test_() ->
    [
        {"returns the closure's value", fun() ->
            ?assertEqual(42, aws_auth_validate_arn_lock:with_lock(fun() -> 42 end))
        end},
        {"propagates the closure's exception to the caller", fun() ->
            ?assertError(
                boom,
                aws_auth_validate_arn_lock:with_lock(fun() -> erlang:error(boom) end)
            )
        end},
        {"releases the lock after a crashing closure (next call still works)", fun() ->
            catch aws_auth_validate_arn_lock:with_lock(fun() -> erlang:error(boom) end),
            ?assertEqual(ok, aws_auth_validate_arn_lock:with_lock(fun() -> ok end))
        end}
    ].

%%--------------------------------------------------------------------
%% Registry
%%--------------------------------------------------------------------

registry_unknown_method_test() ->
    ?assertEqual(
        {error, unknown_method},
        aws_auth_validate_registry:dispatch(<<"nope">>, #{})
    ).

registry_method_disabled_test_() ->
    {setup,
        fun() ->
            application:set_env(
                aws,
                auth_validation_enabled_methods,
                [{<<"ldap">>, false}]
            )
        end,
        fun(_) ->
            application:unset_env(aws, auth_validation_enabled_methods)
        end,
        [
            ?_assertEqual(
                {error, method_disabled},
                aws_auth_validate_registry:dispatch(<<"ldap">>, #{})
            )
        ]}.

registry_field_filter_override_test_() ->
    {setup,
        fun() ->
            application:set_env(
                aws,
                {auth_validation_allowed_fields_override, <<"ldap">>},
                [<<"servers">>, <<"port">>, <<"unknown">>]
            )
        end,
        fun(_) ->
            application:unset_env(
                aws,
                {auth_validation_allowed_fields_override, <<"ldap">>}
            )
        end,
        [
            fun() ->
                Effective = aws_auth_validate_registry:effective_allowed_fields(
                    aws_auth_validate_ldap, <<"ldap">>
                ),
                ?assert(lists:member(<<"servers">>, Effective)),
                ?assert(lists:member(<<"port">>, Effective)),
                ?assertNot(lists:member(<<"unknown">>, Effective))
            end
        ]}.

%%--------------------------------------------------------------------
%% LDAP backend (input parsing only - the real bind path needs slapd)
%%--------------------------------------------------------------------

ldap_method_name_test() ->
    ?assertEqual(<<"ldap">>, aws_auth_validate_ldap:method_name()).

ldap_allowed_fields_test() ->
    Fields = aws_auth_validate_ldap:allowed_fields(),
    [
        ?assert(lists:member(F, Fields))
     || F <- [
            <<"servers">>,
            <<"port">>,
            <<"user_dn">>,
            <<"password_arn">>,
            <<"use_ssl">>,
            <<"use_starttls">>,
            <<"ssl_options">>,
            <<"dn_lookup_base">>,
            <<"dn_lookup_attribute">>,
            <<"queries">>
        ]
    ].

%% These all fail in the pure (network-free) validation pipeline, before any
%% password ARN resolution or outbound connection is attempted.
ldap_input_validation_test_() ->
    [
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_ldap:validate(
                #{<<"port">> => 389, <<"user_dn">> => <<"u">>}
            )
        ),
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_ldap:validate(base_body(#{<<"servers">> => []}))
        ),
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_ldap:validate(base_body(#{<<"port">> => 0}))
        ),
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_ldap:validate(base_body(#{<<"port">> => 65536}))
        ),
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_ldap:validate(base_body(#{<<"user_dn">> => <<>>}))
        ),
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_ldap:validate(base_body(#{<<"ssl_options">> => <<"x">>}))
        ),
        %% An unknown ssl_options key is rejected, not silently dropped.
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_ldap:validate(
                base_body(#{<<"ssl_options">> => #{<<"verfy">> => <<"verify_peer">>}})
            )
        ),
        %% A known key with a mis-typed value is rejected in the pure phase
        %% (rather than dropped and re-defaulted).
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_ldap:validate(
                base_body(#{<<"ssl_options">> => #{<<"verify">> => <<"verfy_none">>}})
            )
        ),
        %% depth must be a non-negative integer.
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_ldap:validate(
                base_body(#{<<"ssl_options">> => #{<<"depth">> => -1}})
            )
        ),
        %% versions must be a list of known TLS versions.
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_ldap:validate(
                base_body(#{<<"ssl_options">> => #{<<"versions">> => [<<"sslv3">>]}})
            )
        )
    ].

ldap_config_conflict_test() ->
    Body = base_body(#{<<"use_ssl">> => true, <<"use_starttls">> => true}),
    ?assertMatch({error, config_conflict, _}, aws_auth_validate_ldap:validate(Body)).

%%--------------------------------------------------------------------
%% DN lookup + authorization query input validation (pure pipeline)
%%--------------------------------------------------------------------

ldap_dn_lookup_input_test_() ->
    [
        %% Wrong types for the optional DN-lookup fields are rejected.
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_ldap:validate(base_body(#{<<"dn_lookup_base">> => 123}))
        ),
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_ldap:validate(base_body(#{<<"dn_lookup_base">> => <<>>}))
        ),
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_ldap:validate(base_body(#{<<"dn_lookup_attribute">> => 1}))
        )
    ].

ldap_queries_input_test_() ->
    [
        %% queries must be an object.
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_ldap:validate(base_body(#{<<"queries">> => <<"nope">>}))
        ),
        %% A non-string query value is a shape error.
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_ldap:validate(
                base_body(#{<<"queries">> => #{<<"tags">> => 123}})
            )
        ),
        %% A syntactically invalid query string is query_invalid (400).
        ?_assertMatch(
            {error, query_invalid, _},
            aws_auth_validate_ldap:validate(
                base_body(#{<<"queries">> => #{<<"vhost_access">> => <<"{garbage,">>}})
            )
        ),
        %% A grammatically-valid query that references a disallowed top-level
        %% term is also query_invalid.
        ?_assertMatch(
            {error, query_invalid, _},
            aws_auth_validate_ldap:validate(
                base_body(#{<<"queries">> => #{<<"tags">> => <<"{bogus_term, 1, 2}">>}})
            )
        )
    ].

%%--------------------------------------------------------------------
%% R6: password must never reach a crash report / log, even on a raise
%%--------------------------------------------------------------------

%% A unique, recognisable sentinel standing in for the resolved bind password.
%% If it appears anywhere in a crash report or log line, R6 is violated.
-define(SECRET, "S3cr3t-Sentinel-Passw0rd-DO-NOT-LEAK").

%% do_ldap_validate/1 is not exported, so reach it via validate/1's public
%% entry point would require a real ARN resolve. Instead we drive the same code
%% path the handler does -- a successful open() followed by a *raising*
%% simple_bind() -- with eldap mocked, and assert: (a) the result collapses to
%% the fixed connection_failed category (never propagates), and (b) the secret
%% byte-string does not appear in the formatted exception/stacktrace that a
%% crash report would render. We exercise validate/1 with the ARN resolver and
%% the eldap module both mocked so no network or AWS call occurs.
ldap_bind_raise_does_not_leak_password_test_() ->
    {setup,
        fun() ->
            ok = meck:new(eldap, [unstick, non_strict]),
            ok = meck:new(aws_arn_util, [passthrough]),
            ok = meck:new(aws_auth_validate_arn_lock, [passthrough]),
            %% with_lock just runs the closure inline for the test.
            meck:expect(aws_auth_validate_arn_lock, with_lock, fun(F) -> F() end),
            %% Resolve any ARN to our sentinel password.
            meck:expect(aws_arn_util, resolve_arn, fun(_) -> {ok, ?SECRET} end),
            %% open() succeeds, then simple_bind/3 RAISES with the password
            %% present in the failing call's arguments -- the worst case for a
            %% crash-report leak.
            meck:expect(eldap, open, fun(_Servers, _Opts) -> {ok, fake_handle} end),
            meck:expect(eldap, close, fun(_H) -> ok end),
            meck:expect(eldap, simple_bind, fun(_H, _Dn, Pw) ->
                erlang:error({ldap_blew_up, Pw})
            end),
            ok
        end,
        fun(_) ->
            meck:unload(eldap),
            meck:unload(aws_arn_util),
            meck:unload(aws_auth_validate_arn_lock)
        end,
        fun(_) ->
            Body = bind_body(),
            %% (a) A raise in the bind path collapses to a fixed category and
            %% never propagates.
            Result = aws_auth_validate_ldap:validate(Body),
            %% (b) The secret never appears in the rendered result term.
            Rendered = lists:flatten(io_lib:format("~p", [Result])),
            [
                ?_assertMatch({error, connection_failed, _}, Result),
                ?_assertEqual(nomatch, string_find(Rendered, ?SECRET))
            ]
        end}.

%% A body that passes the pure pipeline so resolve_password/2 (mocked) and the
%% bind path are actually reached. use_ssl/use_starttls default to false.
bind_body() ->
    #{
        <<"servers">> => [<<"127.0.0.1">>],
        <<"port">> => 389,
        <<"user_dn">> => <<"cn=u,dc=example,dc=com">>,
        <<"password_arn">> =>
            <<"arn:aws:secretsmanager:us-east-1:111111111111:secret:x">>
    }.

string_find(Haystack, Needle) ->
    string:find(Haystack, Needle).

%%--------------------------------------------------------------------
%% LDAP query DSL parser (aws_auth_validate_ldap_query)
%%--------------------------------------------------------------------

parse_accepts_test_() ->
    [
        ?_assertMatch({ok, _}, aws_auth_validate_ldap_query:parse(Q))
     || Q <- accepted_queries()
    ].

parse_rejects_test_() ->
    [
        ?_assertMatch({error, _}, aws_auth_validate_ldap_query:parse(Q))
     || Q <- rejected_queries()
    ].

parse_accepts_string_input_test() ->
    ?assertMatch({ok, _}, aws_auth_validate_ldap_query:parse("{constant, true}")).

%% Literal DN extraction (the placeholder-free DNs used for static reachability)

literal_dns_in_group_test() ->
    {ok, Q} = aws_auth_validate_ldap_query:parse(
        <<"{in_group, \"cn=admins,ou=groups,dc=example,dc=com\"}">>
    ),
    ?assertEqual(
        ["cn=admins,ou=groups,dc=example,dc=com"],
        aws_auth_validate_ldap_query:literal_dns(Q)
    ).

literal_dns_skips_placeholder_test() ->
    %% A DN with a ${...} placeholder is runtime-filled, not literal, so it
    %% contributes no static reachability check.
    {ok, Q} = aws_auth_validate_ldap_query:parse(
        <<"{in_group, \"cn=${username},ou=groups,dc=example,dc=com\"}">>
    ),
    ?assertEqual([], aws_auth_validate_ldap_query:literal_dns(Q)).

literal_dns_constant_test() ->
    {ok, Q} = aws_auth_validate_ldap_query:parse(<<"{constant, true}">>),
    ?assertEqual([], aws_auth_validate_ldap_query:literal_dns(Q)).

literal_dns_nested_and_or_test() ->
    {ok, Q} = aws_auth_validate_ldap_query:parse(
        <<"{'or', [{in_group, \"cn=a,dc=x\"}, {'and', [{in_group, \"cn=b,dc=x\"}]}]}">>
    ),
    ?assertEqual(
        ["cn=a,dc=x", "cn=b,dc=x"],
        aws_auth_validate_ldap_query:literal_dns(Q)
    ).

literal_dns_tag_queries_test() ->
    {ok, Q} = aws_auth_validate_ldap_query:parse(
        <<"[{administrator, {in_group, \"cn=admins,dc=x\"}}, {management, {constant, true}}]">>
    ),
    ?assertEqual(["cn=admins,dc=x"], aws_auth_validate_ldap_query:literal_dns(Q)).

literal_dns_exists_test() ->
    %% parse/1 rejects {exists,_} (parse_query/1 does not allow it), but
    %% literal_dns/1 still walks the term defensively, so test it on a
    %% directly-constructed term rather than via parse/1.
    ?assertEqual(
        ["ou=users,dc=x"],
        aws_auth_validate_ldap_query:literal_dns({exists, "ou=users,dc=x"})
    ).

literal_dns_for_test() ->
    {ok, Q} = aws_auth_validate_ldap_query:parse(
        <<"{for, [{permission, configure, {in_group, \"cn=cfg,dc=x\"}}]}">>
    ),
    ?assertEqual(["cn=cfg,dc=x"], aws_auth_validate_ldap_query:literal_dns(Q)).

%% Regression tests for collect/2 DN extraction: value operands of
%% equals/match and bare-string queries must NOT be treated as DNs, while a
%% literal DN nested in an {attribute, DN, _} operand still is.

literal_dns_equals_placeholder_attribute_test() ->
    %% The DN slot of the attribute is a placeholder, so no literal DN; the
    %% value operand "engineering" is a value, never a DN.
    {ok, Q} = aws_auth_validate_ldap_query:parse(
        <<"{equals, {attribute, \"${u}\", \"dept\"}, \"engineering\"}">>
    ),
    ?assertEqual([], aws_auth_validate_ldap_query:literal_dns(Q)).

literal_dns_equals_literal_attribute_test() ->
    %% A literal DN inside the attribute operand IS extracted; the value
    %% operand "v" is still ignored.
    {ok, Q} = aws_auth_validate_ldap_query:parse(
        <<"{equals, {attribute, \"cn=x,dc=y\", \"dept\"}, \"v\"}">>
    ),
    ?assertEqual(["cn=x,dc=y"], aws_auth_validate_ldap_query:literal_dns(Q)).

literal_dns_match_value_operands_test() ->
    %% Both operands of match are values (a string and a regex), never DNs.
    {ok, Q} = aws_auth_validate_ldap_query:parse(
        <<"{match, \"${username}\", \"^a.*\"}">>
    ),
    ?assertEqual([], aws_auth_validate_ldap_query:literal_dns(Q)).

literal_dns_bare_string_query_test() ->
    %% A top-level bare-string query parses to a character list; it is a
    %% value, not a DN, so it contributes nothing.
    {ok, Q} = aws_auth_validate_ldap_query:parse(<<"\"just a string\"">>),
    ?assertEqual([], aws_auth_validate_ldap_query:literal_dns(Q)).

%% Parity with rabbit_auth_backend_ldap_util:parse_query/1 (design req R12).
%% The broker's parser throws via cuttlefish:invalid/2 on rejection; ours
%% returns {error, _}. Assert both classify each corpus entry the same way.
%%
%% Runs only when upstream parse_query/1 is actually usable in this node.
%% It is not enough that the module loads: parse_query/1 calls
%% rabbit_data_coercion (and cuttlefish:invalid on rejection), and across
%% the RMQ-version CI matrix those deps are not always loaded in the bare
%% eunit node. If they are missing, every parse_query/1 call throws undef
%% and upstream_accepts/1 would report "rejected" for EVERYTHING, making the
%% test fail spuriously. So we probe upstream with a known-good and a
%% known-bad query first and skip the whole parity check unless upstream
%% classifies both correctly.
parity_test_() ->
    case upstream_parser_usable() of
        true ->
            [
                {
                    binary_to_list(Q),
                    ?_assertEqual(upstream_accepts(Q), ours_accepts(Q))
                }
             || Q <- accepted_queries() ++ rejected_queries()
            ];
        false ->
            []
    end.

%% True only if rabbit_auth_backend_ldap_util:parse_query/1 is loaded AND its
%% transitive deps are available, verified by a positive+negative probe.
upstream_parser_usable() ->
    code:ensure_loaded(rabbit_auth_backend_ldap_util) =/= {error, nofile} andalso
        erlang:function_exported(rabbit_auth_backend_ldap_util, parse_query, 1) andalso
        upstream_accepts(<<"{constant, true}">>) andalso
        not upstream_accepts(<<"{bogus_term, 1, 2}">>).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

stop(Pid) ->
    unlink(Pid),
    exit(Pid, kill),
    timer:sleep(10),
    ok.

wait_until_zero(0) ->
    ?assertEqual(0, aws_auth_validate_semaphore:current());
wait_until_zero(N) ->
    case aws_auth_validate_semaphore:current() of
        0 ->
            ok;
        _ ->
            timer:sleep(10),
            wait_until_zero(N - 1)
    end.

%% A minimally-valid body for the pure validation pipeline. Note: the tests
%% that use this assert failures triggered *before* password_arn resolution,
%% so the ARN here is never resolved (no AWS call is made).
base_body(Overrides) when is_map(Overrides) ->
    Base = #{
        <<"servers">> => [<<"127.0.0.1">>],
        <<"port">> => 389,
        <<"user_dn">> => <<"cn=u">>,
        <<"password_arn">> => <<"arn:aws:secretsmanager:us-east-1:111111111111:secret:x">>
    },
    maps:merge(Base, Overrides).

%% Query corpus shared by the parser accept/reject tests and the parity test.

%% Queries the broker accepts. Kept in sync with the parity test.
accepted_queries() ->
    [
        <<"{constant, true}">>,
        <<"{constant, false}">>,
        <<"{in_group, \"cn=admins,ou=groups,dc=example,dc=com\"}">>,
        <<"{in_group_nested, \"cn=g,dc=example,dc=com\", \"member\"}">>,
        <<"{'not', {constant, true}}">>,
        <<"{'and', [{constant, true}, {constant, false}]}">>,
        <<"{'or', [{constant, true}, {constant, false}]}">>,
        <<"{equals, \"${username}\", \"admin\"}">>,
        <<"{match, \"${username}\", \"^a.*\"}">>,
        <<"{for, [{permission, configure, {constant, true}}]}">>,
        %% tag_queries form: a list of {Tag, SubQuery} pairs.
        <<"[{administrator, {in_group, \"cn=admins,dc=example,dc=com\"}}]">>,
        %% trailing dot already present
        <<"{constant, true}.">>,
        %% A bare quoted string parses to an Erlang list, so it hits the
        %% is_list/1 (tag_queries) clause in BOTH parsers. Included here to
        %% pin that parity quirk rather than to endorse it as a useful query.
        <<"\"just a string\"">>
    ].

rejected_queries() ->
    [
        <<"{garbage,">>,
        <<"not even erlang">>,
        <<"{bogus_term, 1, 2}">>,
        <<"42">>,
        <<>>,
        %% Forms the runtime evaluator handles but parse_query/1 (the config
        %% gate we mirror) rejects, so the endpoint rejects them too.
        <<"{in_group, \"cn=g,dc=example,dc=com\", \"member\"}">>,
        <<"{exists, \"ou=users,dc=example,dc=com\"}">>,
        <<"{attribute, \"cn=g,dc=example,dc=com\", \"member\"}">>
    ].

ours_accepts(Q) ->
    case aws_auth_validate_ldap_query:parse(Q) of
        {ok, _} -> true;
        {error, _} -> false
    end.

upstream_accepts(Q) ->
    try rabbit_auth_backend_ldap_util:parse_query(Q) of
        %% cuttlefish:invalid/2 throws on rejection; any throw/exit means the
        %% upstream parser rejected the query.
        _ -> true
    catch
        _:_ -> false
    end.
