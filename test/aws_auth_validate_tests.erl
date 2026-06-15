%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Unit tests for the auth-validation subsystem's pure modules: rate
%% limiter, semaphore, registry, the LDAP backend's input parsing, and the
%% LDAP query DSL parser (incl. upstream parity). The live bind/connect/TLS
%% path lives in aws_auth_validate_ldap_SUITE; the HTTP pipeline (when added)
%% lives in aws_auth_validate_mgmt_SUITE.
-module(aws_auth_validate_tests).

-include_lib("eunit/include/eunit.hrl").

-define(IP1, {10, 0, 0, 1}).
-define(IP2, {10, 0, 0, 2}).

%%--------------------------------------------------------------------
%% Rate limiter
%%--------------------------------------------------------------------

rate_limiter_test_() ->
    {foreach,
        fun() ->
            {ok, Pid} = aws_auth_validate_rate_limiter:start_link(#{
                window_ms => 60_000,
                max_per_window => 3,
                sweep_interval_ms => 60_000
            }),
            Pid
        end,
        fun stop/1, [
            {"allows up to max then rejects", fun() ->
                ?assertEqual(ok, aws_auth_validate_rate_limiter:check(?IP1)),
                ?assertEqual(ok, aws_auth_validate_rate_limiter:check(?IP1)),
                ?assertEqual(ok, aws_auth_validate_rate_limiter:check(?IP1)),
                ?assertEqual(
                    {error, rate_limited},
                    aws_auth_validate_rate_limiter:check(?IP1)
                )
            end},
            {"per-IP isolation", fun() ->
                ok = aws_auth_validate_rate_limiter:check(?IP1),
                ok = aws_auth_validate_rate_limiter:check(?IP1),
                ok = aws_auth_validate_rate_limiter:check(?IP1),
                ?assertEqual(
                    {error, rate_limited},
                    aws_auth_validate_rate_limiter:check(?IP1)
                ),
                ?assertEqual(ok, aws_auth_validate_rate_limiter:check(?IP2))
            end},
            {"reset clears counters", fun() ->
                ok = aws_auth_validate_rate_limiter:check(?IP1),
                ok = aws_auth_validate_rate_limiter:check(?IP1),
                ok = aws_auth_validate_rate_limiter:check(?IP1),
                ?assertEqual(
                    {error, rate_limited},
                    aws_auth_validate_rate_limiter:check(?IP1)
                ),
                ok = aws_auth_validate_rate_limiter:reset(),
                ?assertEqual(ok, aws_auth_validate_rate_limiter:check(?IP1))
            end}
        ]}.

rate_limiter_window_expiry_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = aws_auth_validate_rate_limiter:start_link(#{
                window_ms => 50,
                max_per_window => 1,
                sweep_interval_ms => 60_000
            }),
            Pid
        end,
        fun stop/1, fun(_) ->
            ok = aws_auth_validate_rate_limiter:check(?IP1),
            ?assertEqual(
                {error, rate_limited},
                aws_auth_validate_rate_limiter:check(?IP1)
            ),
            timer:sleep(80),
            [?_assertEqual(ok, aws_auth_validate_rate_limiter:check(?IP1))]
        end}.

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

arn_lock_test_() ->
    {foreach,
        fun() ->
            {ok, Pid} = aws_auth_validate_arn_lock:start_link(),
            Pid
        end,
        fun stop/1, [
            {"returns the closure's value", fun() ->
                ?assertEqual(42, aws_auth_validate_arn_lock:with_lock(fun() -> 42 end))
            end},
            {"serializes concurrent callers (no interleaving)", fun() ->
                %% Each closure marks the lock busy on entry and clears it on
                %% exit; if two ran concurrently one would observe busy=true.
                %% A shared ets table records whether overlap was ever seen.
                T = ets:new(arn_lock_probe, [public, set]),
                ets:insert(T, {busy, false}),
                ets:insert(T, {overlap, false}),
                Self = self(),
                Run = fun() ->
                    aws_auth_validate_arn_lock:with_lock(fun() ->
                        case ets:lookup(T, busy) of
                            [{busy, true}] -> ets:insert(T, {overlap, true});
                            _ -> ok
                        end,
                        ets:insert(T, {busy, true}),
                        timer:sleep(15),
                        ets:insert(T, {busy, false}),
                        ok
                    end),
                    Self ! done
                end,
                Pids = [spawn(Run) || _ <- lists:seq(1, 5)],
                [
                    receive
                        done -> ok
                    after 5_000 -> ?assert(false)
                    end
                 || _ <- Pids
                ],
                ?assertEqual([{overlap, false}], ets:lookup(T, overlap)),
                ets:delete(T)
            end},
            {"re-raises the closure's exception in the caller", fun() ->
                ?assertError(
                    boom,
                    aws_auth_validate_arn_lock:with_lock(fun() -> erlang:error(boom) end)
                )
            end},
            {"survives a crashing closure (next call still works)", fun() ->
                catch aws_auth_validate_arn_lock:with_lock(fun() -> erlang:error(boom) end),
                ?assertEqual(ok, aws_auth_validate_arn_lock:with_lock(fun() -> ok end))
            end}
        ]}.

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
