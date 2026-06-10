%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Unit tests for the auth-validation subsystem covering all four pure
%% modules: rate limiter, semaphore, registry, and the LDAP backend's
%% input parsing. The HTTP pipeline lives in
%% aws_auth_validate_mgmt_SUITE.
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
        fun stop/1,
        [
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
        fun stop/1,
        fun(_) ->
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
        fun stop/1,
        [
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
        fun stop/1,
        fun(_) ->
            Self = self(),
            Worker = spawn(fun() ->
                {ok, _Ref} = aws_auth_validate_semaphore:acquire(),
                Self ! acquired,
                receive
                    die -> exit(boom)
                end
            end),
            receive acquired -> ok after 1_000 -> ?assert(false) end,
            ?assertEqual({error, full}, aws_auth_validate_semaphore:acquire()),
            Worker ! die,
            wait_until_zero(50),
            [?_assertMatch({ok, _}, aws_auth_validate_semaphore:acquire())]
        end}.

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
        0 -> ok;
        _ -> timer:sleep(10), wait_until_zero(N - 1)
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
