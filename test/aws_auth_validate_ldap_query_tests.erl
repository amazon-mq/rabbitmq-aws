%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Unit tests for the self-contained LDAP authorization-query parser
%% (aws_auth_validate_ldap_query), plus a parity test asserting it agrees
%% with the broker's rabbit_auth_backend_ldap_util:parse_query/1 on
%% accept/reject for a representative corpus (design requirement R12).
-module(aws_auth_validate_ldap_query_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Accept / reject
%%--------------------------------------------------------------------

%% Queries the broker accepts. Kept in sync with the parity test below.
accepted_queries() ->
    [
        <<"{constant, true}">>,
        <<"{constant, false}">>,
        <<"{in_group, \"cn=admins,ou=groups,dc=example,dc=com\"}">>,
        <<"{in_group, \"cn=g,dc=example,dc=com\", \"member\"}">>,
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
        <<>>
    ].

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

%%--------------------------------------------------------------------
%% Literal DN extraction
%%--------------------------------------------------------------------

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
    {ok, Q} = aws_auth_validate_ldap_query:parse(<<"{exists, \"ou=users,dc=x\"}">>),
    ?assertEqual(["ou=users,dc=x"], aws_auth_validate_ldap_query:literal_dns(Q)).

literal_dns_for_test() ->
    {ok, Q} = aws_auth_validate_ldap_query:parse(
        <<"{for, [{permission, configure, {in_group, \"cn=cfg,dc=x\"}}]}">>
    ),
    ?assertEqual(["cn=cfg,dc=x"], aws_auth_validate_ldap_query:literal_dns(Q)).

%%--------------------------------------------------------------------
%% Parity with rabbit_auth_backend_ldap_util:parse_query/1 (R12)
%%--------------------------------------------------------------------

%% The broker's parser throws via cuttlefish:invalid/2 on rejection; ours
%% returns {error, _}. This asserts both classify each corpus entry the same
%% way (both accept, or both reject). Skipped gracefully if the upstream
%% module is not on the code path.
parity_test_() ->
    case code:ensure_loaded(rabbit_auth_backend_ldap_util) of
        {module, _} ->
            [
                {binary_to_list(Q), ?_assertEqual(
                    upstream_accepts(Q),
                    ours_accepts(Q)
                )}
             || Q <- accepted_queries() ++ rejected_queries()
            ];
        {error, _} ->
            %% Upstream not available in this build; nothing to compare.
            []
    end.

ours_accepts(Q) ->
    case aws_auth_validate_ldap_query:parse(Q) of
        {ok, _} -> true;
        {error, _} -> false
    end.

upstream_accepts(Q) ->
    try rabbit_auth_backend_ldap_util:parse_query(Q) of
        _ -> true
    catch
        %% cuttlefish:invalid/2 throws a {invalid, _} tuple; any throw/exit
        %% from the parser means it rejected the query.
        _:_ -> false
    end.
