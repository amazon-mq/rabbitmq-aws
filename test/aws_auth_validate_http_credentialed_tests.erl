%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Unit tests for the credentialed-probe mode added to aws_auth_validate_http.
%% Tests cover credential-pair parsing (both-or-neither enforcement), the
%% credentialed response classifier, query-string shape, and the
%% no-password-in-error security invariant.
%%
%% Tests that exercise the full validate/1 path mock external dependencies
%% (aws_auth_validate_net, aws_auth_validate_ssl, aws_iam, etc.) to isolate
%% the parse logic.
-module(aws_auth_validate_http_credentialed_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% parse_credentials/2 tests (exported under -ifdef(TEST))
%%--------------------------------------------------------------------

%% Both username and password_arn supplied -- credentialed mode.
parse_credentials_both_present_test() ->
    Body = #{
        <<"username">> => <<"alice">>,
        <<"password_arn">> => <<"arn:aws:secretsmanager:us-east-1:111:secret:pw">>
    },
    Acc = #{},
    {ok, Result} = aws_auth_validate_http:parse_credentials(Body, Acc),
    ?assertEqual(credentialed, maps:get(credential_mode, Result)),
    ?assertEqual(<<"alice">>, maps:get(username, Result)),
    ?assertEqual(
        <<"arn:aws:secretsmanager:us-east-1:111:secret:pw">>,
        maps:get(password_arn, Result)
    ).

%% Neither username nor password_arn supplied -- reachability mode.
parse_credentials_neither_present_test() ->
    Body = #{<<"user_path">> => <<"https://8.8.8.8/auth">>},
    Acc = #{},
    {ok, Result} = aws_auth_validate_http:parse_credentials(Body, Acc),
    ?assertEqual(reachability, maps:get(credential_mode, Result)).

%% Only username supplied -- input_invalid.
parse_credentials_only_username_test() ->
    Body = #{<<"username">> => <<"alice">>},
    Acc = #{},
    ?assertMatch(
        {error, input_invalid, <<"username and password_arn must be supplied together">>},
        aws_auth_validate_http:parse_credentials(Body, Acc)
    ).

%% Only password_arn supplied -- input_invalid.
parse_credentials_only_password_arn_test() ->
    Body = #{<<"password_arn">> => <<"arn:aws:secretsmanager:us-east-1:111:secret:pw">>},
    Acc = #{},
    ?assertMatch(
        {error, input_invalid, <<"username and password_arn must be supplied together">>},
        aws_auth_validate_http:parse_credentials(Body, Acc)
    ).

%% Empty binary username with valid password_arn -- input_invalid.
parse_credentials_empty_username_test() ->
    Body = #{
        <<"username">> => <<>>,
        <<"password_arn">> => <<"arn:aws:secretsmanager:us-east-1:111:secret:pw">>
    },
    Acc = #{},
    ?assertMatch(
        {error, input_invalid, <<"username and password_arn must be supplied together">>},
        aws_auth_validate_http:parse_credentials(Body, Acc)
    ).

%%--------------------------------------------------------------------
%% classify_credentialed_response/1 tests
%%--------------------------------------------------------------------

%% "allow" body -> ok.
classify_credentialed_response_allow_test() ->
    ?assertEqual(ok, aws_auth_validate_http:classify_credentialed_response(<<"allow">>)).

%% "allow administrator" (allow with tags) -> ok.
classify_credentialed_response_allow_tags_test() ->
    ?assertEqual(
        ok, aws_auth_validate_http:classify_credentialed_response(<<"allow administrator">>)
    ).

%% "Allow" (mixed case) -> ok (normalize lowercases).
classify_credentialed_response_allow_uppercase_test() ->
    ?assertEqual(ok, aws_auth_validate_http:classify_credentialed_response(<<"Allow">>)).

%% "deny" body -> auth_failed.
classify_credentialed_response_deny_test() ->
    ?assertMatch(
        {error, auth_failed, <<"HTTP auth server denied the supplied credentials">>},
        aws_auth_validate_http:classify_credentialed_response(<<"deny">>)
    ).

%% "deny bad password" body -> auth_failed.
classify_credentialed_response_deny_with_reason_test() ->
    ?assertMatch(
        {error, auth_failed, _},
        aws_auth_validate_http:classify_credentialed_response(<<"deny bad password">>)
    ).

%% Non-auth-shaped body -> auth_failed with REASON_ENDPOINT (not an auth server),
%% distinct from a well-formed deny (which uses REASON_AUTH_DENIED). This lets the
%% operator distinguish "wrong endpoint" from "credentials rejected".
classify_credentialed_response_garbage_test() ->
    ?assertMatch(
        {error, auth_failed, <<"HTTP auth server did not return a usable response">>},
        aws_auth_validate_http:classify_credentialed_response(<<"hello">>)
    ).

%% Empty body -> auth_failed with REASON_ENDPOINT (not an auth server).
classify_credentialed_response_empty_test() ->
    ?assertMatch(
        {error, auth_failed, <<"HTTP auth server did not return a usable response">>},
        aws_auth_validate_http:classify_credentialed_response(<<>>)
    ).

%%--------------------------------------------------------------------
%% credentialed_query_for/1 tests
%%--------------------------------------------------------------------

%% Query contains username= and password= (matching rabbit_auth_backend_http's
%% user check). The query parameters are URI-encoded.
credentialed_query_parity_test() ->
    Params = #{username => <<"alice">>, password => <<"s3cr3t">>},
    Query = aws_auth_validate_http:credentialed_query_for(Params),
    %% Must contain both fields
    ?assertNotEqual(nomatch, string:find(Query, "username=alice")),
    ?assertNotEqual(nomatch, string:find(Query, "password=s3cr3t")).

%% Verify special characters are properly encoded in the query.
credentialed_query_encoding_test() ->
    Params = #{username => <<"user@host">>, password => <<"p&ss=w0rd">>},
    Query = aws_auth_validate_http:credentialed_query_for(Params),
    %% The special characters inside the values are percent-encoded: @ -> %40,
    %% and the password's literal & and = become %26 and %3D. The single
    %% unencoded `&' separating username= from password= is the legitimate
    %% parameter delimiter, so we assert on the encoded forms rather than the
    %% raw characters' total absence.
    ?assertEqual(nomatch, string:find(Query, "@")),
    ?assertNotEqual(nomatch, string:find(Query, "%40")),
    ?assertNotEqual(nomatch, string:find(Query, "p%26ss%3Dw0rd")),
    ?assertEqual(nomatch, string:find(Query, "=w0rd")).

%%--------------------------------------------------------------------
%% Security: password never appears in error tuples
%%--------------------------------------------------------------------

%% When resolve_credential fails (e.g. ARN resolution error), the error tuple
%% must not contain the resolved password value.
password_never_in_error_test() ->
    %% The ARN-resolve reason is fixed and field-attributed; it never contains
    %% the resolved content. Verify the reason string names the field, not a
    %% password value.
    Reason = aws_auth_validate_ssl:arn_resolve_reason([<<"password_arn">>]),
    ?assertNotEqual(nomatch, string:find(Reason, "password_arn")),
    %% A hypothetical resolved password should never appear in this reason:
    ?assertEqual(nomatch, string:find(Reason, "s3cr3t")),
    ?assertEqual(nomatch, string:find(Reason, "secret")).
