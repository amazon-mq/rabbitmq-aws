%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Parity tests between the HTTP validation backend and the real
%% rabbit_auth_backend_http that it validates.
%%
%% The validation backend does not reimplement an upstream parser (unlike the
%% LDAP query DSL), so there is no single equal-both-implementations check like
%% aws_auth_validate_tests:parity_test_/0. What it DOES do is build request
%% queries and interpret allow/deny responses the same way the real backend
%% does, mirroring that contract by hand. These tests pin the parts of that
%% contract that upstream exposes, so a change in rabbit_auth_backend_http that
%% would make the probe diverge from a real broker request is caught here rather
%% than silently.
%%
%% What is checked:
%%   * query encoding parity -- the probe's per-path query strings encode
%%     identically to rabbit_auth_backend_http:q/1 (the exported query builder
%%     the broker uses). This is a true differential test: both are run on the
%%     same params and asserted equal.
%%   * tag-join parity -- join_tags/1 is referenced by the authz query shape.
%%   * response-grammar contract -- our classify_response/2 accepts exactly the
%%     allow/deny forms the broker's own parsing accepts, and rejects the rest.
%%     The broker's parsing is inline in user_login_authentication/2 and req/2
%%     (not an exported pure function), so this side is asserted against the
%%     documented grammar rather than by calling upstream directly.
%%
%% Like the LDAP parity tests, the differential checks run only when upstream is
%% actually usable in this node (the module and its deps loaded); otherwise they
%% skip rather than fail spuriously across the RMQ-version CI matrix.
-module(aws_auth_validate_http_parity_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BACKEND, aws_auth_validate_http).
-define(UPSTREAM, rabbit_auth_backend_http).

%%--------------------------------------------------------------------
%% Query-encoding parity (differential: our query_for vs upstream q/1)
%%--------------------------------------------------------------------

%% For every probe path type, the query string our backend sends must encode
%% byte-for-byte the same as rabbit_auth_backend_http:q/1 given the same params.
%% If upstream ever changes its encoding (e.g. space as %20 instead of +, or a
%% different escaping of `/'), the probe would send params that a conformant
%% auth server parses differently and this fails.
query_encoding_parity_test_() ->
    case upstream_q_usable() of
        false ->
            [];
        true ->
            [
                {
                    "query_for(" ++ binary_to_list(Key) ++ ") == upstream q/1",
                    ?_assertEqual(
                        ?UPSTREAM:q(?BACKEND:params_for(Key)),
                        ?BACKEND:query_for(Key)
                    )
                }
             || Key <- path_keys()
            ]
    end.

%% Spot-check the encoding of individual special characters, independent of the
%% fixed probe params, so a regression is attributable to encoding rather than to
%% a params change. uri_string:compose_query/1 is the encoder query_for/1
%% delegates to, so this compares the two encoders directly.
%%
%% Two groups, because the encoders agree byte-for-byte on most characters but
%% not all:
%%   * AGREE -- space (both `+'), `/', `#', and reserved `&'/`=' encode
%%     identically, so byte equality holds and pins that no regression appears.
%%   * DIVERGE-BUT-SAFE -- `~' (upstream leaves literal, compose_query emits
%%     %7E) and `*' (upstream emits %2A, compose_query leaves literal) differ in
%%     bytes but decode to the same character, so a conformant auth server that
%%     percent-decodes reads the same param either way. Asserting byte equality
%%     here would be wrong; assert post-decode (semantic) equivalence instead,
%%     which is the property that actually matters on the wire.
query_encoding_special_chars_test_() ->
    case upstream_q_usable() of
        false ->
            [];
        true ->
            Agree = [
                [{"username", "a b"}],
                [{"name", "a/b"}],
                [{"vhost", "/"}],
                [{"routing_key", "#"}],
                [{"k", "a&b=c"}]
            ],
            DivergeButSafe = [
                [{"k", "tilde~x"}],
                [{"k", "star*x"}]
            ],
            [
                {
                    "byte parity: " ++ lists:flatten(io_lib:format("~p", [P])),
                    ?_assertEqual(?UPSTREAM:q(P), uri_string:compose_query(P))
                }
             || P <- Agree
            ] ++
                [
                    {
                        "decode parity: " ++ lists:flatten(io_lib:format("~p", [P])),
                        ?_assertEqual(
                            uri_string:dissect_query(?UPSTREAM:q(P)),
                            uri_string:dissect_query(uri_string:compose_query(P))
                        )
                    }
                 || P <- DivergeButSafe
                ]
    end.

%%--------------------------------------------------------------------
%% Credentialed-query encoding parity (non-ASCII byte sequences)
%%--------------------------------------------------------------------

%% The credentialed query must encode non-ASCII bytes identically to
%% rabbit_http_util:quote_plus/1 (the encoder the real backend uses). A prior
%% version used uri_string:compose_query which treats each byte from
%% binary_to_list as a Unicode codepoint, re-encoding to UTF-8 and producing
%% double-encoded sequences for multi-byte chars (e.g. 0xC3 0xA9 -> %C3%83%C2%A9
%% instead of %C3%A9). This test pins the byte-parity for a non-ASCII value.
credentialed_query_nonascii_parity_test_() ->
    case upstream_q_usable() of
        false ->
            [];
        true ->
            %% "cafe" with accent on e: UTF-8 bytes 0x63 0x61 0x66 0xC3 0xA9
            NonAscii = <<"caf", 16#C3, 16#A9>>,
            Expected = rabbit_http_util:quote_plus(NonAscii),
            Params = #{username => NonAscii, password => <<"pw">>},
            Query = aws_auth_validate_http:credentialed_query_for(Params),
            [
                {"non-ASCII username encoding matches rabbit_http_util:quote_plus",
                    ?_assertNotEqual(nomatch, string:find(Query, "username=" ++ Expected))},
                {"non-ASCII password encoding matches rabbit_http_util:quote_plus", fun() ->
                    PwNonAscii = <<"p", 16#C3, 16#A4, "ss">>,
                    PwExpected = rabbit_http_util:quote_plus(PwNonAscii),
                    PwParams = #{username => <<"u">>, password => PwNonAscii},
                    PwQuery = aws_auth_validate_http:credentialed_query_for(PwParams),
                    ?assertNotEqual(nomatch, string:find(PwQuery, "password=" ++ PwExpected))
                end}
            ]
    end.

%%--------------------------------------------------------------------
%% Tag-join parity (join_tags/1 is exported upstream)
%%--------------------------------------------------------------------

join_tags_parity_test_() ->
    case upstream_join_tags_usable() of
        false ->
            [];
        true ->
            [
                ?_assertEqual("", ?UPSTREAM:join_tags([])),
                ?_assertEqual("administrator", ?UPSTREAM:join_tags([administrator])),
                ?_assertEqual(
                    "administrator management",
                    ?UPSTREAM:join_tags([administrator, management])
                )
            ]
    end.

%%--------------------------------------------------------------------
%% Response-grammar contract (our classify_response vs the documented grammar)
%%--------------------------------------------------------------------
%%
%% The broker's allow/deny parsing is inline in user_login_authentication/2
%% (authn: a raw `"deny " ++ Reason', then bare `deny', or `allow' +
%% space-separated tags) and req/2 (authz: a raw `"deny " ++ Reason', then bare
%% `allow' / `deny'), both after lowercasing the trimmed body. Those are not
%% exported pure functions, so this pins our classify_response/2 against that
%% grammar directly. If someone loosens or tightens our parser, this flags it
%% against the contract we are mirroring.
%%
%% LIMITATION: unlike the query-encoding tests above, this side is NOT
%% differential. The Ok/Bad corpora below are a hand-written restatement of the
%% upstream grammar, not a call into upstream, so an upstream change to the
%% allow/deny parsing does NOT fail these tests. When bumping the RabbitMQ
%% dependency, re-read user_login_authentication/2 and req/2 and re-sync these
%% corpora by hand. A durable fix would require upstream to export a pure
%% response-parsing predicate to assert against directly.

%% authn (user_path): `deny'[ reason...] or `allow'[ tags...] succeed; anything
%% else fails.
response_grammar_authn_test_() ->
    Ok = [
        <<"allow">>,
        <<"allow administrator">>,
        <<"allow a b c">>,
        <<"deny">>,
        %% a deny may carry a reason, mirroring the broker's `"deny " ++ Reason'
        <<"deny insufficient permissions">>,
        %% trimming + case-insensitivity, as the broker lowercases the trimmed body
        <<"  ALLOW  ">>,
        <<"Deny">>,
        <<"DENY too bad">>
    ],
    Bad = [<<"allowed">>, <<"allowxyz">>, <<"denied">>, <<"maybe">>, <<>>, <<"allow-tag">>],
    [?_assertEqual(ok, ?BACKEND:classify_response(<<"user_path">>, B)) || B <- Ok] ++
        [
            ?_assertMatch({error, auth_failed, _}, ?BACKEND:classify_response(<<"user_path">>, B))
         || B <- Bad
        ].

%% authz (vhost/resource/topic_path): bare `allow', or `deny'[ reason...],
%% succeed; the allow-with-tags form is authn-only and must NOT be accepted here.
response_grammar_authz_test_() ->
    Keys = [<<"vhost_path">>, <<"resource_path">>, <<"topic_path">>],
    Ok = [<<"allow">>, <<"deny">>, <<"deny not allowed here">>, <<"  ALLOW ">>],
    Bad = [<<"allow administrator">>, <<"allowed">>, <<"maybe">>, <<>>],
    [?_assertEqual(ok, ?BACKEND:classify_response(K, B)) || K <- Keys, B <- Ok] ++
        [
            ?_assertMatch({error, auth_failed, _}, ?BACKEND:classify_response(K, B))
         || K <- Keys, B <- Bad
        ].

%%--------------------------------------------------------------------
%% Corpus + upstream-usable guards
%%--------------------------------------------------------------------

path_keys() ->
    [<<"user_path">>, <<"vhost_path">>, <<"resource_path">>, <<"topic_path">>].

upstream_q_usable() ->
    upstream_callable(q, [{"k", "a b"}]).

upstream_join_tags_usable() ->
    upstream_callable(join_tags, [a, b]).

%% True when ?UPSTREAM:Fun can be called with SampleArgs and returns a string.
%% rabbit_auth_backend_http:q/1 depends on rabbit_http_util:quote_plus and
%% rabbit_data_coercion, which across the RMQ-version CI matrix are not always
%% loaded in the bare eunit node; if a dep is missing the function throws undef
%% and the parity check would fail spuriously, so skip it then.
%%
%% The probe checks CALLABILITY (it runs and returns a string), not a specific
%% output: pinning the expected value would make an upstream behaviour change
%% flip this guard to false and skip the parity test, silently disabling the one
%% check that exists to catch that drift. With only a callability guard, such
%% drift fails the differential assertion instead.
upstream_callable(Fun, SampleArgs) ->
    code:ensure_loaded(?UPSTREAM) =/= {error, nofile} andalso
        erlang:function_exported(?UPSTREAM, Fun, 1) andalso
        try
            is_list(?UPSTREAM:Fun(SampleArgs))
        catch
            _:_ -> false
        end.
