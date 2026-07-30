%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Parity tests between aws_auth_validate_tls:validate_chain/3 and a REAL
%% ssl:handshake mTLS handshake.
%%
%% The reviewer's criticism: "the suite tests this module against public_key
%% while the thing being predicted is ssl's path building, and those disagree
%% in all the ways above." These tests stand up a real TLS listener and
%% connect a client with the same certificate material, asserting the
%% endpoint's verdict AGREES with the actual handshake outcome.
%%
%% Three scenarios from the reviewer's findings:
%%   1. Intermediate-only CA bundle -- ssl rejects (unknown_ca), endpoint must too.
%%   2. CA key rollover -- two roots sharing a subject DN, both bundle orders accept.
%%   3. Unordered chain tail -- leaf-first with scrambled tail passes; root-first
%%      chain (leaf not first) is rejected by both sides.
%%
%% No broker required -- only the ssl application (already in LOCAL_DEPS).
%% No meck -- purely openssl-generated fixtures + real handshakes.
-module(aws_auth_validate_tls_parity_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Guard: skip the entire module if openssl is unusable or ssl won't start
%%====================================================================

can_run_parity_tests() ->
    case os:find_executable("openssl") of
        false -> false;
        _ -> ensure_ssl_started()
    end.

ensure_ssl_started() ->
    case application:ensure_all_started(ssl) of
        {ok, _} -> true;
        {error, _} -> false
    end.

%%====================================================================
%% Top-level test generator -- skips cleanly if preconditions unmet
%%====================================================================

parity_test_() ->
    case can_run_parity_tests() of
        false ->
            [];
        true ->
            {setup, fun setup/0, fun cleanup/1, fun(Fixtures) ->
                [
                    {"intermediate_only_leaf_alone_both_reject", fun() ->
                        intermediate_only_leaf_alone_both_reject(Fixtures)
                    end},
                    {"intermediate_only_leaf_plus_int_both_reject", fun() ->
                        intermediate_only_leaf_plus_int_both_reject(Fixtures)
                    end},
                    {"intermediate_only_control_root_bundle_leaf_plus_int_both_accept", fun() ->
                        intermediate_only_control_root_bundle_leaf_plus_int_accept(Fixtures)
                    end},
                    {"root_int_bundle_leaf_alone_both_accept", fun() ->
                        root_int_bundle_leaf_alone_both_accept(Fixtures)
                    end},
                    {"key_rollover_old_new_order_both_accept", fun() ->
                        key_rollover_old_new_order_both_accept(Fixtures)
                    end},
                    {"key_rollover_new_old_order_both_accept", fun() ->
                        key_rollover_new_old_order_both_accept(Fixtures)
                    end},
                    {"unordered_tail_leaf_root_int_both_accept", fun() ->
                        unordered_tail_leaf_root_int_both_accept(Fixtures)
                    end},
                    {"root_first_chain_both_reject", fun() ->
                        root_first_chain_both_reject(Fixtures)
                    end}
                ]
            end}
    end.

%%====================================================================
%% Fixture generation (once per test group)
%%====================================================================

setup() ->
    Dir = tmp_dir(),
    %% Hierarchy for findings 1 and 7: Root -> Intermediate -> Leaf
    {RootKey, RootCert, RootDer} = gen_self_signed_ca(Dir, "parity-root"),
    {IntKey, IntCert, IntDer} = gen_intermediate_ca(Dir, "parity-int", RootKey, RootCert),
    {LeafKey, LeafCert, LeafDer} = gen_leaf(Dir, "parity-leaf", IntKey, IntCert),
    %% For finding 2: two roots sharing the same subject DN with different keys
    CommonCN = "ParityRolloverRoot",
    {_OldRootKey, _OldRootCert, OldRootDer} = gen_self_signed_ca_cn(
        Dir, "parity-old-root", CommonCN
    ),
    {NewRootKey, NewRootCert, NewRootDer} = gen_self_signed_ca_cn(Dir, "parity-new-root", CommonCN),
    {RolloverLeafKey, RolloverLeafCert, RolloverLeafDer} =
        gen_leaf(Dir, "parity-rollover-leaf", NewRootKey, NewRootCert),
    %% Server cert (signed by root, for the TLS listener -- does not matter which
    %% CA signs it as long as the client can complete its side of the handshake; we
    %% skip client-side server verification for simplicity).
    {ServerKey, ServerCert, _ServerDer} = gen_leaf(Dir, "parity-server", RootKey, RootCert),
    #{
        root_der => RootDer,
        int_der => IntDer,
        leaf_der => LeafDer,
        leaf_key => LeafKey,
        leaf_cert => LeafCert,
        old_root_der => OldRootDer,
        new_root_der => NewRootDer,
        rollover_leaf_der => RolloverLeafDer,
        rollover_leaf_key => RolloverLeafKey,
        rollover_leaf_cert => RolloverLeafCert,
        server_key => ServerKey,
        server_cert => ServerCert,
        root_cert => RootCert,
        int_cert => IntCert
    }.

cleanup(_Fixtures) ->
    ok.

%%====================================================================
%% Parity test cases
%%====================================================================

%% Finding 1: Intermediate-only bundle, client presents [leaf].
%% ssl rejects with unknown_ca; endpoint must also reject.
intermediate_only_leaf_alone_both_reject(#{
    int_der := IntDer,
    leaf_der := LeafDer,
    leaf_key := LeafKey,
    leaf_cert := LeafCert,
    server_key := ServerKey,
    server_cert := ServerCert,
    int_cert := IntCert
}) ->
    EndpointResult = aws_auth_validate_tls:validate_chain([LeafDer], [IntDer], undefined),
    SslResult = do_handshake(
        _CaCerts = [IntCert],
        _ClientCertFile = LeafCert,
        _ClientKeyFile = LeafKey,
        ServerCert,
        ServerKey
    ),
    assert_parity(EndpointResult, SslResult, "intermediate_only_leaf_alone").

%% Finding 1: Intermediate-only bundle, client presents [leaf, int].
%% ssl still rejects -- partial_chain only accepts self-signed roots.
intermediate_only_leaf_plus_int_both_reject(#{
    int_der := IntDer,
    leaf_der := LeafDer,
    leaf_key := LeafKey,
    leaf_cert := LeafCert,
    server_key := ServerKey,
    server_cert := ServerCert,
    int_cert := IntCert
}) ->
    EndpointResult = aws_auth_validate_tls:validate_chain([LeafDer, IntDer], [IntDer], undefined),
    %% Client presents its full chain (leaf + intermediate)
    ChainFile = write_chain_file([LeafCert, IntCert]),
    SslResult = do_handshake(
        _CaCerts = [IntCert],
        _ClientCertFile = ChainFile,
        _ClientKeyFile = LeafKey,
        ServerCert,
        ServerKey
    ),
    assert_parity(EndpointResult, SslResult, "intermediate_only_leaf_plus_int").

%% Control for Finding 1: bundle=[Root], client=[leaf, int] -- both accept.
intermediate_only_control_root_bundle_leaf_plus_int_accept(#{
    root_der := RootDer,
    int_der := IntDer,
    leaf_der := LeafDer,
    leaf_key := LeafKey,
    leaf_cert := LeafCert,
    server_key := ServerKey,
    server_cert := ServerCert,
    root_cert := RootCert,
    int_cert := IntCert
}) ->
    EndpointResult = aws_auth_validate_tls:validate_chain([LeafDer, IntDer], [RootDer], undefined),
    ChainFile = write_chain_file([LeafCert, IntCert]),
    SslResult = do_handshake(
        _CaCerts = [RootCert],
        _ClientCertFile = ChainFile,
        _ClientKeyFile = LeafKey,
        ServerCert,
        ServerKey
    ),
    assert_parity(EndpointResult, SslResult, "control_root_bundle_leaf_plus_int").

%% Control for Finding 1: bundle=[Root, Int], client=[leaf] -- both accept.
%%
%% This is the reviewer's fourth measured control ("bundle=[Root, Int], client
%% sends [leaf] -> handshake ok"). A listener treats every cacerts entry as
%% chain-building material, not only as a trust anchor, so it discovers the
%% intermediate in the bundle and completes the leaf-only client. Requiring a
%% self-signed anchor (the Finding 1 fix) turned this into a false FAIL until
%% validate_chain/3 also extended the chain from the bundle. Keep this pinned:
%% it is the case that distinguishes "reject non-self-signed ANCHORS" from the
%% overreach of "ignore non-self-signed bundle entries entirely".
root_int_bundle_leaf_alone_both_accept(#{
    root_der := RootDer,
    int_der := IntDer,
    leaf_der := LeafDer,
    leaf_key := LeafKey,
    leaf_cert := LeafCert,
    server_key := ServerKey,
    server_cert := ServerCert,
    root_cert := RootCert,
    int_cert := IntCert
}) ->
    EndpointResult = aws_auth_validate_tls:validate_chain([LeafDer], [RootDer, IntDer], undefined),
    ChainFile = write_chain_file([LeafCert]),
    SslResult = do_handshake(
        _CaCerts = [RootCert, IntCert],
        _ClientCertFile = ChainFile,
        _ClientKeyFile = LeafKey,
        ServerCert,
        ServerKey
    ),
    assert_parity(EndpointResult, SslResult, "root_int_bundle_leaf_alone").

%% Finding 2: Key rollover, bundle order [OldRoot, NewRoot], leaf chains to NewRoot.
key_rollover_old_new_order_both_accept(#{
    old_root_der := OldRootDer,
    new_root_der := NewRootDer,
    rollover_leaf_der := RolloverLeafDer,
    rollover_leaf_key := RolloverLeafKey,
    rollover_leaf_cert := RolloverLeafCert,
    server_key := ServerKey,
    server_cert := ServerCert
}) ->
    EndpointResult = aws_auth_validate_tls:validate_chain(
        [RolloverLeafDer], [OldRootDer, NewRootDer], undefined
    ),
    %% For ssl, provide both root certs in the cacerts list (order: old, new).
    OldRootPem = der_to_pem(OldRootDer),
    NewRootPem = der_to_pem(NewRootDer),
    CaBundleFile = write_chain_file([OldRootPem, NewRootPem]),
    SslResult = do_handshake(
        _CaCerts = [CaBundleFile],
        _ClientCertFile = RolloverLeafCert,
        _ClientKeyFile = RolloverLeafKey,
        ServerCert,
        ServerKey
    ),
    assert_parity(EndpointResult, SslResult, "key_rollover_old_new_order").

%% Finding 2: Key rollover, bundle order [NewRoot, OldRoot].
key_rollover_new_old_order_both_accept(#{
    old_root_der := OldRootDer,
    new_root_der := NewRootDer,
    rollover_leaf_der := RolloverLeafDer,
    rollover_leaf_key := RolloverLeafKey,
    rollover_leaf_cert := RolloverLeafCert,
    server_key := ServerKey,
    server_cert := ServerCert
}) ->
    EndpointResult = aws_auth_validate_tls:validate_chain(
        [RolloverLeafDer], [NewRootDer, OldRootDer], undefined
    ),
    NewRootPem = der_to_pem(NewRootDer),
    OldRootPem = der_to_pem(OldRootDer),
    CaBundleFile = write_chain_file([NewRootPem, OldRootPem]),
    SslResult = do_handshake(
        _CaCerts = [CaBundleFile],
        _ClientCertFile = RolloverLeafCert,
        _ClientKeyFile = RolloverLeafKey,
        ServerCert,
        ServerKey
    ),
    assert_parity(EndpointResult, SslResult, "key_rollover_new_old_order").

%% Finding 7: Unordered tail -- client PEM [leaf, root, int] (leaf first, tail
%% arbitrarily ordered). Bundle=[Root]. ssl rebuilds the chain and accepts.
unordered_tail_leaf_root_int_both_accept(#{
    root_der := RootDer,
    int_der := IntDer,
    leaf_der := LeafDer,
    leaf_key := LeafKey,
    leaf_cert := LeafCert,
    server_key := ServerKey,
    server_cert := ServerCert,
    root_cert := RootCert,
    int_cert := IntCert
}) ->
    %% Endpoint: pass DERs in unordered-tail order [leaf, root, int]
    EndpointResult = aws_auth_validate_tls:validate_chain(
        [LeafDer, RootDer, IntDer], [RootDer], undefined
    ),
    %% ssl: client chain file in PEM order [leaf, root, int]
    ChainFile = write_chain_file([LeafCert, RootCert, IntCert]),
    SslResult = do_handshake(
        _CaCerts = [RootCert],
        _ClientCertFile = ChainFile,
        _ClientKeyFile = LeafKey,
        ServerCert,
        ServerKey
    ),
    assert_parity(EndpointResult, SslResult, "unordered_tail_leaf_root_int").

%% Finding 7 narrowing: A fully root-first chain [root, int, leaf] is rejected
%% by a real listener (bad_certificate) because the leaf is not the peer cert.
%% Both sides must reject.
root_first_chain_both_reject(#{
    root_der := RootDer,
    int_der := IntDer,
    leaf_der := LeafDer,
    leaf_key := LeafKey,
    leaf_cert := LeafCert,
    server_key := ServerKey,
    server_cert := ServerCert,
    root_cert := RootCert,
    int_cert := IntCert
}) ->
    %% Endpoint: pass DERs in root-first order [root, int, leaf]
    EndpointResult = aws_auth_validate_tls:validate_chain(
        [RootDer, IntDer, LeafDer], [RootDer], undefined
    ),
    %% ssl: client chain file in PEM order [root, int, leaf] -- the server sees
    %% "root" as the peer cert, which is not the leaf.
    ChainFile = write_chain_file([RootCert, IntCert, LeafCert]),
    SslResult = do_handshake(
        _CaCerts = [RootCert],
        _ClientCertFile = ChainFile,
        _ClientKeyFile = LeafKey,
        ServerCert,
        ServerKey
    ),
    assert_parity(EndpointResult, SslResult, "root_first_chain").

%%====================================================================
%% Real TLS handshake helper
%%====================================================================

%% Perform a real mTLS handshake and return `accept' or `reject'.
%% CaCerts: list of PEM file paths that the SERVER trusts (client CA bundle).
%% ClientCertFile: PEM file the client presents (may be a chain).
%% ClientKeyFile: PEM file with the client's private key.
%% ServerCertFile, ServerKeyFile: server identity (any valid cert/key pair).
%%
%% Returns `accept' if the server-side handshake succeeds, `reject' otherwise.
do_handshake(CaCerts, ClientCertFile, ClientKeyFile, ServerCertFile, ServerKeyFile) ->
    %% Build server cacerts: decode all CA PEM files to DER for the cacerts option
    ServerCaDers = lists:flatmap(
        fun(Path) when is_list(Path); is_binary(Path) ->
            {ok, Pem} = file:read_file(Path),
            [Der || {'Certificate', Der, not_encrypted} <- public_key:pem_decode(Pem)]
        end,
        CaCerts
    ),
    ServerOpts = [
        {certfile, to_list(ServerCertFile)},
        {keyfile, to_list(ServerKeyFile)},
        {cacerts, ServerCaDers},
        {verify, verify_peer},
        {fail_if_no_peer_cert, true},
        {reuseaddr, true}
    ],
    %% Listen on a random port on loopback
    {ok, LSock} = ssl:listen(0, [{ip, {127, 0, 0, 1}} | ServerOpts]),
    {ok, {_, Port}} = ssl:sockname(LSock),
    %% Spawn the server acceptor
    Parent = self(),
    Ref = make_ref(),
    _Pid = spawn_link(fun() ->
        Result =
            case ssl:transport_accept(LSock, 5000) of
                {ok, TlsSock} ->
                    case ssl:handshake(TlsSock, 5000) of
                        {ok, SSock} ->
                            ssl:close(SSock),
                            accept;
                        {error, _} ->
                            reject
                    end;
                {error, _} ->
                    reject
            end,
        Parent ! {Ref, Result}
    end),
    %% Client connects
    ClientOpts = [
        {certfile, to_list(ClientCertFile)},
        {keyfile, to_list(ClientKeyFile)},
        {verify, verify_none}
    ],
    _ClientResult =
        case ssl:connect({127, 0, 0, 1}, Port, ClientOpts, 5000) of
            {ok, CSock} ->
                ssl:close(CSock),
                ok;
            {error, _} ->
                ok
        end,
    %% Collect server-side result (the server validates the client cert)
    ServerResult =
        receive
            {Ref, R} -> R
        after 6000 ->
            reject
        end,
    ssl:close(LSock),
    ServerResult.

%%====================================================================
%% Parity assertion
%%====================================================================

%% Assert that the endpoint and ssl agree on accept/reject.
assert_parity(EndpointResult, SslResult, Label) ->
    EndpointVerdict = normalize_verdict(EndpointResult),
    ?assertEqual(
        SslResult,
        EndpointVerdict,
        lists:flatten(
            io_lib:format(
                "Parity failure in ~s: ssl=~p, endpoint=~p (raw: ~p)",
                [Label, SslResult, EndpointVerdict, EndpointResult]
            )
        )
    ).

normalize_verdict(ok) -> accept;
normalize_verdict({error, _, _}) -> reject.

%%====================================================================
%% Certificate generation helpers
%%====================================================================

gen_self_signed_ca(Dir, Name) ->
    gen_self_signed_ca_cn(Dir, Name, "Parity" ++ Name ++ "CA").

gen_self_signed_ca_cn(Dir, Name, CN) ->
    Suffix = integer_to_list(erlang:unique_integer([positive])),
    KeyFile = filename:join(Dir, Name ++ "-key-" ++ Suffix ++ ".pem"),
    CertFile = filename:join(Dir, Name ++ "-cert-" ++ Suffix ++ ".pem"),
    Cmd = lists:flatten(
        io_lib:format(
            "openssl req -x509 -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
            "-days 2 -subj '/CN=~s' 2>/dev/null",
            [KeyFile, CertFile, CN]
        )
    ),
    _ = os:cmd(Cmd),
    true = filelib:is_regular(CertFile),
    {ok, Pem} = file:read_file(CertFile),
    [{'Certificate', Der, not_encrypted}] = public_key:pem_decode(Pem),
    {KeyFile, CertFile, Der}.

gen_intermediate_ca(Dir, Name, CaKeyFile, CaCertFile) ->
    Suffix = integer_to_list(erlang:unique_integer([positive])),
    KeyFile = filename:join(Dir, Name ++ "-key-" ++ Suffix ++ ".pem"),
    CsrFile = filename:join(Dir, Name ++ "-csr-" ++ Suffix ++ ".pem"),
    CertFile = filename:join(Dir, Name ++ "-cert-" ++ Suffix ++ ".pem"),
    ExtFile = filename:join(Dir, Name ++ "-ext-" ++ Suffix ++ ".cnf"),
    ok = file:write_file(
        ExtFile,
        "basicConstraints=critical,CA:TRUE,pathlen:0\n"
        "keyUsage=critical,keyCertSign,cRLSign\n"
    ),
    Sh = fun(Fmt, Args) -> os:cmd(lists:flatten(io_lib:format(Fmt, Args))) end,
    _ = Sh(
        "openssl req -new -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
        "-subj '/CN=Parity~sIntCA' 2>/dev/null",
        [KeyFile, CsrFile, Name]
    ),
    _ = Sh(
        "openssl x509 -req -in ~ts -CA ~ts -CAkey ~ts -CAcreateserial "
        "-out ~ts -days 2 -extfile ~ts 2>/dev/null",
        [CsrFile, CaCertFile, CaKeyFile, CertFile, ExtFile]
    ),
    true = filelib:is_regular(CertFile),
    {ok, Pem} = file:read_file(CertFile),
    [{'Certificate', Der, not_encrypted}] = public_key:pem_decode(Pem),
    {KeyFile, CertFile, Der}.

gen_leaf(Dir, Name, CaKeyFile, CaCertFile) ->
    Suffix = integer_to_list(erlang:unique_integer([positive])),
    KeyFile = filename:join(Dir, Name ++ "-key-" ++ Suffix ++ ".pem"),
    CsrFile = filename:join(Dir, Name ++ "-csr-" ++ Suffix ++ ".pem"),
    CertFile = filename:join(Dir, Name ++ "-cert-" ++ Suffix ++ ".pem"),
    Sh = fun(Fmt, Args) -> os:cmd(lists:flatten(io_lib:format(Fmt, Args))) end,
    _ = Sh(
        "openssl req -new -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
        "-subj '/CN=Parity~sLeaf' 2>/dev/null",
        [KeyFile, CsrFile, Name]
    ),
    _ = Sh(
        "openssl x509 -req -in ~ts -CA ~ts -CAkey ~ts -CAcreateserial "
        "-out ~ts -days 2 2>/dev/null",
        [CsrFile, CaCertFile, CaKeyFile, CertFile]
    ),
    true = filelib:is_regular(CertFile),
    {ok, Pem} = file:read_file(CertFile),
    [{'Certificate', Der, not_encrypted}] = public_key:pem_decode(Pem),
    {KeyFile, CertFile, Der}.

%%====================================================================
%% Utility helpers
%%====================================================================

tmp_dir() ->
    Base = filename:join(["/tmp", "aws_auth_validate_tls_parity_tests"]),
    ok = filelib:ensure_dir(filename:join(Base, "x")),
    Base.

%% Write multiple PEM file paths/binaries into a single chain PEM file.
%% Accepts either file paths (read and concatenated) or raw PEM binaries.
write_chain_file(Items) ->
    Dir = tmp_dir(),
    Suffix = integer_to_list(erlang:unique_integer([positive])),
    ChainFile = filename:join(Dir, "chain-" ++ Suffix ++ ".pem"),
    Content = iolist_to_binary(
        lists:map(
            fun
                (Path) when is_list(Path) ->
                    {ok, Bin} = file:read_file(Path),
                    Bin;
                (Bin) when is_binary(Bin) ->
                    Bin
            end,
            Items
        )
    ),
    ok = file:write_file(ChainFile, Content),
    ChainFile.

%% Convert a DER binary back to PEM format.
der_to_pem(Der) ->
    public_key:pem_encode([{'Certificate', Der, not_encrypted}]).

to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(L) when is_list(L) -> L.
