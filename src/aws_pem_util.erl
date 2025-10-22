%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_pem_util).

-export([decode_data/1, decode_key_data/1]).

decode_data(PemList) when is_list(PemList) ->
    decode_data(list_to_binary(PemList));
decode_data(PemBin) when is_binary(PemBin) ->
    try
        CertsDerEncoded = [
            Der
         || {KeyType, Der, Encrypted} <- public_key:pem_decode(PemBin),
            KeyType =:= 'Certificate',
            Encrypted =:= 'not_encrypted'
        ],
        case CertsDerEncoded of
            Certs when is_list(Certs) andalso length(Certs) > 0 ->
                {ok, CertsDerEncoded};
            _ ->
                {error, {invalid_pem_data, "invalid PEM data: no valid certificates found"}}
        end
    catch
        Class:Error ->
            {error, {error_decoding_certs, {Class, Error}}}
    end.

-spec decode_key_data(iodata()) -> {ok, binary()} | {error, term()}.
decode_key_data(PemList) when is_list(PemList) ->
    decode_key_data(list_to_binary(PemList));
decode_key_data(PemBin) when is_binary(PemBin) ->
    try
        KeyInfos = [
            {KeyType, DerEncoded}
         || {KeyType, DerEncoded, Encryption} <- public_key:pem_decode(PemBin),
            lists:member(KeyType, [
                'RSAPrivateKey', 'DSAPrivateKey', 'ECPrivateKey', 'PrivateKeyInfo'
            ]),
            Encryption =:= 'not_encrypted'
        ],
        case KeyInfos of
            [KeyInfo] ->
                {ok, KeyInfo};
            [KeyInfo | _] ->
                {ok, KeyInfo};
            [] ->
                {error, {invalid_pem_key_data, "invalid PEM data: no valid private keys found"}}
        end
    catch
        Class:Error ->
            {error, {error_decoding_key, {Class, Error}}}
    end.
