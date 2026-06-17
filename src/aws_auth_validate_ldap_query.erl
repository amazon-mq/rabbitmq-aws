%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Self-contained parser for the rabbitmq_auth_backend_ldap authorization
%% query DSL.
%%
%% This intentionally mirrors the grammar accepted by
%% rabbit_auth_backend_ldap_util:parse_query/1 (erl_scan + erl_parse:parse_term
%% over the same accepted-term allowlist) so that a query the broker would
%% accept is accepted here and vice-versa (design requirement R12: the
%% validation endpoint must not diverge from rabbit_auth_backend_ldap). A
%% parity test in aws_auth_validate_tests guards against drift.
%%
%% Two deliberate differences from the upstream helper:
%%   1. It returns {ok, Query} | {error, Reason} instead of throwing via
%%      cuttlefish:invalid/2 -- this runs in the request path, not at
%%      config-load time.
%%   2. It adds literal_dns/1, which walks a parsed query and returns the
%%      DNs that are fully literal (contain no ${...} runtime placeholder).
%%      Those are the only DNs the endpoint can check for existence without
%%      a runtime principal, and they drive the static reachability check.
-module(aws_auth_validate_ldap_query).

-export([parse/1, literal_dns/1, is_literal/1]).

-export_type([query/0]).

-type query() :: tuple() | list().

%%--------------------------------------------------------------------
%% Parsing
%%--------------------------------------------------------------------

%% Parse one query expression. Accepts the same top-level term shapes as the
%% broker's parse_query/1. Returns the parsed Erlang term on success.
-spec parse(binary() | string()) -> {ok, query()} | {error, binary()}.
parse(Query) when is_binary(Query) ->
    parse(unicode:characters_to_list(Query, utf8));
parse(Query0) when is_list(Query0) ->
    case scan(fixup(Query0)) of
        {ok, Tokens} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> accept(Term);
                {error, _} -> {error, <<"query is not a valid expression">>}
            end;
        {error, _} ->
            {error, <<"query could not be tokenized">>}
    end;
parse(_) ->
    {error, <<"query must be a string">>}.

%% erl_parse:parse_term/1 needs a trailing dot.
fixup(Query0) ->
    Query1 = string:trim(Query0, both),
    case Query1 of
        "" ->
            ".";
        _ ->
            case lists:last(Query1) of
                $. -> Query1;
                _ -> Query1 ++ "."
            end
    end.

scan(Str) ->
    case erl_scan:string(Str) of
        {ok, Tokens, _EndLine} -> {ok, Tokens};
        {error, _Info, _Loc} -> {error, scan_failed}
    end.

%% Top-level allowlist -- mirrors rabbit_auth_backend_ldap_util:parse_query/1.
%% Anything not matched here is rejected rather than passed to the backend.
accept({constant, B} = T) when is_boolean(B) -> {ok, T};
accept({in_group, _} = T) -> {ok, T};
accept({in_group_nested, _, _} = T) -> {ok, T};
accept({for, Q} = T) when is_list(Q) -> {ok, T};
accept({'not', _} = T) -> {ok, T};
accept({'and', Q} = T) when is_list(Q) -> {ok, T};
accept({'or', Q} = T) when is_list(Q) -> {ok, T};
accept({equals, _, _} = T) -> {ok, T};
accept({match, _, _} = T) -> {ok, T};
%% tag_queries are expressed as a list of {Tag, SubQuery} pairs.
accept(T) when is_list(T) -> {ok, T};
accept(_) -> {error, <<"unrecognised query expression">>}.

%%--------------------------------------------------------------------
%% Literal DN extraction
%%--------------------------------------------------------------------

%% Walk a parsed query and collect the DNs that can be checked for existence
%% without a runtime principal: DN-bearing terms (exists, in_group,
%% in_group_nested, attribute) whose DN pattern contains no ${...}
%% placeholder. Returns a deduplicated list of DN strings.
-spec literal_dns(query()) -> [string()].
literal_dns(Query) ->
    lists:usort(collect(Query, [])).

collect({constant, _}, Acc) ->
    Acc;
collect({exists, DN}, Acc) ->
    maybe_dn(DN, Acc);
collect({in_group, DN}, Acc) ->
    maybe_dn(DN, Acc);
collect({in_group, DN, _Desc}, Acc) ->
    maybe_dn(DN, Acc);
collect({in_group_nested, DN}, Acc) ->
    maybe_dn(DN, Acc);
collect({in_group_nested, DN, _Desc}, Acc) ->
    maybe_dn(DN, Acc);
collect({in_group_nested, DN, _Desc, _Scope}, Acc) ->
    maybe_dn(DN, Acc);
collect({attribute, DN, _AttrName}, Acc) ->
    maybe_dn(DN, Acc);
collect({'not', SubQuery}, Acc) ->
    collect(SubQuery, Acc);
collect({'and', Queries}, Acc) when is_list(Queries) ->
    lists:foldl(fun collect/2, Acc, Queries);
collect({'or', Queries}, Acc) when is_list(Queries) ->
    lists:foldl(fun collect/2, Acc, Queries);
collect({for, Clauses}, Acc) when is_list(Clauses) ->
    lists:foldl(
        fun
            ({_Type, _Value, SubQuery}, A) -> collect(SubQuery, A);
            (_Other, A) -> A
        end,
        Acc,
        Clauses
    );
collect({equals, A1, A2}, Acc) ->
    collect(A2, collect(A1, Acc));
collect({match, A1, A2}, Acc) ->
    collect(A2, collect(A1, Acc));
%% A list operand is ONLY ever the tag_queries shape: a list of {Tag,
%% SubQuery} pairs. A bare string (a value/regex operand of equals/match, or
%% a top-level bare-string query) is also a flat list, but in the broker DSL
%% it is a literal VALUE, never a DN -- so it must contribute zero DNs. We
%% therefore recurse only into {_Tag, SubQuery} elements and ignore every
%% other list element, including bare character lists. Real literal DNs are
%% collected exclusively by the explicit DN-bearing clauses above (exists,
%% in_group, in_group_nested, attribute), which match before this clause.
collect(List, Acc) when is_list(List) ->
    lists:foldl(
        fun
            ({_Tag, SubQuery}, A) -> collect(SubQuery, A);
            (_Other, A) -> A
        end,
        Acc,
        List
    );
collect(_Other, Acc) ->
    Acc.

maybe_dn(DN, Acc) ->
    case is_literal(DN) of
        true -> [dn_to_list(DN) | Acc];
        false -> Acc
    end.

%% A DN pattern is "literal" when it is a concrete string with no ${...}
%% runtime placeholder. {string, Pattern} wrappers are unwrapped first.
-spec is_literal(term()) -> boolean().
is_literal({string, Pattern}) ->
    is_literal(Pattern);
is_literal(DN) when is_binary(DN) ->
    is_dn_string(binary_to_list(DN)) andalso not has_placeholder(binary_to_list(DN));
is_literal(DN) when is_list(DN) ->
    is_dn_string(DN) andalso not has_placeholder(DN);
is_literal(_) ->
    false.

dn_to_list({string, Pattern}) -> dn_to_list(Pattern);
dn_to_list(DN) when is_binary(DN) -> binary_to_list(DN);
dn_to_list(DN) when is_list(DN) -> DN.

%% True when the string contains a ${...} fill placeholder consumed at
%% runtime by the broker's fill/2 (e.g. ${username}, ${vhost}).
has_placeholder(Str) ->
    string:find(Str, "${") =/= nomatch.

%% A printable flat string (proper list of integers in a sane char range).
is_dn_string([]) ->
    false;
is_dn_string(L) when is_list(L) ->
    lists:all(fun(C) -> is_integer(C) andalso C >= 0 andalso C =< 16#10FFFF end, L);
is_dn_string(_) ->
    false.
