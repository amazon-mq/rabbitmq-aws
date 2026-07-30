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
%% accept is accepted here and vice-versa -- the endpoint's parse decision must
%% match the live broker's for the same input. A parity test in
%% aws_auth_validate_tests guards against drift.
%%
%% Three deliberate differences from the upstream helper:
%%   1. It returns {ok, Query} | {error, Reason} instead of throwing via
%%      cuttlefish:invalid/2 -- this runs in the request path, not at
%%      config-load time.
%%   2. It adds literal_dns/1, which walks a parsed query and returns the
%%      DNs that are fully literal (contain no ${...} runtime placeholder).
%%      Those are the only DNs the endpoint can check for existence without
%%      a runtime principal, and they drive the static reachability check.
%%   3. It adds fill/2 (and ad_args/1, is_evaluable/1), which substitute a
%%      request-supplied principal into ${...} placeholders so the backend can
%%      *evaluate* membership queries, not just reachability-check static DNs.
%%      fill/2 mirrors rabbit_auth_backend_ldap_util:fill/2 (raw substitution
%%      for value sinks) and rabbit_ldap_rfc4514:fill_dn/2 (RFC 4514 escaping
%%      for DN sinks, exempting the user_dn key). It is re-implemented here
%%      rather than called directly because rabbitmq_auth_backend_ldap is only
%%      a TEST_DEP, not a runtime dependency (same rationale as parse/1); a
%%      parity test guards against drift.
-module(aws_auth_validate_ldap_query).

-export([parse/1, literal_dns/1, is_literal/1, fill/2, is_evaluable/1, ad_args/1]).

-export_type([query/0]).

-type query() :: tuple() | list().

%%--------------------------------------------------------------------
%% Parsing
%%--------------------------------------------------------------------

%% Parse one query expression. Accepts the same top-level term shapes as the
%% broker's parse_query/1. Returns the parsed Erlang term on success.
%%
%% Tokenization goes through aws_auth_validate_ldap_query_lexer (a safe,
%% non-interning lexer), NOT erl_scan:string/1. The query string is
%% attacker-controlled, and erl_scan interns an atom per distinct identifier;
%% the safe lexer resolves atoms with list_to_existing_atom/1 instead, so a
%% query referencing an atom the broker has never interned is rejected rather
%% than permanently growing the atom table. See the lexer module for detail.
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
    %% Safe, non-interning tokenizer: an atom that does not already exist makes
    %% the lexer return {error, ...} rather than minting it (see the lexer
    %% module). The token shape is identical to erl_scan's, so the downstream
    %% erl_parse:parse_term/1 is unchanged.
    case aws_auth_validate_ldap_query_lexer:string(Str) of
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

%% True when a DN pattern is *evaluable*: a concrete string that, after fill/2
%% has substituted the principal placeholders, no longer contains any ${...}
%% placeholder. Distinct from is_literal/1 only in intent -- is_literal/1 gates
%% the no-principal reachability path (a DN that was literal to begin with);
%% is_evaluable/1 gates the post-fill evaluation path (a DN that became concrete
%% once the username/user_dn were filled in). A residual placeholder means the
%% term keyed on per-operation context (${vhost}/${resource}/...) we cannot
%% supply, so the backend skips (degrades) rather than failing it.
-spec is_evaluable(term()) -> boolean().
is_evaluable(DN) ->
    is_literal(DN).

%%--------------------------------------------------------------------
%% Placeholder filling (principal substitution)
%%--------------------------------------------------------------------

%% Substitute the principal placeholders in a parsed query, returning a query
%% term of the SAME shape with its DN/value sinks filled. Mirrors the broker's
%% two-mode fill: DN-bearing operands (exists/in_group/in_group_nested/
%% attribute) are RFC 4514-escaped per Args value (except the user_dn key,
%% which already holds a complete DN), while value operands of equals/match and
%% bare/{string,_} terms are filled raw (they are ACL value sinks, not DNs).
%%
%% Args is the principal context the endpoint can supply without a live AMQP
%% operation: [{username, U}, {user_dn, D} | ad_args(U)]. Keys absent from a
%% template are left untouched; values that cannot be stringified are dropped to
%% "" (to_repl parity). Placeholders keyed on per-operation context
%% (${vhost}/${resource}/${name}/${permission}) are simply not in Args, so they
%% survive unfilled and the DN stays non-evaluable (the backend degrades it).
-spec fill(query(), [{atom(), term()}]) -> query().
fill({constant, _} = T, _Args) ->
    T;
fill({exists, DN}, Args) ->
    {exists, fill_dn(DN, Args)};
fill({in_group, DN}, Args) ->
    {in_group, fill_dn(DN, Args)};
fill({in_group, DN, Desc}, Args) ->
    {in_group, fill_dn(DN, Args), Desc};
fill({in_group_nested, DN}, Args) ->
    {in_group_nested, fill_dn(DN, Args)};
fill({in_group_nested, DN, Desc}, Args) ->
    {in_group_nested, fill_dn(DN, Args), Desc};
fill({in_group_nested, DN, Desc, Scope}, Args) ->
    {in_group_nested, fill_dn(DN, Args), Desc, Scope};
fill({attribute, DN, AttrName}, Args) ->
    {attribute, fill_dn(DN, Args), AttrName};
fill({'not', SubQuery}, Args) ->
    {'not', fill(SubQuery, Args)};
fill({'and', Queries}, Args) when is_list(Queries) ->
    {'and', [fill(Q, Args) || Q <- Queries]};
fill({'or', Queries}, Args) when is_list(Queries) ->
    {'or', [fill(Q, Args) || Q <- Queries]};
fill({for, Clauses}, Args) when is_list(Clauses) ->
    {for, [fill_for_clause(C, Args) || C <- Clauses]};
fill({equals, A1, A2}, Args) ->
    {equals, fill_value(A1, Args), fill_value(A2, Args)};
fill({match, A1, A2}, Args) ->
    {match, fill_value(A1, Args), fill_value(A2, Args)};
%% tag_queries: a list of {Tag, SubQuery} pairs. Recurse into the sub-query of
%% each pair; leave any non-pair element untouched.
fill(List, Args) when is_list(List) ->
    [fill_list_element(E, Args) || E <- List];
fill(Other, _Args) ->
    Other.

fill_for_clause({Type, Value, SubQuery}, Args) ->
    {Type, Value, fill(SubQuery, Args)};
fill_for_clause(Other, _Args) ->
    Other.

fill_list_element({Tag, SubQuery}, Args) ->
    {Tag, fill(SubQuery, Args)};
fill_list_element(Other, _Args) ->
    Other.

%% A value operand of equals/match: a {string, Pattern} or a bare string is an
%% ACL value sink and is filled RAW (no DN escaping); an embedded DN-bearing
%% term (e.g. {attribute, DN, _}) recurses through fill/2 so its DN is escaped.
fill_value({string, Pattern}, Args) ->
    {string, fill_raw(Pattern, Args)};
fill_value(Operand, Args) when is_tuple(Operand) ->
    fill(Operand, Args);
fill_value(Operand, Args) ->
    case is_dn_string(to_str(Operand)) of
        true -> fill_raw(Operand, Args);
        false -> Operand
    end.

%% Active-Directory split: a DOMAIN\user username yields ${ad_domain}/${ad_user}
%% fill args; any other shape yields none. Mirrors
%% rabbit_auth_backend_ldap_util:get_active_directory_args/1.
-spec ad_args(string() | binary()) -> [{atom(), string()}].
ad_args(Username) when is_binary(Username) ->
    ad_args(binary_to_list(Username));
ad_args(Username) when is_list(Username) ->
    case string:split(Username, "\\", all) of
        [Domain, User] when Domain =/= [], User =/= [] ->
            [{ad_domain, Domain}, {ad_user, User}];
        _ ->
            []
    end;
ad_args(_) ->
    [].

%% Fill a DN sink: RFC 4514-escape every Args value except user_dn (which is
%% already a complete DN), then substitute. Mirrors rabbit_ldap_rfc4514:fill_dn/2.
fill_dn({string, Pattern}, Args) ->
    {string, fill_dn(Pattern, Args)};
fill_dn(DN, Args) ->
    fill_raw(DN, [{K, escape_arg(K, V)} || {K, V} <- Args]).

escape_arg(user_dn, V) -> V;
escape_arg(_, V) -> escape_value(V).

%% Raw template fill: replace each ${Key} with to_repl(Value), per Args entry,
%% globally. Mirrors rabbit_auth_backend_ldap_util:fill/2 exactly (including the
%% & / backslash escaping in to_repl that protects re:replace's replacement
%% syntax). Operates on, and returns, a flat string.
fill_raw(Fmt, []) ->
    to_str(Fmt);
fill_raw(Fmt, [{K, V} | T]) ->
    Var = "\\$\\{" ++ atom_to_list(K) ++ "\\}",
    fill_raw(re:replace(to_str(Fmt), Var, to_repl(V), [global, {return, list}]), T).

%% Escape backslash and ampersand so a substituted value cannot inject
%% re:replace replacement directives. Unstringifiable values become "".
to_repl(V) when is_atom(V) -> to_repl(atom_to_list(V));
to_repl(V) when is_binary(V) -> to_repl(binary_to_list(V));
to_repl([]) -> [];
to_repl([$\\ | T]) -> [$\\, $\\ | to_repl(T)];
to_repl([$& | T]) -> [$\\, $& | to_repl(T)];
to_repl([H | T]) when is_integer(H) -> [H | to_repl(T)];
to_repl(_) -> [].

%% RFC 4514 escaping of a DN attribute value (Section 2.4): backslash-escape
%%   , + " \ < > ; and NUL anywhere; a leading SPACE or #; a trailing SPACE.
%% Mirrors rabbit_ldap_rfc4514:escape_value/1.
escape_value(V) when is_binary(V) -> escape_value(binary_to_list(V));
escape_value(V) when is_atom(V) -> escape_value(atom_to_list(V));
escape_value([]) -> [];
escape_value([H | T]) when H =:= $\s; H =:= $# -> [$\\, H | escape_middle(T)];
escape_value(V) when is_list(V) -> escape_middle(V);
escape_value(V) -> V.

escape_middle([]) ->
    [];
escape_middle([$\s]) ->
    [$\\, $\s];
escape_middle([H | T]) ->
    case is_special(H) of
        true -> [$\\, H | escape_middle(T)];
        false -> [H | escape_middle(T)]
    end.

is_special($,) -> true;
is_special($+) -> true;
is_special($") -> true;
is_special($\\) -> true;
is_special($<) -> true;
is_special($>) -> true;
is_special($;) -> true;
is_special(0) -> true;
is_special(_) -> false.

to_str(V) when is_binary(V) -> binary_to_list(V);
to_str(V) when is_list(V) -> V;
to_str(V) when is_atom(V) -> atom_to_list(V);
to_str(V) -> V.

%% A printable flat string (proper list of integers in a sane char range).
is_dn_string([]) ->
    false;
is_dn_string(L) when is_list(L) ->
    lists:all(fun(C) -> is_integer(C) andalso C >= 0 andalso C =< 16#10FFFF end, L);
is_dn_string(_) ->
    false.
