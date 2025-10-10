-module(aws_arn_env).

-export([replace/3, replace/5]).

-spec replace(
    App :: atom(),
    ConfigKey :: atom(),
    NewValue :: any()
) -> ok.
replace(App, ConfigKey, NewValue) ->
    ok = application:set_env(App, ConfigKey, NewValue).

-spec replace(
    App :: atom(),
    ConfigKey :: atom(),
    KeyToDelete :: atom(),
    Key :: atom(),
    Value :: any()
) -> ok.
replace(App, ConfigKey, cacertfile, cacerts, {ok, CaCertsDerEncoded0}) ->
    {ok, OrigCacertFile} = aws_app_env:delete(App, ConfigKey, cacertfile),
    {ok, CaCertsDerEncoded1} = maybe_add_cacertfile_to_cacerts(OrigCacertFile, CaCertsDerEncoded0),
    ok = aws_app_env:update(App, ConfigKey, cacerts, CaCertsDerEncoded1);
replace(App, ConfigKey, certfile, certs_keys, {ok, [CertDerEncoded]}) ->
    ok = replace_with_certs_keys(App, ConfigKey, certfile, cert, CertDerEncoded);
replace(App, ConfigKey, keyfile, certs_keys, {ok, KeyDerEncoded}) ->
    ok = replace_with_certs_keys(App, ConfigKey, keyfile, key, KeyDerEncoded);
replace(_App, _ConfigKey, _KeyToDelete, _Key, {error, _} = Error) ->
    Error.

%% Note: "false" means that the `cacertfile` setting was *not* present
%% in configuration at all.
maybe_add_cacertfile_to_cacerts(false, CaCertsDerEncoded) ->
    {ok, CaCertsDerEncoded};
maybe_add_cacertfile_to_cacerts({cacertfile, CacertFilePath}, CaCertsDerEncoded) ->
    maybe_add_cacertfile_to_cacerts_from_file(file:read_file(CacertFilePath), CaCertsDerEncoded).

maybe_add_cacertfile_to_cacerts_from_file({ok, CacertBin}, CaCertsDerEncoded) ->
    maybe_add_decoded_pem_to_cacerts(aws_pem_util:decode_data(CacertBin), CaCertsDerEncoded);
maybe_add_cacertfile_to_cacerts_from_file(_, CaCertsDerEncoded) ->
    {ok, CaCertsDerEncoded}.

maybe_add_decoded_pem_to_cacerts({ok, CaCertsDerEncoded0}, CaCertsDerEncoded1) ->
    {ok, CaCertsDerEncoded0 ++ CaCertsDerEncoded1};
maybe_add_decoded_pem_to_cacerts(_, CaCertsDerEncoded1) ->
    {ok, CaCertsDerEncoded1}.

%%% {certs_keys, CertsKeys :: [cert_key_conf()]}
%%% -type cert_key_conf() ::
%%%           #{cert => public_key:der_encoded() | [public_key:der_encoded()],
%%%             key => key(),
%%%             certfile => file:filename(),
%%%             keyfile => file:filename(),
%%%             password => iodata() | fun(() -> iodata())}.
%% App - rabbitmq_management
%% ConfigKey - ssl_config
%% ConfigKeySubKEy - certfile, keyfile
%% CertsKeysKey - cert, key
replace_with_certs_keys(App, ConfigKey, certfile, cert, CertDerEncoded) ->
    do_replace_with_certs_keys(App, ConfigKey, certfile, cert, CertDerEncoded);
replace_with_certs_keys(App, ConfigKey, keyfile, key, CertDerEncoded) ->
    do_replace_with_certs_keys(App, ConfigKey, keyfile, key, CertDerEncoded);
replace_with_certs_keys(_App, _ConfigKey, Arg0, Arg1, _CertDerEncoded) ->
    {error, {invalid_key_combination, {Arg0, Arg1}}}.

do_replace_with_certs_keys(App, ConfigKey, ConfigKeySubKey, CertsKeysKey, CertDerEncoded) ->
    Config0 =
        case application:get_env(App, ConfigKey) of
            {ok, Value} when is_list(Value) ->
                Value;
            undefined ->
                []
        end,
    Config1 = lists:keydelete(ConfigKeySubKey, 1, Config0),
    NewConfig =
        case lists:keyfind(certs_keys, 1, Config1) of
            false ->
                % certs_keys is not in the list, create a new map and add it
                % the value for certs_keys is a *list* of maps
                % https://www.erlang.org/doc/apps/ssl/ssl.html#t:common_option_cert/0
                [{certs_keys, [#{CertsKeysKey => CertDerEncoded}]} | Config1];
            {certs_keys, [CertsKeys0]} when is_map(CertsKeys0) ->
                CertsKeys1 = CertsKeys0#{CertsKeysKey => CertDerEncoded},
                NewCertsKeys = {certs_keys, [CertsKeys1]},
                lists:keyreplace(certs_keys, 1, Config1, NewCertsKeys)
        end,
    ok = application:set_env(App, ConfigKey, NewConfig).
