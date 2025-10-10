-module(aws_arn_config_oauth2).

-export([run/3, run/4]).

run(ArnData, _KeyStr, https, cacertfile) ->
    handle_content(cacertfile, ArnData).

run(_KeyStr, providers_https_cacertfile, OAuth2Map) when is_map(OAuth2Map) ->
    % Note:
    % In this case, the ARNs are the values of ArnMap, and the keys
    % are whatever key is necessary to handle the content
    F = fun(MapKey, Arn) ->
        case aws_arn_util:resolve_arn(Arn) of
            {ok, Content} ->
                case handle_oauth2_providers(MapKey, Content) of
                    ok ->
                        ok;
                    Error ->
                        throw({handle_content_error, Error})
                end;
            Error ->
                throw({arn_map_error, Error})
        end
    end,
    try
        ok = maps:foreach(F, OAuth2Map)
    catch
        throw:{arn_map_error, Error} ->
            Error;
        throw:{handle_content_error, Error} ->
            Error
    end.

%%--------------------------------------------------------------------------------------------------
%% rabbitmq_auth_backend_oauth2 plugin
%%
handle_content(cacertfile, PemData) ->
    %% Note: yes it's really key_config and not https like in the
    %% cuttlefish schema
    aws_arn_env:replace(
        rabbitmq_auth_backend_oauth2,
        key_config,
        cacertfile,
        cacerts,
        aws_pem_util:decode_data(PemData)
    ).

handle_oauth2_providers(Key, CaCertPemData) ->
    case aws_pem_util:decode_data(CaCertPemData) of
        {ok, CaCertsDerEncoded} ->
            handle_oauth2_providers_config(
                application:get_env(rabbitmq_auth_backend_oauth2, oauth_providers),
                Key,
                CaCertsDerEncoded
            );
        Error ->
            Error
    end.

handle_oauth2_providers_config({ok, Map0}, Key, CaCertsDerEncoded) when is_map_key(Key, Map0) ->
    % Note:
    % Example for 0th entry in oauth_providers
    % #{<<"0">> => [{https,[{cacertfile,"/tmp/dummycert.pem"}]}],
    Config0 = maps:get(Key, Map0),
    case proplists:get_value(https, Config0) of
        undefined ->
            % TODO: warn?
            ok;
        HttpsProplist0 ->
            HttpsProplist1 = proplists:delete(cacertfile, HttpsProplist0),
            HttpsProplist2 = [{cacerts, CaCertsDerEncoded} | HttpsProplist1],
            Config1 = lists:keyreplace(https, 1, Config0, {https, HttpsProplist2}),
            Map1 = Map0#{Key => Config1},
            ok = application:set_env(rabbitmq_auth_backend_oauth2, oauth_providers, Map1)
    end;
handle_oauth2_providers_config(_, _, _) ->
    ok.
