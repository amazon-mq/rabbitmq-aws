%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0

-module(aws_arn_config_ldap).

-include("aws.hrl").

-export([run/4]).

run(ArnData, _KeyStr, ssl_options, ConfigSubKey) ->
    handle_content(ConfigSubKey, ArnData);
run(ArnData, _KeyStr, dn_lookup_bind, password) ->
    handle_content(dn_lookup_bind_password, ArnData);
run(ArnData, _KeyStr, other_bind, password) ->
    handle_content(other_bind_password, ArnData).

%%--------------------------------------------------------------------------------------------------
%% rabbitmq_auth_backend_ldap plugin
%%
handle_content(cacertfile, PemData) ->
    aws_arn_env:replace(
        rabbitmq_auth_backend_ldap,
        ssl_options,
        cacertfile,
        cacerts,
        aws_pem_util:decode_data(PemData)
    );
handle_content(certfile, PemData) ->
    aws_arn_env:replace(
        rabbitmq_auth_backend_ldap,
        ssl_options,
        certfile,
        certs_keys,
        aws_pem_util:decode_data(PemData)
    );
handle_content(keyfile, PemData) ->
    aws_arn_env:replace(
        rabbitmq_auth_backend_ldap,
        ssl_options,
        keyfile,
        certs_keys,
        aws_pem_util:decode_key_data(PemData)
    );
handle_content(dn_lookup_bind_password, SecretContent) ->
    do_update_ldap_env(dn_lookup_bind, SecretContent);
handle_content(other_bind_password, SecretContent) ->
    do_update_ldap_env(other_bind, SecretContent).

do_update_ldap_env(LdapAppConfigKey, Password) ->
    case application:get_env(rabbitmq_auth_backend_ldap, LdapAppConfigKey) of
        {ok, ExistingConfig} ->
            {ok, NewConfig} = update_ldap_config_password(
                LdapAppConfigKey, ExistingConfig, Password
            ),
            ok = application:set_env(rabbitmq_auth_backend_ldap, LdapAppConfigKey, NewConfig);
        _ ->
            {error, {ldap_config_key_missing, LdapAppConfigKey}}
    end.

-spec update_ldap_config_password(atom(), tuple() | atom(), binary()) -> {ok, tuple()}.
update_ldap_config_password(_ConfigKey, {UserName, _OldPassword}, NewPassword) ->
    % Case 1: Proper tuple - replace with new password
    % Password is already binary from JSON decode
    NewConfig = {UserName, NewPassword},
    {ok, NewConfig};
update_ldap_config_password(ConfigKey, BadConfig, _NewPassword) ->
    % Case 2: Any other value (as_user, anon, etc.) - this should not happen
    ?AWS_LOG_WARNING(
        "expected ~tp to be configured as {username, password} tuple but got ~tp",
        [ConfigKey, BadConfig]
    ),
    {ok, BadConfig}.
