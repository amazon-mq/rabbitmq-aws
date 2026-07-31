%% ====================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @copyright 2016, Gavin M. Roy
%% @copyright 2007-2024 Broadcom. The term “Broadcom” refers to Broadcom Inc. and/or its subsidiaries. All rights reserved.
%% @private
%% @doc aws_lib configuration functionality
%% @end
%% ====================================================================
-module(aws_lib_config).

%% API
-export([
    credentials/1,
    value/2,
    values/1,
    instance_metadata_url/1,
    instance_credentials_url/1,
    instance_availability_zone_url/0,
    instance_role_url/0,
    instance_id_url/0,
    instance_id/1,
    load_imdsv2_token/0,
    instance_metadata_request_headers/1,
    region/1,
    endpoint_url/1
]).

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

-include("aws_lib.hrl").
-include_lib("kernel/include/logger.hrl").

-spec credentials(aws_config()) ->
    {ok, aws_credentials(), aws_config()} | error().
%% @doc Return the credentials from environment variables, configuration or the
%%      EC2 local instance metadata server, if available.
%%
%%      If the ``AWS_ACCESS_KEY_ID`` and ``AWS_SECRET_ACCESS_KEY`` environment
%%      variables are set, those values will be returned. If they are not, the
%%      local configuration file or shared credentials file will be consulted.
%%      If either exists and can be checked, they will attempt to return the
%%      authentication credential values for the ``default`` profile if the
%%      ``AWS_DEFAULT_PROFILE`` environment is not set.
%%
%%      When checking for the configuration file, it will attempt to read the
%%      file from ``~/.aws/config`` if the ``AWS_CONFIG_FILE`` environment
%%      variable is not set. If the file is found, and both the access key and
%%      secret access key are set for the profile, they will be returned. If not
%%      it will attempt to consult the shared credentials file.
%%
%%      When checking for the shared credentials file, it will attempt to read
%%      read from ``~/.aws/credentials`` if the ``AWS_SHARED_CREDENTIALS_FILE``
%%      environment variable is not set. If the file is found and the both the
%%      access key and the secret access key are set for the profile, they will
%%      be returned.
%%
%%      If credentials are returned at any point up through this stage, they
%%      will be returned as ``{ok, AccessKey, SecretKey, undefined}``,
%%      indicating the credentials are locally configured, and are not
%%      temporary.
%%
%%      If no credentials could be resolved up until this point, there will be
%%      an attempt to contact a local EC2 instance metadata service for
%%      credentials.
%%
%%      When the EC2 instance metadata server is checked for but does not exist,
%%      the operation will timeout in ``?DEFAULT_IMDS_TIMEOUT``ms.
%%
%%      When the EC2 instance metadata server exists, but data is not returned
%%      quickly, the operation will timeout in ``?DEFAULT_IMDS_TIMEOUT``ms.
%%
%%      If the service does exist, it will attempt to use the
%%      ``/meta-data/iam/security-credentials`` endpoint to request expiring
%%      request credentials to use. If they are found, a tuple of
%%      ``{ok, AccessKey, SecretAccessKey, SecurityToken}`` will be returned
%%      indicating the credentials are temporary and require the use of the
%%      ``X-Amz-Security-Token`` header should be used.
%%
%%      Finally, if no credentials are found by this point, an error tuple
%%      will be returned.
%% @end
credentials(Config) ->
    credentials(profile(), Config).

-spec credentials(string(), aws_config()) ->
    {ok, aws_credentials(), aws_config()} | error().
%% @doc Return the credentials from environment variables, configuration or the
%%      EC2 local instance metadata server, if available.
%%
%%      If the ``AWS_ACCESS_KEY_ID`` and ``AWS_SECRET_ACCESS_KEY`` environment
%%      variables are set, those values will be returned. If they are not, the
%%      local configuration file or shared credentials file will be consulted.
%%
%%      When checking for the configuration file, it will attempt to read the
%%      file from ``~/.aws/config`` if the ``AWS_CONFIG_FILE`` environment
%%      variable is not set. If the file is found, and both the access key and
%%      secret access key are set for the profile, they will be returned. If not
%%      it will attempt to consult the shared credentials file.
%%
%%      When checking for the shared credentials file, it will attempt to read
%%      read from ``~/.aws/credentials`` if the ``AWS_SHARED_CREDENTIALS_FILE``
%%      environment variable is not set. If the file is found and the both the
%%      access key and the secret access key are set for the profile, they will
%%      be returned.
%%
%%      If credentials are returned at any point up through this stage, they
%%      will be returned as ``{ok, AccessKey, SecretKey, undefined}``,
%%      indicating the credentials are locally configured, and are not
%%      temporary.
%%
%%      If no credentials could be resolved up until this point, there will be
%%      an attempt to contact a local EC2 instance metadata service for
%%      credentials.
%%
%%      When the EC2 instance metadata server is checked for but does not exist,
%%      the operation will timeout in ``?DEFAULT_IMDS_TIMEOUT``ms.
%%
%%      When the EC2 instance metadata server exists, but data is not returned
%%      quickly, the operation will timeout in ``?DEFAULT_IMDS_TIMEOUT``ms.
%%
%%      If the service does exist, it will attempt to use the
%%      ``/meta-data/iam/security-credentials`` endpoint to request expiring
%%      request credentials to use. If they are found, a tuple of
%%      ``{ok, AccessKey, SecretAccessKey, SecurityToken}`` will be returned
%%      indicating the credentials are temporary and require the use of the
%%      ``X-Amz-Security-Token`` header should be used.
%%
%%      Finally, if no credentials are found by this point, an error tuple
%%      will be returned.
%% @end
credentials(Profile, Config) ->
    lookup_credentials(
        Profile,
        os:getenv("AWS_ACCESS_KEY_ID"),
        os:getenv("AWS_SECRET_ACCESS_KEY"),
        os:getenv("AWS_SESSION_TOKEN"),
        Config
    ).

-spec region(Config :: aws_config()) -> {ok, string(), aws_config()}.
%% @doc Return the region as configured by ``AWS_DEFAULT_REGION`` environment
%%      variable or as configured in the configuration file using the default
%%      profile or configured ``AWS_DEFAULT_PROFILE`` environment variable.
%%
%%      If the environment variable is not set and a configuration
%%      file is not found, it will try and return the region from the EC2
%%      local instance metadata server.
%% @end
region(Config) ->
    region(profile(), Config).

-spec region(Region :: string(), Config :: aws_config()) ->
    {ok, region(), aws_config()}.
%% @doc Return the region as configured by ``AWS_DEFAULT_REGION`` environment
%%      variable or as configured in the configuration file using the specified
%%      profile.
%%
%%      If the environment variable is not set and a configuration
%%      file is not found, it will try and return the region from the EC2
%%      local instance metadata server.
%% @end
region(Profile, Config) ->
    case lookup_region(Profile, os:getenv("AWS_DEFAULT_REGION"), Config) of
        {ok, Region, Config1} ->
            {ok, Region, Config1};
        _ ->
            {ok, ?DEFAULT_REGION, Config}
    end.

-spec endpoint_url(Service :: string()) -> string() | undefined.
%% @doc Return the endpoint-URL override for the given AWS service, or
%%      `undefined' when none is configured. This matches the AWS CLI v2 and
%%      official SDK convention for local-development endpoints (LocalStack,
%%      moto, and similar): the per-service ``AWS_ENDPOINT_URL_<SERVICE>``
%%      variable takes precedence over the global ``AWS_ENDPOINT_URL``.
%%
%%      The service name is upper-cased and its hyphens are replaced with
%%      underscores to form the variable suffix, so the "acm-pca" service maps
%%      to ``AWS_ENDPOINT_URL_ACM_PCA``.
%% @end
endpoint_url(Service) ->
    case os:getenv("AWS_ENDPOINT_URL_" ++ service_env_suffix(Service)) of
        Value when is_list(Value), Value =/= "" ->
            Value;
        _ ->
            case os:getenv("AWS_ENDPOINT_URL") of
                Value when is_list(Value), Value =/= "" -> Value;
                _ -> undefined
            end
    end.

%% Map a service name to the AWS_ENDPOINT_URL_<SERVICE> variable suffix:
%% upper-cased with hyphens turned into underscores (e.g. "acm-pca" ->
%% "ACM_PCA"), matching the AWS SDK convention.
service_env_suffix(Service) ->
    string:uppercase(
        lists:map(
            fun
                ($-) -> $_;
                (C) -> C
            end,
            Service
        )
    ).

-spec instance_id(aws_config()) -> {'ok', string(), aws_config()} | {'error', 'undefined'}.
%% @doc Return the instance ID from the EC2 metadata service.
%% @end
instance_id(Config) ->
    URL = instance_id_url(),
    case perform_http_get_instance_metadata(URL, Config) of
        {ok, Response, Config1} ->
            case parse_body_response({ok, Response}) of
                {ok, Body} ->
                    {ok, Body, Config1};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec value(Profile :: string(), Key :: atom()) ->
    Value :: any() | {error, Reason :: atom()}.
%% @doc Return the configuration data for the specified profile or an error
%%      if the profile is not found.
%% @end
value(Profile, Key) ->
    get_value(Key, values(Profile)).

-spec values(Profile :: string()) ->
    Settings ::
        list()
        | {error, Reason :: atom()}.
%% @doc Return the configuration data for the specified profile or an error
%%      if the profile is not found.
%% @end
values(Profile) ->
    case config_file_data() of
        {error, Reason} ->
            {error, Reason};
        Settings ->
            Prefixed = lists:flatten(["profile ", Profile]),
            proplists:get_value(
                Profile,
                Settings,
                proplists:get_value(
                    Prefixed,
                    Settings,
                    {error, undefined}
                )
            )
    end.

%% -----------------------------------------------------------------------------
%% Private / Internal Methods
%% -----------------------------------------------------------------------------

-spec config_file() -> string().
%% @doc Return the configuration file to test using either the value of the
%%      AWS_CONFIG_FILE or the default location where the file is expected to
%%      exist.
%% @end
config_file() ->
    config_file(os:getenv("AWS_CONFIG_FILE")).

-spec config_file(Path :: false | string()) -> string().
%% @doc Return the configuration file to test using either the value of the
%%      AWS_CONFIG_FILE or the default location where the file is expected to
%%      exist.
%% @end
config_file(false) ->
    filename:join([home_path(), ".aws", "config"]);
config_file(EnvVar) ->
    EnvVar.

-spec config_file_data() -> list() | {error, Reason :: atom()}.
%% @doc Return the values from a configuration file as a proplist by section
%% @end
config_file_data() ->
    ini_file_data(config_file()).

-spec credentials_file() -> string().
%% @doc Return the shared credentials file to test using either the value of the
%%      AWS_SHARED_CREDENTIALS_FILE or the default location where the file
%%      is expected to exist.
%% @end
credentials_file() ->
    credentials_file(os:getenv("AWS_SHARED_CREDENTIALS_FILE")).

-spec credentials_file(Path :: false | string()) -> string().
%% @doc Return the shared credentials file to test using either the value of the
%%      AWS_SHARED_CREDENTIALS_FILE or the default location where the file
%%      is expected to exist.
%% @end
credentials_file(false) ->
    filename:join([home_path(), ".aws", "credentials"]);
credentials_file(EnvVar) ->
    EnvVar.

-spec credentials_file_data() -> list() | {error, Reason :: atom()}.
%% @doc Return the values from a configuration file as a proplist by section
%% @end
credentials_file_data() ->
    ini_file_data(credentials_file()).

-spec get_value
    (Key :: atom(), Settings :: list()) -> any();
    (Key :: atom(), {error, Reason :: atom()}) -> {error, Reason :: atom()}.
%% @doc Get the value for a key from a settings proplist.
%% @end
get_value(Key, Settings) when is_list(Settings) ->
    proplists:get_value(Key, Settings, {error, undefined});
get_value(_, {error, Reason}) ->
    {error, Reason}.

-spec home_path() -> string().
%% @doc Return the path to the current user's home directory, checking for the
%%      HOME environment variable before returning the current working
%%      directory if it's not set.
%% @end
home_path() ->
    home_path(os:getenv("HOME")).

-spec home_path(Value :: string() | false) -> string().
%% @doc Return the path to the current user's home directory, checking for the
%%      HOME environment variable before returning the current working
%%      directory if it's not set.
%% @end
home_path(false) -> filename:absname(".");
home_path(Value) -> Value.

-spec ini_file_data(Path :: string()) ->
    list() | {error, atom()}.
%% @doc Return the parsed ini file for the specified path.
%% @end
ini_file_data(Path) ->
    ini_file_data(Path, filelib:is_file(Path)).

-spec ini_file_data(Path :: string(), FileExists :: boolean()) ->
    list() | {error, atom()}.
%% @doc Return the parsed ini file for the specified path.
%% @end
ini_file_data(Path, true) ->
    case read_file(Path) of
        {ok, Lines} -> ini_parse_lines(Lines, none, none, []);
        {error, Reason} -> {error, Reason}
    end;
ini_file_data(_, false) ->
    {error, enoent}.

-spec ini_format_key(any()) -> atom() | {error, type}.
%% @doc Converts a ini file key to an atom, stripping any leading whitespace
%% @end
ini_format_key(Key) ->
    case io_lib:printable_list(Key) of
        true -> list_to_atom(string:strip(Key));
        false -> {error, type}
    end.

-spec ini_parse_line(
    Section :: list(),
    Key :: atom(),
    Line :: binary()
) ->
    {Section :: list(), Key :: string() | none}.
%% @doc Parse the AWS configuration INI file, returning a proplist
%% @end
ini_parse_line(Section, Parent, <<" ", Line/binary>>) ->
    Child = proplists:get_value(Parent, Section, []),
    {ok, NewChild} = ini_parse_line_parts(Child, ini_split_line(Line)),
    {lists:keystore(Parent, 1, Section, {Parent, NewChild}), Parent};
ini_parse_line(Section, _, Line) ->
    case ini_parse_line_parts(Section, ini_split_line(Line)) of
        {ok, NewSection} -> {NewSection, none};
        {new_parent, Parent} -> {Section, Parent}
    end.

-spec ini_parse_line_parts(
    Section :: list(),
    Parts :: list()
) ->
    {ok, list()} | {new_parent, atom()}.
%% @doc Parse the AWS configuration INI file, returning a proplist
%% @end
ini_parse_line_parts(Section, []) ->
    {ok, Section};
ini_parse_line_parts(Section, [RawKey, Value]) ->
    Key = ini_format_key(RawKey),
    {ok, lists:keystore(Key, 1, Section, {Key, maybe_convert_number(Value)})};
ini_parse_line_parts(_, [RawKey]) ->
    {new_parent, ini_format_key(RawKey)}.

-spec ini_parse_lines(
    Lines :: [binary()],
    SectionName :: string() | atom(),
    Parent :: atom(),
    Accumulator :: list()
) ->
    list().
%% @doc Parse the AWS configuration INI file
%% @end
ini_parse_lines([], _, _, Settings) ->
    Settings;
ini_parse_lines([H | T], SectionName, Parent, Settings) ->
    {ok, NewSectionName} = ini_parse_section_name(SectionName, H),
    {ok, NewParent, NewSettings} = ini_parse_section(
        H,
        NewSectionName,
        Parent,
        Settings
    ),
    ini_parse_lines(T, NewSectionName, NewParent, NewSettings).

-spec ini_parse_section(
    Line :: binary(),
    SectionName :: string(),
    Parent :: atom(),
    Section :: list()
) ->
    {ok, NewParent :: atom(), Section :: list()}.
%% @doc Parse a line from the ini file, returning it as part of the appropriate
%%      section.
%% @end
ini_parse_section(Line, SectionName, Parent, Settings) ->
    Section = proplists:get_value(SectionName, Settings, []),
    {NewSection, NewParent} = ini_parse_line(Section, Parent, Line),
    {ok, NewParent,
        lists:keystore(
            SectionName,
            1,
            Settings,
            {SectionName, NewSection}
        )}.

-spec ini_parse_section_name(
    CurrentSection :: string() | atom(),
    Line :: binary()
) ->
    {ok, SectionName :: string()}.
%% @doc Attempts to parse a section name from the current line, returning either
%%      the new parsed section name, or the current section name.
%% @end
ini_parse_section_name(CurrentSection, Line) ->
    Value = binary_to_list(Line),
    case re:run(Value, "\\[([\\w\\s+\\-_]+)\\]", [{capture, all, list}]) of
        {match, [_, SectionName]} -> {ok, SectionName};
        nomatch -> {ok, CurrentSection}
    end.

-spec ini_split_line(binary()) -> list().
%% @doc Split a key value pair on the first ``=`` only, returning ``[Key]`` for a
%%      section/parent line or ``[Key, Value]`` for a key/value line. Splitting on
%%      only the first ``=`` keeps any ``=`` in the value intact (e.g. a
%%      ``credential_process`` command line such as ``aws ... --account=123``),
%%      which a naive split on every ``=`` would break into 3+ parts and crash
%%      ini_parse_line_parts/2.
%% @end
ini_split_line(Line) ->
    Stripped = string:strip(binary_to_list(Line)),
    case string:str(Stripped, "=") of
        0 ->
            [Stripped];
        Index ->
            Key = string:substr(Stripped, 1, Index - 1),
            case string:substr(Stripped, Index + 1) of
                %% An empty value (e.g. "s3 =") denotes a parent/section line, so
                %% return [Key] only -- matching the previous string:tokens/2
                %% behaviour that dropped the trailing empty token.
                "" -> [Key];
                Value -> [Key, Value]
            end
    end.

-spec instance_availability_zone_url() -> string().
%% @doc Return the URL for querying the availability zone from the Instance
%%      Metadata service
%% @end
instance_availability_zone_url() ->
    instance_metadata_url(string:join([?INSTANCE_METADATA_BASE, ?INSTANCE_AZ], "/")).

-spec instance_credentials_url(string()) -> string().
%% @doc Return the URL for querying temporary credentials from the Instance
%%      Metadata service for the specified role
%% @end
instance_credentials_url(Role) ->
    instance_metadata_url(string:join([?INSTANCE_METADATA_BASE, ?INSTANCE_CREDENTIALS, Role], "/")).

-spec instance_metadata_url(string()) -> string().
%% @doc Build the Instance Metadata service URL for the specified path
%% @end
instance_metadata_url(Path) ->
    uri_string:recompose(#{scheme => "http", host => ?INSTANCE_HOST, path => Path}).

-spec instance_role_url() -> string().
%% @doc Return the URL for querying the role associated with the current
%%      instance from the Instance Metadata service
%% @end
instance_role_url() ->
    instance_metadata_url(string:join([?INSTANCE_METADATA_BASE, ?INSTANCE_CREDENTIALS], "/")).

-spec imdsv2_token_url() -> string().
%% @doc Return the URL for obtaining EC2 IMDSv2 token from the Instance Metadata service.
%% @end
imdsv2_token_url() ->
    instance_metadata_url(?TOKEN_URL).

-spec instance_id_url() -> string().
%% @doc Return the URL for querying the id of the current instance from the Instance Metadata service.
%% @end
instance_id_url() ->
    instance_metadata_url(string:join([?INSTANCE_METADATA_BASE, ?INSTANCE_ID], "/")).

-spec lookup_credentials(
    Profile :: string(),
    AccessKey :: string() | false,
    SecretKey :: string() | false,
    SessionToken :: string() | false,
    Config :: aws_config()
) ->
    {ok, aws_credentials(), aws_config()} | error().
%% @doc Return the access key and secret access key if they are set in
%%      environment variables, otherwise lookup the credentials from the config
%%      file for the specified profile.
%% @end
lookup_credentials(Profile, false, _, _, Config) ->
    lookup_credentials_from_config(
        Profile,
        value(Profile, aws_access_key_id),
        value(Profile, aws_secret_access_key),
        value(Profile, aws_session_token),
        Config
    );
lookup_credentials(Profile, _, false, _, Config) ->
    lookup_credentials_from_config(
        Profile,
        value(Profile, aws_access_key_id),
        value(Profile, aws_secret_access_key),
        value(Profile, aws_session_token),
        Config
    );
lookup_credentials(_, AccessKey, SecretKey, SessionToken, Config) ->
    Creds =
        case SessionToken of
            false ->
                #aws_credentials{
                    access_key = AccessKey,
                    secret_key = SecretKey,
                    security_token = undefined,
                    expiration = undefined
                };
            SessionToken ->
                #aws_credentials{
                    access_key = AccessKey,
                    secret_key = SecretKey,
                    security_token = SessionToken,
                    expiration = undefined
                }
        end,
    {ok, Creds, Config}.

-spec lookup_credentials_from_config(
    Profile :: string(),
    access_key() | {error, Reason :: atom()},
    secret_access_key() | {error, Reason :: atom()},
    security_token() | {error, Reason :: atom()},
    aws_config()
) ->
    {ok, aws_credentials(), aws_config()} | error().
%% @doc Return the access key and secret access key if they are set in
%%      for the specified profile in the config file, if it exists. If it does
%%      not exist or the profile is not set or the values are not set in the
%%      profile, look up the values in the shared credentials file
%% @end
lookup_credentials_from_config(Profile, {error, _}, _, _, Config) ->
    lookup_credentials_from_file(Profile, credentials_file_data(), Config);
lookup_credentials_from_config(_, AccessKey, SecretKey, SessionToken, Config) ->
    Creds =
        case SessionToken of
            {error, _} ->
                #aws_credentials{
                    access_key = AccessKey,
                    secret_key = SecretKey,
                    security_token = undefined,
                    expiration = undefined
                };
            SessionToken ->
                #aws_credentials{
                    access_key = AccessKey,
                    secret_key = SecretKey,
                    security_token = SessionToken,
                    expiration = undefined
                }
        end,
    {ok, Creds, Config}.

-spec lookup_credentials_from_file(
    Profile :: string(),
    Credentials :: list(),
    Config :: aws_config()
) ->
    {ok, aws_credentials(), aws_config()} | error().
%% @doc Check to see if the shared credentials file exists and if it does,
%%      invoke ``lookup_credentials_from_shared_creds_section/2`` to attempt to
%%      get the credentials values out of it. If the file does not exist,
%%      attempt to lookup the values from the container credentials endpoint or
%%      the EC2 instance metadata service.
%% @end
lookup_credentials_from_file(_, {error, _}, Config) ->
    lookup_credentials_from_container_or_imds(Config);
lookup_credentials_from_file(Profile, Credentials, Config) ->
    Section = proplists:get_value(Profile, Credentials),
    lookup_credentials_from_section(Section, Config).

-spec lookup_credentials_from_section(
    Credentials :: list() | undefined,
    Config :: aws_config()
) ->
    {ok, aws_credentials(), aws_config()} | error().
%% @doc Return the access key and secret access key if they are set in
%%      for the specified profile from the shared credentials file. If the
%%      profile is not set or the values are not set in the profile, attempt to
%%      lookup the values from the container credentials endpoint or the EC2
%%      instance metadata service.
%% @end
lookup_credentials_from_section(undefined, Config) ->
    lookup_credentials_from_container_or_imds(Config);
lookup_credentials_from_section(Credentials, Config) ->
    AccessKey = proplists:get_value(aws_access_key_id, Credentials, undefined),
    SecretKey = proplists:get_value(aws_secret_access_key, Credentials, undefined),
    SessionToken = proplists:get_value(aws_session_token, Credentials, undefined),
    lookup_credentials_from_proplist(AccessKey, SecretKey, SessionToken, Config).

-spec lookup_credentials_from_proplist(
    AccessKey :: access_key(),
    SecretAccessKey :: secret_access_key(),
    SessionToken :: security_token(),
    Config :: aws_config()
) ->
    {ok, aws_credentials(), aws_config()} | error().
%% @doc Process the contents of the Credentials proplists checking if the
%%      access key and secret access key are both set.
%% @end
lookup_credentials_from_proplist(undefined, _, _, Config) ->
    lookup_credentials_from_container_or_imds(Config);
lookup_credentials_from_proplist(_, undefined, _, Config) ->
    lookup_credentials_from_container_or_imds(Config);
lookup_credentials_from_proplist(AccessKey, SecretKey, SessionToken, Config) ->
    Creds = #aws_credentials{
        access_key = AccessKey,
        secret_key = SecretKey,
        security_token = SessionToken,
        expiration = undefined
    },
    {ok, Creds, Config}.

%% =============================================================================
%% Container Credentials (ECS/EKS)
%%
%% The AWS credential chain for containers inserts between config/credentials
%% files and IMDS. When AWS_CONTAINER_CREDENTIALS_RELATIVE_URI or
%% AWS_CONTAINER_CREDENTIALS_FULL_URI is set, credentials are fetched from that
%% endpoint. If the env var is set but the fetch fails, we HARD ERROR (no
%% fallthrough to IMDS) -- falling through would silently use the EC2 host
%% instance role instead of the intended ECS task role.
%% =============================================================================

-spec lookup_credentials_from_container_or_imds(aws_config()) ->
    {ok, aws_credentials(), aws_config()} | error().
%% @doc Funnel that ALL IMDS fallthrough sites call. Checks for container
%%      credential env vars; if present, fetches from the container endpoint.
%%      If absent, falls through to IMDS as before.
%% @end
lookup_credentials_from_container_or_imds(Config) ->
    case container_credentials_url() of
        {ok, URL} ->
            try container_auth_token() of
                Token ->
                    case fetch_container_credentials(URL, Token) of
                        {ok, Creds} ->
                            {ok, Creds, Config};
                        {error, Reason} ->
                            %% Hard error: the operator explicitly configured container
                            %% credentials (env var is set) but the fetch failed. Do NOT
                            %% fall through to IMDS -- that would silently use the EC2
                            %% host instance role instead of the intended task role.
                            %% Log only the error category, not the full reason -- the
                            %% reason may contain response body fragments (P0 R6).
                            ?LOG_ERROR(
                                "Container credentials fetch failed: ~tp. "
                                "Not falling through to IMDS because "
                                "AWS_CONTAINER_CREDENTIALS_*_URI is set.",
                                [sanitize_container_error(Reason)]
                            ),
                            {error, {container_credentials_failed, Reason}}
                    end
            catch
                error:TokenErr ->
                    %% Token file configured but unreadable/oversized -- hard error.
                    ?LOG_ERROR(
                        "Container credentials authorization token error: ~tp. "
                        "Not falling through to IMDS.",
                        [TokenErr]
                    ),
                    {error, {container_credentials_failed, {token_error, TokenErr}}}
            end;
        {error, uri_not_allowed, Reason} ->
            %% FULL_URI failed SSRF allowlist validation -- hard error, no fallthrough.
            ?LOG_ERROR(
                "Container credentials FULL_URI not allowed: ~tp. "
                "Not falling through to IMDS.",
                [Reason]
            ),
            {error, {container_credentials_failed, Reason}};
        not_configured ->
            lookup_credentials_from_instance_metadata(Config)
    end.

-spec container_credentials_url() ->
    {ok, string()} | not_configured | {error, uri_not_allowed, term()}.
%% @doc Build the container credentials URL from environment variables.
%%      AWS_CONTAINER_CREDENTIALS_RELATIVE_URI: a path appended to the
%%      ECS credentials host (169.254.170.2).
%%      AWS_CONTAINER_CREDENTIALS_FULL_URI: a complete URL subject to SSRF
%%      allowlist validation.
%%      RELATIVE_URI takes precedence (matches AWS SDK behavior).
%% @end
container_credentials_url() ->
    case os:getenv("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI") of
        RelUri when is_list(RelUri), RelUri =/= "" ->
            BaseHost = container_credentials_base_host(),
            {ok, "http://" ++ BaseHost ++ RelUri};
        _ ->
            case os:getenv("AWS_CONTAINER_CREDENTIALS_FULL_URI") of
                FullUri when is_list(FullUri), FullUri =/= "" ->
                    case validate_full_uri(FullUri) of
                        ok -> {ok, FullUri};
                        {error, Reason} -> {error, uri_not_allowed, Reason}
                    end;
                _ ->
                    not_configured
            end
    end.

-spec container_credentials_base_host() -> string().
%% @doc Return the base host for RELATIVE_URI. Defaults to the ECS task
%%      metadata endpoint (169.254.170.2). In test builds only, an
%%      application env override allows integration tests to point at a
%%      local stub; this compiles out in production.
%% @end
-ifdef(TEST).
container_credentials_base_host() ->
    case application:get_env(aws, container_credentials_host_override) of
        {ok, Host} when is_list(Host), Host =/= "" ->
            Host;
        _ ->
            ?ECS_CREDENTIALS_HOST
    end.
-else.
container_credentials_base_host() ->
    ?ECS_CREDENTIALS_HOST.
-endif.

-spec validate_full_uri(string()) -> ok | {error, term()}.
%% @doc SSRF allowlist for AWS_CONTAINER_CREDENTIALS_FULL_URI.
%%
%%      The AWS SDKs restrict this to:
%%        - Loopback (127.0.0.0/8, ::1)
%%        - ECS/EKS link-local (169.254.170.2, 169.254.170.23, fd00:ec2::23)
%%        - OR any host when the scheme is HTTPS (TLS authenticates the endpoint)
%%
%%      This is an ALLOWLIST (not a denylist). Plaintext HTTP is ONLY permitted
%%      to loopback or the specific ECS/EKS link-local addresses above.
%%
%%      We do NOT reuse aws_auth_validate_net.erl -- that module uses a denylist
%%      model (blocks known-bad CIDRs) for a different use case (outbound
%%      validation to arbitrary customer servers). Our use case is simpler and
%%      stricter: allowlist a small set for plaintext, HTTPS bypasses.
%% @end
validate_full_uri(URI) ->
    case aws_lib_uri:parse(URI) of
        {ok, UriMap} ->
            Scheme = string:lowercase(
                unicode:characters_to_list(maps:get(scheme, UriMap, ""))
            ),
            case Scheme of
                "https" ->
                    %% HTTPS to any host is allowed -- TLS authenticates the endpoint.
                    ok;
                "http" ->
                    Host = aws_lib_uri:host(UriMap),
                    case is_allowed_plaintext_host(Host) of
                        true -> ok;
                        false -> {error, {full_uri_not_allowed, Host}}
                    end;
                Other ->
                    {error, {unsupported_scheme, Other}}
            end;
        {error, Reason} ->
            {error, {malformed_full_uri, Reason}}
    end.

-spec is_allowed_plaintext_host(string()) -> boolean().
%% @doc Return true if the host is safe for plaintext HTTP container credential
%%      requests. Only loopback and the specific ECS/EKS link-local addresses
%%      are permitted.
%% @end
is_allowed_plaintext_host(Host) ->
    case inet:parse_address(Host) of
        {ok, {127, _, _, _}} ->
            true;
        {ok, {0, 0, 0, 0, 0, 0, 0, 1}} ->
            %% ::1
            true;
        {ok, IPv4} when tuple_size(IPv4) =:= 4 ->
            is_ecs_link_local_v4(IPv4);
        {ok, IPv6} when tuple_size(IPv6) =:= 8 ->
            is_ecs_link_local_v6(IPv6);
        {error, einval} ->
            %% Not a literal IP -- hostnames are NOT allowed for plaintext.
            false
    end.

is_ecs_link_local_v4(IP) ->
    case inet:parse_address(?ECS_CREDENTIALS_HOST) of
        {ok, IP} ->
            true;
        _ ->
            case inet:parse_address(?ECS_CREDENTIALS_HOST_ALT) of
                {ok, IP} -> true;
                _ -> false
            end
    end.

is_ecs_link_local_v6(IP) ->
    case inet:parse_address(?ECS_CREDENTIALS_HOST_V6) of
        {ok, IP} -> true;
        _ -> false
    end.

-spec container_auth_token() -> string() | undefined.
%% @doc Read the authorization token for the container credentials endpoint.
%%      AWS_CONTAINER_AUTHORIZATION_TOKEN_FILE takes precedence over
%%      AWS_CONTAINER_AUTHORIZATION_TOKEN (matches SDK behavior).
%%      If TOKEN_FILE is set but unreadable/oversized, raises an error tuple
%%      that the caller treats as a hard failure.
%%      NEVER logs the token value (P0 R6 -- no credential leakage).
%% @end
container_auth_token() ->
    case os:getenv("AWS_CONTAINER_AUTHORIZATION_TOKEN_FILE") of
        TokenFile when is_list(TokenFile), TokenFile =/= "" ->
            read_token_file(TokenFile);
        _ ->
            case os:getenv("AWS_CONTAINER_AUTHORIZATION_TOKEN") of
                Token when is_list(Token), Token =/= "" ->
                    Token;
                _ ->
                    undefined
            end
    end.

-spec read_token_file(string()) -> string() | no_return().
%% @doc Read a bearer token from a file. Validates the file is readable and
%%      within the size cap. Raises on failure so the credential chain can
%%      distinguish "token configured but broken" from "no token configured".
%%      Reads first, then checks size -- eliminates TOCTOU between stat and read.
%% @end
read_token_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} when byte_size(Bin) =< ?CONTAINER_AUTH_TOKEN_MAX_SIZE ->
            string:trim(binary_to_list(Bin));
        {ok, Bin} ->
            error({container_token_file_too_large, byte_size(Bin)});
        {error, Reason} ->
            error({container_token_file_unreadable, Reason})
    end.

-spec fetch_container_credentials(string(), string() | undefined) ->
    {ok, aws_credentials()} | {error, term()}.
%% @doc Fetch temporary credentials from an ECS/EKS container credential
%%      endpoint. The endpoint returns the same JSON shape as the IMDS
%%      security-credentials endpoint.
%%
%%      Security:
%%        - Never follows HTTP redirects (a redirect could leak the Authorization
%%          token to an attacker-controlled host; gun does not follow by default).
%%        - Enforces a response body size cap. The cap is checked after the body
%%          is buffered (gun reads the full body before returning). Practical OOM
%%          risk is bounded by the 5s response timeout and the fact that for HTTP
%%          the endpoint MUST be loopback or ECS link-local (both trusted). For
%%          HTTPS, the attacker must hold a valid TLS certificate for the host.
%%        - Never logs the token or credential values.
%% @end
fetch_container_credentials(URL, Token) ->
    case aws_lib_uri:parse(URL) of
        {ok, Uri} ->
            Host = aws_lib_uri:host(Uri),
            Port = aws_lib_uri:port(Uri),
            Path = aws_lib_uri:target(Uri),
            Transport = aws_lib_uri:transport(Uri),
            Headers = container_creds_headers(Token),
            Opts = container_creds_open_opts(Transport, Host),
            case aws_lib_httpc:request(Host, Port, get, Path, Headers, <<>>, Opts) of
                {ok, {{_, 200, _}, RespHeaders, Body}} ->
                    %% Check body size cap. Also verify Content-Length if present
                    %% (early reject before full buffering on compliant servers).
                    CLHeaderOk =
                        case content_length_value(RespHeaders) of
                            undefined -> true;
                            CL when CL =< ?CONTAINER_CREDS_MAX_BODY -> true;
                            _ -> false
                        end,
                    case CLHeaderOk andalso byte_size(Body) =< ?CONTAINER_CREDS_MAX_BODY of
                        true ->
                            parse_container_credentials_body(Body);
                        false ->
                            {error, response_body_too_large}
                    end;
                {ok, {{_, Status, Reason}, _RespHeaders, _Body}} ->
                    {error, {http_error, Status, Reason}};
                {error, Reason} ->
                    {error, {transport_error, Reason}}
            end;
        {error, Reason} ->
            {error, {malformed_url, Reason}}
    end.

-spec content_length_value([{string(), string()}]) -> non_neg_integer() | undefined.
%% @doc Extract the Content-Length header value, if present and parseable.
%% @end
content_length_value([]) ->
    undefined;
content_length_value([{Key, Value} | Rest]) ->
    case string:lowercase(Key) of
        "content-length" ->
            case string:to_integer(string:trim(Value)) of
                {Int, ""} when Int >= 0 -> Int;
                _ -> undefined
            end;
        _ ->
            content_length_value(Rest)
    end.

-spec container_creds_headers(string() | undefined) -> headers().
%% @doc Build request headers for the container credentials endpoint.
%% @end
container_creds_headers(undefined) ->
    [];
container_creds_headers(Token) ->
    [{"Authorization", Token}].

-spec container_creds_open_opts(tls | tcp, string()) -> map().
%% @doc Build gun open options for the container credentials connection.
%%      For HTTPS, configures TLS with verify_peer using OTP system CAs and
%%      hostname verification. For plaintext, uses plain TCP.
%% @end
container_creds_open_opts(tls, Host) ->
    BaseTlsOpts = [
        {verify, verify_peer},
        {cacerts, public_key:cacerts_get()},
        {customize_hostname_check, [
            {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
        ]}
    ],
    %% SNI must be a DNS hostname per RFC 6066; do not set it for IP literals.
    TlsOpts =
        case inet:parse_address(Host) of
            {ok, _IP} -> BaseTlsOpts;
            {error, einval} -> [{server_name_indication, Host} | BaseTlsOpts]
        end,
    #{
        transport => tls,
        protocols => [http],
        connect_timeout => ?CONTAINER_CREDS_CONNECT_TIMEOUT,
        timeout => ?CONTAINER_CREDS_RESPONSE_TIMEOUT,
        tls_opts => TlsOpts
    };
container_creds_open_opts(tcp, _Host) ->
    #{
        transport => tcp,
        protocols => [http],
        connect_timeout => ?CONTAINER_CREDS_CONNECT_TIMEOUT,
        timeout => ?CONTAINER_CREDS_RESPONSE_TIMEOUT
    }.

-spec parse_container_credentials_body(binary()) ->
    {ok, aws_credentials()} | {error, term()}.
%% @doc Parse the JSON credentials response from the container endpoint.
%%      The response format matches IMDS security-credentials:
%%      {"AccessKeyId", "SecretAccessKey", "Token", "Expiration"}.
%% @end
parse_container_credentials_body(Body) ->
    try
        Parsed = aws_lib_json:decode(Body),
        AccessKey = proplists:get_value("AccessKeyId", Parsed),
        SecretKey = proplists:get_value("SecretAccessKey", Parsed),
        SecurityToken = proplists:get_value("Token", Parsed),
        Expiration = proplists:get_value("Expiration", Parsed),
        case AccessKey =:= undefined orelse SecretKey =:= undefined of
            true ->
                {error, missing_credential_fields};
            false ->
                Creds = #aws_credentials{
                    access_key = AccessKey,
                    secret_key = SecretKey,
                    security_token = SecurityToken,
                    expiration = parse_expiration(Expiration)
                },
                {ok, Creds}
        end
    catch
        _:_Reason ->
            %% Do not include the exception reason in the error tuple -- it may
            %% contain fragments of the response body (credential material, P0 R6).
            {error, json_decode_failed}
    end.

%% @doc Sanitize container credential error reasons for safe logging.
%%      Strips inner terms that could contain response body fragments or
%%      credential material (P0 R6). Only the error category atom is logged.
%% @end
sanitize_container_error({json_decode_failed, _}) -> json_decode_failed;
sanitize_container_error({http_error, Status, _Phrase}) -> {http_error, Status};
sanitize_container_error({transport_error, Reason}) -> {transport_error, Reason};
sanitize_container_error(response_body_too_large) -> response_body_too_large;
sanitize_container_error(missing_credential_fields) -> missing_credential_fields;
sanitize_container_error({malformed_url, _}) -> malformed_url;
sanitize_container_error(Other) when is_atom(Other) -> Other;
sanitize_container_error(_) -> unknown_error.

-spec parse_expiration(string() | undefined) -> calendar:datetime() | undefined.
%% @doc Parse the Expiration field if present, returning undefined when absent.
%% @end
parse_expiration(undefined) ->
    undefined;
parse_expiration(Timestamp) ->
    parse_iso8601_timestamp(Timestamp).

-spec with_metadata_connection(fun((aws_lib_httpc:conn()) -> Result)) -> Result.
%% @doc Execute a function with a shared metadata service connection
%% @end
with_metadata_connection(Fun) ->
    case aws_lib_uri:parse(instance_metadata_url("")) of
        {ok, Uri} ->
            Host = aws_lib_uri:host(Uri),
            Port = aws_lib_uri:port(Uri),
            case aws_lib_httpc:open(Host, Port, metadata_open_opts()) of
                {ok, Conn} ->
                    try
                        Fun(Conn)
                    after
                        aws_lib_httpc:close(Conn)
                    end;
                {error, _Reason} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% Open options shared by the EC2 metadata-service sites: plain TCP/HTTP to the
%% link-local IMDS host, with the short IMDS connect timeout.
metadata_open_opts() ->
    #{transport => tcp, protocols => [http], connect_timeout => 5000}.

-spec lookup_credentials_from_instance_metadata(aws_config()) ->
    {ok, aws_credentials(), aws_config()} | error().
%% @spec lookup_credentials_from_instance_metadata() -> Result.
%% @doc Attempt to lookup the values from the EC2 instance metadata service.
%% @end
lookup_credentials_from_instance_metadata(Config) ->
    with_metadata_connection(fun(ConnPid) ->
        case maybe_get_role_from_instance_metadata_with_conn(ConnPid, Config) of
            {ok, Role, Config1} ->
                maybe_get_credentials_from_instance_metadata_with_conn(ConnPid, Role, Config1);
            Error ->
                Error
        end
    end).

-spec lookup_region(
    Profile :: string(),
    Region :: false | string(),
    Config :: aws_config()
) ->
    {ok, string(), aws_config()} | {error, undefined}.
%% @doc If Region is false, lookup the region from the config or the EC2
%%      instance metadata service.
%% @end
lookup_region(Profile, false, Config) ->
    lookup_region_from_config(values(Profile), Config);
lookup_region(_, Region, Config) ->
    {ok, Region, Config}.

-spec lookup_region_from_config(
    Settings :: list() | {error, atom()},
    Config :: aws_config()
) -> {ok, string(), aws_config()} | {error, undefined}.
%% @doc Return the region from the local configuration file. If local config
%%      settings are not found, try to lookup the region from the EC2 instance
%%      metadata service.
%% @end
lookup_region_from_config({error, _}, Config) ->
    maybe_get_region_from_instance_metadata(Config);
lookup_region_from_config(Settings, Config) ->
    lookup_region_from_settings(proplists:get_value(region, Settings), Config).

-spec lookup_region_from_settings(any() | undefined, aws_config()) ->
    {ok, string(), aws_config()} | {error, undefined}.
%% @doc Decide if the region should be loaded from the Instance Metadata service
%%      of if it's already set.
%% @end
lookup_region_from_settings(undefined, Config) ->
    maybe_get_region_from_instance_metadata(Config);
lookup_region_from_settings(Region, Config) ->
    {ok, Region, Config}.

-spec maybe_convert_number(string()) -> integer() | float().
%% @doc Returns an integer or float from a string if possible, otherwise
%%      returns the string().
%% @end
maybe_convert_number(Value) ->
    Stripped = string:strip(Value),
    case string:to_float(Stripped) of
        {error, no_float} ->
            try
                list_to_integer(Stripped)
            catch
                error:badarg -> Stripped
            end;
        {F, _Rest} ->
            F
    end.

-spec maybe_get_credentials_from_instance_metadata_with_conn(
    ConnPid :: aws_lib_httpc:conn(),
    Role :: string(),
    aws_config()
) -> {'ok', aws_credentials(), aws_config()} | error().
%% @doc Try to query the EC2 local instance metadata service to get temporary
%%      authentication credentials using an existing connection.
%% @end
maybe_get_credentials_from_instance_metadata_with_conn(ConnPid, Role, Config) ->
    case aws_lib_uri:parse(instance_credentials_url(Role)) of
        {ok, Uri} ->
            Path = aws_lib_uri:target(Uri),
            case perform_http_get_with_conn(ConnPid, Path, Config) of
                {ok, Result, Config1} ->
                    case parse_credentials_response({ok, Result}) of
                        {ok, Creds} ->
                            {ok, Creds, Config1};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec maybe_get_region_from_instance_metadata(aws_config()) ->
    {ok, Region :: string(), Config :: aws_config()} | {error, Reason :: atom()}.
%% @doc Try to query the EC2 local instance metadata service to get the region
%% @end
maybe_get_region_from_instance_metadata(Config) ->
    URL = instance_availability_zone_url(),
    case perform_http_get_instance_metadata(URL, Config) of
        {ok, Result, Config1} ->
            case parse_az_response({ok, Result}) of
                {ok, Region} ->
                    {ok, Region, Config1};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec perform_http_get_with_conn(aws_lib_httpc:conn(), string(), aws_config()) ->
    {ok, {any(), any(), any()}, aws_config()} | {error, term()}.
%% @doc Make HTTP GET request using existing Gun connection
%% @end
perform_http_get_with_conn(Conn, Path, Config) ->
    {ok, Headers, Config1} = instance_metadata_request_headers(Config),
    case aws_lib_httpc:request(Conn, get, Path, Headers, <<>>, ?DEFAULT_IMDS_TIMEOUT) of
        {ok, Response} ->
            {ok, Response, Config1};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Try to query the EC2 local instance metadata service to get the role
%%      assigned to the instance using an existing connection.
%% @end
-spec maybe_get_role_from_instance_metadata_with_conn(aws_lib_httpc:conn(), aws_config()) ->
    {ok, string(), aws_config()} | error().
maybe_get_role_from_instance_metadata_with_conn(ConnPid, Config) ->
    case aws_lib_uri:parse(instance_role_url()) of
        {ok, Uri} ->
            Path = aws_lib_uri:target(Uri),
            case perform_http_get_with_conn(ConnPid, Path, Config) of
                {ok, Result, Config1} ->
                    case parse_body_response({ok, Result}) of
                        {ok, Body} ->
                            {ok, Body, Config1};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Parse the response from the Availability Zone query to the
%%      Instance Metadata service, returning the Region if successful.
%% end.
parse_az_response({ok, {{_, 200, _}, _, Body}}) when is_binary(Body) ->
    {ok, region_from_availability_zone(binary_to_list(Body))};
parse_az_response({ok, {{_, _, _}, _, _}}) ->
    {error, undefined}.

%% @doc Parse the return response from the Instance Metadata Service where the
%%      body value is the string to process.
%% end.
parse_body_response({ok, {{_, 200, _}, _, Body}}) when is_binary(Body) ->
    {ok, binary_to_list(Body)};
parse_body_response({ok, {{_, 401, _}, _, _}}) ->
    ?LOG_ERROR(
        get_instruction_on_instance_metadata_error(
            "Unauthorized instance metadata service request."
        )
    ),
    {error, undefined};
parse_body_response({ok, {{_, 403, _}, _, _}}) ->
    ?LOG_ERROR(
        get_instruction_on_instance_metadata_error(
            "The request is not allowed or the instance metadata service is turned off."
        )
    ),
    {error, undefined};
parse_body_response(_) ->
    {error, undefined}.

-spec parse_credentials_response({ok, {tuple(), headers(), body()}}) ->
    {ok, aws_credentials()} | {error, undefined}.
%% @doc Try to query the EC2 local instance metadata service to get the role
%%      assigned to the instance.
%% @end
parse_credentials_response({ok, {{_, 404, _}, _, _}}) ->
    {error, undefined};
parse_credentials_response({ok, {{_, 200, _}, _, Body}}) ->
    Parsed = aws_lib_json:decode(Body),
    Creds = #aws_credentials{
        access_key = proplists:get_value("AccessKeyId", Parsed),
        secret_key = proplists:get_value("SecretAccessKey", Parsed),
        security_token = proplists:get_value("Token", Parsed),
        expiration = parse_iso8601_timestamp(proplists:get_value("Expiration", Parsed))
    },
    {ok, Creds}.

%% @doc Wrap httpc:get/4 to simplify Instance Metadata service v2 requests
%% @end
perform_http_get_instance_metadata(URL, Config) ->
    ?LOG_DEBUG("Querying instance metadata service: ~tp", [URL]),
    % Parse metadata service URL
    case aws_lib_uri:parse(URL) of
        {ok, Uri} ->
            perform_http_get_instance_metadata_conn(Uri, Config);
        {error, _} = Error ->
            Error
    end.

perform_http_get_instance_metadata_conn(Uri, Config) ->
    Host = aws_lib_uri:host(Uri),
    Port = aws_lib_uri:port(Uri),
    Path = aws_lib_uri:target(Uri),
    {ok, Headers, Config1} = instance_metadata_request_headers(Config),
    Opts = maps:put(timeout, ?DEFAULT_IMDS_TIMEOUT, metadata_open_opts()),
    case aws_lib_httpc:request(Host, Port, get, Path, Headers, <<>>, Opts) of
        {ok, Response} ->
            {ok, Response, Config1};
        {error, _Reason} = Error ->
            Error
    end.

-spec get_instruction_on_instance_metadata_error(string()) -> string().
%% @doc Return error message on failures related to EC2 Instance Metadata Service with a reference to AWS document.
%% end
get_instruction_on_instance_metadata_error(ErrorMessage) ->
    ErrorMessage ++
        " Please refer to the AWS documentation for details on how to configure the instance metadata service: "
        "https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configuring-instance-metadata-service.html.".

-spec parse_iso8601_timestamp(Timestamp :: string() | binary()) -> calendar:datetime().
%% @doc Parse a ISO8601 timestamp, returning a datetime() value.
%% @end
parse_iso8601_timestamp(Timestamp) when is_binary(Timestamp) ->
    parse_iso8601_timestamp(binary_to_list(Timestamp));
parse_iso8601_timestamp(Timestamp) ->
    [Date, Time] = string:tokens(Timestamp, "T"),
    [Year, Month, Day] = string:tokens(Date, "-"),
    [Hour, Minute, Second] = string:tokens(Time, ":"),
    {{list_to_integer(Year), list_to_integer(Month), list_to_integer(Day)}, {
        list_to_integer(Hour), list_to_integer(Minute), list_to_integer(string:left(Second, 2))
    }}.

-spec profile() -> string().
%% @doc Return the value of the AWS_DEFAULT_PROFILE environment variable or the
%%      "default" profile.
%% @end
profile() -> profile(os:getenv("AWS_DEFAULT_PROFILE")).

-spec profile(false | string()) -> string().
%% @doc Process the value passed in to determine if we will return the default
%%      profile or the value from the environment variable.
%% @end
profile(false) -> ?DEFAULT_PROFILE;
profile(Value) -> Value.

-spec read_file(string()) -> {'ok', [binary()]} | {error, Reason :: atom()}.
%% @doc Read the specified file, returning the contents as a list of strings.
%% @end
read_file(Path) ->
    case file:read_file(Path) of
        {ok, Binary} ->
            {ok, re:split(Binary, <<"\r\n|\n">>, [{return, binary}])};
        {error, _} = Error ->
            Error
    end.

%% @doc Strip the availability zone suffix from the region.
%% @end
region_from_availability_zone(Value) ->
    string:sub_string(Value, 1, length(Value) - 1).

-spec load_imdsv2_token() -> security_token().
%% @doc Attempt to obtain EC2 IMDSv2 token.
%% @end
load_imdsv2_token() ->
    TokenUrl = imdsv2_token_url(),
    ?LOG_INFO("Attempting to obtain EC2 IMDSv2 token from ~tp ...", [TokenUrl]),
    % Parse metadata service URL
    case aws_lib_uri:parse(TokenUrl) of
        {ok, Uri} ->
            load_imdsv2_token(Uri);
        {error, Reason} ->
            %% A malformed token URL is a fallback like any other IMDSv2 failure:
            %% log and return undefined so credential loading uses IMDSv1.
            ?LOG_WARNING(
                get_instruction_on_instance_metadata_error(
                    "Failed to parse EC2 IMDSv2 token URL: ~tp. "
                    "Falling back to EC2 IMDSv1 for now. It is recommended to use EC2 IMDSv2."
                ),
                [Reason]
            ),
            undefined
    end.

load_imdsv2_token(Uri) ->
    Host = aws_lib_uri:host(Uri),
    Port = aws_lib_uri:port(Uri),
    Path = aws_lib_uri:target(Uri),
    % PUT request with IMDSv2 token TTL header
    Headers = [{?METADATA_TOKEN_TTL_HEADER, integer_to_list(?METADATA_TOKEN_TTL_SECONDS)}],
    Opts = maps:put(timeout, ?DEFAULT_IMDS_TIMEOUT, metadata_open_opts()),
    case aws_lib_httpc:request(Host, Port, put, Path, Headers, <<>>, Opts) of
        {ok, {{_, 200, _}, _RespHeaders, Body}} ->
            ?LOG_DEBUG("Successfully obtained EC2 IMDSv2 token."),
            binary_to_list(Body);
        {ok, {{_, 400, _}, _RespHeaders, _Body}} ->
            ?LOG_WARNING(
                "Failed to obtain EC2 IMDSv2 token: Missing or Invalid Parameters – The PUT request is not valid."
            ),
            undefined;
        Other ->
            ?LOG_WARNING(
                get_instruction_on_instance_metadata_error(
                    "Failed to obtain EC2 IMDSv2 token: ~tp. "
                    "Falling back to EC2 IMDSv1 for now. It is recommended to use EC2 IMDSv2."
                ),
                [Other]
            ),
            undefined
    end.

-spec instance_metadata_request_headers(Config :: aws_config()) -> {ok, headers(), aws_config()}.
%% @doc Return headers used for instance metadata service requests.
%% @end
instance_metadata_request_headers(Config) ->
    case application:get_env(aws, aws_prefer_imdsv2) of
        {ok, false} ->
            {ok, [], Config};
        %% undefined or {ok, true}
        _ ->
            ?LOG_DEBUG("EC2 Instance Metadata Service v2 (IMDSv2) is preferred."),
            maybe_imdsv2_token_headers(Config)
    end.

-spec maybe_imdsv2_token_headers(Config :: aws_config()) -> {ok, headers(), aws_config()}.
%% @doc Construct http request headers from Imdsv2Token to use with GET requests submitted to the EC2 Instance Metadata Service.
%% @end
maybe_imdsv2_token_headers(Config) ->
    case Config of
        #aws_config{imdsv2_token = #imdsv2token{token = Value}} when Value =/= undefined ->
            case aws_lib:expired_imdsv2_token(Config#aws_config.imdsv2_token) of
                false ->
                    {ok, [{?METADATA_TOKEN, Value}], Config};
                true ->
                    % Token expired, load new one
                    case load_imdsv2_token() of
                        undefined ->
                            {ok, [], Config};
                        NewValue ->
                            Expiration =
                                calendar:datetime_to_gregorian_seconds(aws_lib:local_time()) +
                                    ?METADATA_TOKEN_TTL_SECONDS,
                            NewToken = #imdsv2token{token = NewValue, expiration = Expiration},
                            Config1 = Config#aws_config{imdsv2_token = NewToken},
                            {ok, [{?METADATA_TOKEN, NewValue}], Config1}
                    end
            end;
        _ ->
            % No token yet, try to load one
            case load_imdsv2_token() of
                undefined ->
                    {ok, [], Config};
                Value ->
                    Expiration =
                        calendar:datetime_to_gregorian_seconds(aws_lib:local_time()) +
                            ?METADATA_TOKEN_TTL_SECONDS,
                    NewToken = #imdsv2token{token = Value, expiration = Expiration},
                    Config1 = Config#aws_config{imdsv2_token = NewToken},
                    {ok, [{?METADATA_TOKEN, Value}], Config1}
            end
    end.
