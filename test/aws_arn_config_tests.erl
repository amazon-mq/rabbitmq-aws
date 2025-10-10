-module(aws_arn_config_tests).

-include_lib("eunit/include/eunit.hrl").

parse_arn_s3_test() ->
    Arn = "arn:aws:s3:::private-ca-42/cacertfile.pem",
    {ok, Parsed} = aws_arn_util:parse_arn(Arn),
    ?assertEqual("aws", maps:get(partition, Parsed)),
    ?assertEqual("s3", maps:get(service, Parsed)),
    ?assertEqual("", maps:get(region, Parsed)),
    ?assertEqual("", maps:get(account, Parsed)),
    ?assertEqual("private-ca-42/cacertfile.pem", maps:get(resource, Parsed)).

parse_arn_s3_nested_path_test() ->
    Arn = "arn:aws:s3:::my-bucket/path/to/cert.pem",
    {ok, Parsed} = aws_arn_util:parse_arn(Arn),
    ?assertEqual("my-bucket/path/to/cert.pem", maps:get(resource, Parsed)).

parse_arn_invalid_test() ->
    ?assertMatch({error, {invalid_arn_format, _}}, aws_arn_util:parse_arn("invalid")).

parse_arn_empty_test() ->
    ?assertMatch({error, {invalid_arn_format, _}}, aws_arn_util:parse_arn("")).

parse_arn_incomplete_test() ->
    ?assertMatch({error, {invalid_arn_format, _}}, aws_arn_util:parse_arn("arn:aws:s3")).

replace_in_env_test() ->
    ExpectedOtherKeyValue = "value",
    Expected = [<<"cacertdata">>],
    ok = application:set_env(
        rabbitmq_auth_backend_oauth2,
        key_config,
        [{other_key, ExpectedOtherKeyValue}, {cacertfile, "/tmp/ca.pem"}]
    ),

    ok = aws_arn_env:replace(
        rabbitmq_auth_backend_oauth2, key_config, cacertfile, cacerts, {ok, Expected}
    ),

    {ok, KeyConfig} = application:get_env(rabbitmq_auth_backend_oauth2, key_config),
    ?assertMatch(Expected, proplists:get_value(cacerts, KeyConfig)),
    ?assertMatch(ExpectedOtherKeyValue, proplists:get_value(other_key, KeyConfig)).
