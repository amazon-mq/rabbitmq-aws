-module(aws_arn_util).

-ifdef(TEST).
-compile(export_all).
-else.
-export([resolve_arn/1]).
-endif.

-spec resolve_arn(string()) -> {ok, binary()} | {error, term()}.
resolve_arn(Arn) ->
    case parse_arn(Arn) of
        {ok, #{partition := "aws-debug", service := "file", resource := Resource}} ->
            {ok, _File} = file:read_file(Resource);
        {ok, #{service := "s3", resource := Resource}} ->
            aws_s3:fetch_object(Resource);
        {ok, #{service := "secretsmanager", region := Region}} ->
            aws_sms:fetch_secret(Arn, Region);
        {ok, #{service := "acm-pca", region := Region}} ->
            aws_acm_pca:fetch_certificate(Arn, Region);
        {ok, #{service := Service}} ->
            Reason = unsupported_service,
            {error, {Reason, Service}};
        {error, _} = Error ->
            Error
    end.

-spec parse_arn(string()) -> {ok, map()} | {error, term()}.
parse_arn(Arn) ->
    try
        % resource name in arn could contain ":" itself, therefore using parts.
        % eg: arn:aws:secretsmanager:us-east-1:12345678910:secret:mysecret
        case re:split(Arn, ":", [{parts, 6}, {return, list}]) of
            ["arn", Partition, Service, Region, Account, Resource] ->
                {ok, #{
                    partition => Partition,
                    service => Service,
                    region => Region,
                    account => Account,
                    resource => Resource
                }};
            UnexpectedMatch ->
                {error, {invalid_arn_format, UnexpectedMatch}}
        end
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.
