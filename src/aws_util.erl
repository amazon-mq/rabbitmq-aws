-module(aws_util).

-export([reset_aws_credentials/0]).

-spec reset_aws_credentials() -> ok.
reset_aws_credentials() ->
    ok = rabbitmq_aws:refresh_credentials().
