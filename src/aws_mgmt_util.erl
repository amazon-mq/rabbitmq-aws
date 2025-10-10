-module(aws_mgmt_util).

-export([unprocessable_entity/3]).

unprocessable_entity(Reason, ReqData, Context) ->
    halt_response(422, unprocessable_entity, Reason, ReqData, Context).

halt_response(Code, Type, Reason, ReqData, Context) ->
    rabbit_web_dispatch_access_control:halt_response(
        Code,
        Type,
        Reason,
        ReqData,
        Context
    ).
