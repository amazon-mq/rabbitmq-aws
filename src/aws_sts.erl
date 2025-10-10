-module(aws_sts).

-export([add_custom_headers/1]).

add_custom_headers(BaseHeaders) ->
    case application:get_env(aws, sts) of
        {ok, StsList} when is_list(StsList) ->
            case proplists:get_value(custom_headers, StsList) of
                CustomHeadersMap when is_map(CustomHeadersMap) ->
                    BaseHeaders ++ maps:to_list(CustomHeadersMap);
                _ ->
                    BaseHeaders
            end;
        _ ->
            BaseHeaders
    end.
