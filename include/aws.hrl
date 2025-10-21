-include_lib("kernel/include/logger.hrl").

-define(AWS_LOG_DEBUG(Arg),
    ?LOG_DEBUG("~tp: ~tp", [?MODULE, Arg])
).

-define(AWS_LOG_DEBUG(Fmt, Args),
    ?LOG_DEBUG("~tp: " ++ Fmt, [?MODULE | Args])
).

-define(AWS_LOG_WARNING(Arg),
    ?LOG_WARNING("~tp: ~ts", [?MODULE, Arg])
).

-define(AWS_LOG_WARNING(Fmt, Args),
    ?LOG_WARNING("~tp: " ++ Fmt, [?MODULE | Args])
).

-define(AWS_LOG_ERROR(Arg),
    ?LOG_ERROR("~tp: ~tp", [?MODULE, Arg])
).

-define(AWS_LOG_ERROR(Fmt, Args),
    ?LOG_ERROR("~tp: " ++ Fmt, [?MODULE | Args])
).

-define(AWS_LOG_INFO(Arg),
    ?LOG_INFO("~tp: ~ts", [?MODULE, Arg])
).

-define(AWS_LOG_INFO(Fmt, Args),
    ?LOG_INFO("~tp: " ++ Fmt, [?MODULE | Args])
).

-define(UNPROCESSABLE_ENTITY, 422).
