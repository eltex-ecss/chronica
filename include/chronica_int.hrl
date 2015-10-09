%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-define(MAX_ROTATION_FILE_SIZE, 1024 * 1024).
-define(MAX_ROTATION_FILE_NUM, 10).

-define(MAX_ERROR_BUFFER_LEN, 100).

-define(DEFAULT_PRIORITY, debug).
-define(DEFAULT_FLOW, [tty]).
-define(DEFAULT_FILTER_FUN, fun(_X) -> true end).
-define(DEFAULT_LOCAL_UDP_PORT, 0).

-define(ERROR_LOG, errors).
-define(ERROR_LOGGER_LOG, error_logger).

-define(LOG_HOME_DIR, "log").
-define(LOG_FILE_DEFAULT, "default").

-define(DEFAULT_FILE_CHECK_TIMEOUT, 10000).
-define(HIBERNATE_TIMEOUT, 10000).

-define(INT_LOG_MODULE(LogLevel, F, A),
    begin
        chronica_internal_logger:LogLevel(os:timestamp(), F, A,
                                          ?MODULE, ?FUNC_STRING, ?LINE)
    end).

-define(INT_ERR(F, A), ?INT_LOG_MODULE(log_error, F, A)).
-define(INT_WARN(F, A), ?INT_LOG_MODULE(log_warning, F, A)).
-define(INT_INFO(F, A), ?INT_LOG_MODULE(log_info, F, A)).
-define(INT_DBG(F, A), ?INT_LOG_MODULE(log_debug, F, A)).
-define(INT_EXCEPT(F, A), ?INT_LOG_MODULE(log_error, F ++ "\nStacktrace: ~p",
                        A ++ [ erlang:get_stacktrace() ])).

-define(P_NOTHING, 0).
-define(P_ERROR, 1).
-define(P_WARNING, 2).
-define(P_INFO, 3).
-define(P_TRACE, 4).
-define(P_DEBUG, 5).

-record(flow_handle,
{
    id :: term(),
    format_type :: atom(),
    output_module :: atom(),
    open_params :: term(),
    writer_options :: term()
}).