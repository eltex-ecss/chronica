%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_error_logger_handler).

-behaviour(gen_event).

-include_lib("pt_scripts/include/pt_macro.hrl").
-include("chronica.hrl").
-include("chronica_int.hrl").

-export(
   [
    init/1,
    handle_event/2,
    terminate/2,
    code_change/3,
    handle_call/2,
    handle_info/2
   ]).

init(_Args) -> {ok, []}.

handle_event(Event = {ReportType, _, _}, State) ->
    Priority = case ReportType of
                   error_report -> ?P_ERROR;
                   info_report -> ?P_INFO;
                   error -> ?P_ERROR;
                   info_msg -> ?P_INFO;
                   info -> ?P_INFO;
                   warning_msg -> ?P_WARNING;
                   warning_report -> ?P_WARNING;
                   _ -> ?P_INFO
               end,
    try
        Res = write_report(Event),
        case Priority of
            ?P_ERROR   -> log:error(?ERROR_LOGGER_LOG, Res, []);
            ?P_WARNING -> log:warning(?ERROR_LOGGER_LOG, Res, []);
            ?P_INFO    -> log:info(?ERROR_LOGGER_LOG, Res, [])
        end
    catch
        C:E ->
            ?INT_EXCEPT("Exception: ~p:~p", [C, E])
    end,
    {ok, State}.

write_report({error_report, _GL, {Pid, Type, Report}}) ->
    Head = write_head(Type, Pid),
    write_report2(Head, Type, Report);

write_report({info_report, _GL, {Pid, Type, Report}}) ->
    Head = write_head(Type, Pid),
    write_report2(Head, Type, Report);

write_report({warning_report, _GL, {Pid, Type, Report}}) ->
    Head = write_head(Type, Pid),
    write_report2(Head, Type, Report);

write_report({error, _GL, {_Pid, Format, Args}}) ->
    Head = write_head(error_report, _Pid),
    io_lib:format(Head ++ Format, Args);

write_report({info_msg, _GL, {_Pid, Format, Args}}) ->
    io_lib:format(Format, Args);

write_report({warning_msg, _GL, {_Pid, Format, Args}}) ->
    io_lib:format(Format, Args);

write_report(Event) ->
    io_lib:format("Bad formated error_logger event ~p", [Event]).

write_head(supervisor_report, Pid) ->
    write_head1("SUPERVISOR REPORT", Pid);
write_head(crash_report, Pid) ->
    write_head1("CRASH REPORT", Pid);
write_head(progress, Pid) ->
    write_head1("PROGRESS REPORT", Pid);
write_head(error_report, Pid) ->
    write_head1("ERROR REPORT", Pid);
write_head(std_info, Pid) ->
    write_head1("STD INFO", Pid);
write_head(std_error, Pid) ->
    write_head1("STD ERROR", Pid);
write_head(_Unknown, Pid) ->
    write_head1("UNKNOWN", Pid).

write_head1(Type, Pid) when node(Pid) /= node() ->
    io_lib:format("=~s==== (~p) ===~n",
    [Type, node(Pid)]);
write_head1(Type, _) ->
    io_lib:format("=~s=======~n",
    [Type]).

write_report2(Head, supervisor_report, Report) ->
    Name = sup_get(supervisor, Report),
    Context = sup_get(errorContext, Report),
    Reason = sup_get(reason, Report),
    Offender = sup_get(offender, Report),
    io_lib:format(Head ++ "     Supervisor: ~p~n     Context:    ~p~n"
                  "     Reason:     ~80.18p~n     Offender:   ~80.18p~n~n",
          [Name, Context, Reason, Offender]);
write_report2(Head, progress, Report) ->
    Format = format_key_val(Report),
    io_lib:format(Head ++ "~s", [Format]);
write_report2(Head, crash_report, Report) ->
    Format = proc_lib:format(Report),
    io_lib:format(Head ++ "~s", [Format]);
write_report2(Head, std_info, {What, Info}) ->
    io_lib:format(Head ++ "~p: ~p", [What, Info]);
write_report2(Head, std_error, {What, Info}) ->
    io_lib:format(Head ++ "~p: ~p", [What, Info]);
write_report2(Head, _Unknown, Report) ->
    io_lib:format(Head ++ "Unknown report: ~p", [Report]).


handle_call(_Request, State) ->
    ?INT_ERR("error_logger_handler: Unhandled call!!! ~p~n", [_Request]),
    {ok, ok, State}.

handle_info(_Info, State) ->
    ?INT_ERR("error_logger_handler: Unhandled info!!! ~p~n", [_Info]),
    {ok, State}.

terminate(_Args, _State) ->
    ?INT_DBG("*** error_logger_handler terminates!!!~p ~p~n", [_Args, _State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

sup_get(Tag, Report) ->
    case lists:keysearch(Tag, 1, Report) of
        {value, {_, Value}} ->
            Value;
        _ ->
            ""
    end.

format_key_val([{Tag, Data}]) ->
    io_lib:format("    ~16w: ~p",[Tag,Data]) ++ format_key_val([]);
format_key_val([{Tag, Data}|Rep]) ->
    io_lib:format("    ~16w: ~p~n",[Tag,Data]) ++ format_key_val(Rep);
format_key_val(_) ->
    [].
