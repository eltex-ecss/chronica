%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin, Nikita Roshchupkin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_output).

-include("chronica_int.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").

-export([log_it/10, format_sys_dbg/3]).

log_it(_Priority, _NowTime, _Format, _Args, _Module, _Line, _File, _Function, _Pid, _Flow = []) ->
    ok;
log_it(Priority, NowTime, Format, Args, Module, Line, File, Function, Pid, Flow) ->
    try
        {Date, Time} = calendar:now_to_local_time(NowTime),
        Mk = element(3, NowTime),
        put_to_outputs({{Date, Time, Mk}, Priority, Module, Pid, Line, File, Function, Format, Args}, default, Flow)
    catch
        C:E ->
            ?INT_EXCEPT("Exception: io_lib:format ~p:~p module: ~p line: ~p", [C, E, Module, Line])
    end.

put_to_outputs({Time, Priority, Module, Pid, Line, File, Function, F, A} = Params,
               UserStr, [#flow_handle{id = Handler, format_type = Format} | Tail]) ->
    {FString, NewUserStr, TypeFormat} =
        case {Format, UserStr} of
            {binary, _} -> {{F, A}, UserStr, binary};
            {_, default} -> Tmp = io_lib:format(F, A), {Tmp, Tmp, default};
            {_, _} -> {UserStr, UserStr, default}
        end,

    FormatedStr = chronica_format:Format({Time, Priority, Module, Pid, Line, File, Function, FString}),
    ?INT_DBG("Write message to output ~p", [Handler]),
    chronica_gen_backend:write(Handler, {FormatedStr, TypeFormat}),
    put_to_outputs(Params, NewUserStr, Tail);

put_to_outputs(_ , _, []) ->
    ok;

put_to_outputs(Params, DefaultStr, Flow) ->
    ?INT_ERR("Unhandled params in put_to_outputs: ~p, ~p, ~p", [Params, DefaultStr, Flow]),
    ok.

format_sys_dbg(Name, {in, {'$gen_event', Event}}, {_, StateName}) ->
    io_lib:format("*DBG* ~p got event ~p in state ~w~n", [Name, Event, StateName]);
format_sys_dbg(Name, {in, {'$gen_all_state_event', Event}}, {_, StateName}) ->
    io_lib:format("*DBG* ~p got all_state_event ~p in state ~w~n", [Name, Event, StateName]);
format_sys_dbg(Name, {in, {timeout, Ref, {'$gen_timer', Message}}}, {_, StateName}) ->
    io_lib:format("*DBG* ~p got timer ~p in state ~w~n", [Name, {timeout, Ref, Message}, StateName]);
format_sys_dbg(Name, {in, {timeout, _Ref, {'$gen_event', Event}}}, {_, StateName}) ->
    io_lib:format("*DBG* ~p got timer ~p in state ~w~n", [Name, Event, StateName]);
format_sys_dbg(Name, {in, Msg}, {_, StateName}) ->
    io_lib:format("*DBG* ~p got ~p in state ~w~n", [Name, Msg, StateName]);
format_sys_dbg(Name, return, {_, StateName}) ->
    io_lib:format("*DBG* ~p switched to state ~w~n", [Name, StateName]);
format_sys_dbg(Name, {in, {'$gen_call', {From, _Tag}, Call}}, _) ->
    io_lib:format("*DBG* ~p got call ~p from ~w~n", [Name, Call, From]);
format_sys_dbg(Name, {in, {'$gen_cast', Cast}}, _) ->
    io_lib:format("*DBG* ~p got cast ~p~n", [Name, Cast]);
format_sys_dbg(Name, {in, Msg}, _) ->
    io_lib:format("*DBG* ~p got ~p~n", [Name, Msg]);

format_sys_dbg(Name, {out, Msg, To, State}, _) ->
    io_lib:format("*DBG* ~p sent ~p to ~w, new state ~w~n", [Name, Msg, To, State]);

format_sys_dbg(Name, {noreply, State}, _) ->
    io_lib:format("*DBG* ~p new state ~w~n", [Name, State]);

format_sys_dbg(Name, {notify, Event}, _) ->
    io_lib:format("*DBG* ~p got event ~p~n", [Name, Event]);

format_sys_dbg(Name, {_, _, {call, Handler, Query}}, _) ->
    io_lib:format("*DBG* ~p(~p) got call ~p~n", [Name, Handler, Query]);

format_sys_dbg(Name, Event, _) ->
    io_lib:format("*DBG* ~p dbg  ~p~n", [Name, Event]).
