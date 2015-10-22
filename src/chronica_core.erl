%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_core).

-include("chronica_int.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").
-include_lib("pt_scripts/include/pt_recompilable.hrl").

-export(
        [
            sync_fast_log/1,
            unwrap_param/1,
            sys_dbg_on/2, sys_dbg_off/1,
            log_fast/9
        ]).

unwrap_param(Param) when is_function(Param, 0) ->
    try
        Param()
    catch
        _:E -> ?INT_EXCEPT("Exception while processing user's fun: ~p", [E]), throw({bad_user_fun, Param})
    end;
unwrap_param(Param) ->
    Param.

sync_fast_log({fast_log_message, Priority, Format, Args, ModuleName, Line, File, Function, undefined, undefined, IfaceModule, Key}) ->
    sync_fast_log({fast_log_message, Priority, Format, Args, ModuleName, Line, File, Function, self(), os:timestamp(), IfaceModule, Key});

sync_fast_log({fast_log_message, Priority, Format, Args, ModuleName, Line, File, Function, Pid, NowTime, IfaceModule, Key} = Msg) ->
    try
        case Priority of
            ?P_ERROR -> cycled_events:add_event(chronica_error_buffer, Msg);
            _ -> ok
        end,

        case IfaceModule:get_flows(Key, Priority) of
            not_found ->
                ?INT_ERR("Flows for (~p, ~p) are not found", [Key, Priority]);
            [] -> ok;
            Flows when is_list(Flows) ->
                chronica_output:log_it(Priority, NowTime, unwrap_param(Format), unwrap_param(Args), ModuleName, Line, File, Function, Pid, Flows);
            BadReturn ->
                ?INT_ERR("get_flows(~p, ~p) returned ~p", [Key, Priority, BadReturn])
        end
    catch
        _:E -> ?INT_ERR("Exception(~p) while logging: ~p", [E, Msg])
    end.

log_fast(Iface, Priority, Tags, Module, Line, File, Function, Format, Args) ->
    try
        Iface:log_fast(Priority, Tags, Module, Line, File, Function, Format, Args, undefined, undefined)
    catch
        error:undef ->
            {_ModName, AppName} = find_application(Iface),
            ?INT_ERR("Module ~p(~p) try to log, but application ~p isn't registered on chronica", [Module, Line, AppName])
    end,
    ok.

sys_dbg_on(ProcName, ModuleName) ->
    catch sys:install(ProcName, {fun sys_dbg_handler/3, ModuleName}).

sys_dbg_off(ProcName) ->
    catch sys:remove(ProcName, fun sys_dbg_handler/3).

sys_dbg_handler(State, Event, ProcState) ->
    log:log(?P_DEBUG, State, State, 0, chronica_output:format_sys_dbg(State, Event, ProcState), []),
    State.

find_application(LogIfaceModule) ->
    case atom_to_list(LogIfaceModule) of
        "chronica_iface_" ++ ModuleName ->
            Apps = application:loaded_applications(),
            {ModuleName, app_search(erlang:list_to_atom(ModuleName), Apps)};
        _ -> {unknown, unknown}
    end.

app_search(_ModuleName, []) -> unknown;
app_search(ModuleName, [{Name, _, _} = App | Tail]) ->
    case application:get_key(Name, modules) of
        {ok, List} ->
            case lists:member(ModuleName, List) of
                true -> App;
                false -> app_search(ModuleName, Tail)
            end;
        _ ->
            app_search(ModuleName, Tail)
    end.
