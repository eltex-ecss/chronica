%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_internal_logger).

-dialyzer({no_match, close/0}).

-export([init/1, close/0, log_error/6, log_warning/6, log_info/6, log_debug/6]).

-include_lib("pt_scripts/include/pt_pmodule.hrl").

-module_parameters([handle, file, tty]).

-type priority() :: debug | info | warning | error | none.

-define(PD, 0).
-define(PI, 1).
-define(PW, 2).
-define(PE, 3).
-define(PN, 4).

-spec init([{file, string(), {integer(), integer()}, priority()} | {tty, priority()}]) ->
    ok | {error, term()}.
init(Params) ->
    try
        chronica_config_validation:validate_internal_logger(Params),

        case pt_pmodule:is_inited(?MODULE) of
            true -> throw({error, already_inited});
            false -> ok
        end,

        MParams =
            case lists:keyfind(file, 1, Params) of
                {file, Filename, Sizes, Filter} ->
                    Handle = open_file(Filename, Sizes),
                    [Handle, get_p_num(Filter)];
                false ->
                    [null, get_p_num(none)]
            end,
        MParams2 =
            case lists:keyfind(tty, 1, Params) of
                {tty, FilterTTY} -> MParams ++ [get_p_num(FilterTTY)];
                false-> MParams ++ [get_p_num(none)]
            end,
        pt_pmodule:init(?MODULE, MParams2)
    catch
        throw:Err -> Err
    end.

-spec close() -> ok | {error, term()}.
close() ->
    Handle =
        try
            ?MPARAM(handle)
        catch
            _:_ -> null
        end,

    case Handle of
        null ->
            ok;
        _ ->
            disk_log:sync(Handle),
            disk_log:close(Handle)
    end,

    catch pt_pmodule:dispose(?MODULE).


log(P, Time, F, A, Module, Func, Line) ->
    try
        case {?MPARAM(file), ?MPARAM(tty)} of
            {P1, P2} when (P1 > P) and (P2 > P) -> ok;
            _ ->
                T = format(P, calendar:now_to_local_time(Time), F, A, Module, Func, Line),
                case (?MPARAM(tty) =< P) of
                    true -> io:format(T, []);
                    false -> ok
                end,
                case (?MPARAM(file) =< P) of
                    true -> disk_log:balog(?MPARAM(handle), T);
                    false -> ok
                end,
                ok
        end
    catch
        _:Err -> Err
    end.


-spec log_error(Time::term(), string(), [any()], atom(), string(), integer()) -> ok.

log_error(Time, F, A, Module, Func, Line) -> log(?PE, Time, F, A, Module, Func, Line).

-spec log_warning(Time::term(), string(), [any()], atom(), string(), integer()) -> ok.

log_warning(Time, F, A, Module, Func, Line) -> log(?PW, Time, F, A, Module, Func, Line).

-spec log_info(Time::term(), string(), [any()], atom(), string(), integer()) -> ok.

log_info(Time, F, A, Module, Func, Line) -> log(?PI, Time, F, A, Module, Func, Line).

-spec log_debug(Time::term(), string(), [any()], atom(), string(), integer()) -> ok.

log_debug(Time, F, A, Module, Func, Line) -> log(?PD, Time, F, A, Module, Func, Line).


open_file(Filename, Sizes = {S, N})
            when is_list(Filename), Filename =/= [], is_integer(S), is_integer(N), S > 0, N > 0 ->
    case filelib:ensure_dir(Filename) of
        ok -> ok;
        Err -> throw({ensure_dir_error, {Filename, Err}})
    end,

    LogName = erlang:list_to_atom(Filename),

    case disk_log:open([{name, LogName},
                                   {file, Filename},
                                   {format, external},
                                   {type, wrap},
                                   {size, Sizes}]) of
        {ok, Handle} ->
            Handle;
        {error, {name_already_open, Handle}} ->
            Handle;
        {repaired, Handle, _, _} ->
            Handle;
        {error, {size_mismatch, Cursize, NewSize} = Reason} ->
            case disk_log:open(
                        [{name, LogName},
                         {file, Filename},
                         {format, external},
                         {type, wrap},
                         {size, Cursize}]) of
                {Succ, Handle} when Succ =:= ok;
                                     Succ =:= repaired ->
                    case disk_log:change_size(Handle, NewSize) of
                        ok ->
                            Handle;
                        {error, Reason3} ->
                            io:format("internal_logger:init_file() File ~p successfully open, but cant change size cause: ~p~n", [Filename, Reason3]),
                            Handle
                    end;
                {error, Reason2} -> throw({open_file_error, {Reason, Reason2}});
                Err2 -> io:format("internal_logger:init_file() unhandled result of open ~p~n", [Err2]), throw({open_file_error, Err2})
            end;
        Error ->
            throw({open_file_error, Error})
    end;
open_file(Fn, Sz) ->
    throw({open_file_bad_params, {Fn, Sz}}).

format(?PE, {{Y, M, D}, {H, Mi, S}}, F, A, Module, Func, Line) ->
    io_lib:format("~2.10.0B.~2.10.0B.~4.10.0B ~2.10.0B:~2.10.0B:~2.10.0B "
                  "~p:~p: log *ERROR* (~s) " ++ F ++ "~n",
                  [D, M, Y, H, Mi, S, Module, Line, Func] ++ A);
format(?PW, {{Y, M, D}, {H, Mi, S}}, F, A, Module, Func, Line) ->
    io_lib:format("~2.10.0B.~2.10.0B.~4.10.0B ~2.10.0B:~2.10.0B:~2.10.0B "
                  "~p:~p: log WARNING (~s) " ++ F ++ "~n",
                  [D, M, Y, H, Mi, S, Module, Line, Func] ++ A);
format(?PI, {{Y, M, D}, {H, Mi, S}}, F, A, Module, _Func, Line) ->
    io_lib:format("~2.10.0B.~2.10.0B.~4.10.0B ~2.10.0B:~2.10.0B:~2.10.0B "
                  "~p:~p: log info: " ++ F ++ "~n",
                  [D, M, Y, H, Mi, S, Module, Line] ++ A);
format(?PD, {{Y, M, D}, {H, Mi, S}}, F, A, Module, Func, Line) ->
    io_lib:format("~2.10.0B.~2.10.0B.~4.10.0B ~2.10.0B:~2.10.0B:~2.10.0B "
                  "~p:~p: log debug (~s) " ++ F ++ "~n",
                  [D, M, Y, H, Mi, S, Module, Line, Func] ++ A).

get_p_num(debug) -> ?PD;
get_p_num(info) -> ?PI;
get_p_num(warning) -> ?PW;
get_p_num(error) -> ?PE;
get_p_num(none) -> ?PN.
