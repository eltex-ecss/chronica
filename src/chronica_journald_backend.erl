%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Dylgyrzhapov Buin
%%% @copyright (C) 2016, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_journald_backend).

-compile(inline_list_funcs).
-compile(inline).

-behaviour(chronica_gen_backend).
-behaviour(gen_server).

-include("chronica_int.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").

-export([
    handle_open/3,
    handle_close/1,
    handle_write/4,
    handle_clear/1,
    handle_rotate/1,
    handle_check/1
    ]).

-export([
    start/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
    ]).

%%TODO: Буин, нужно сделать константы переопределяемыми через application env
-define(max_mailbox_len, 1000).
-define(wait_before_restart, 1000).
-define(wait_for_old_proc, 100000).

-record(s, {
    nodename
    }).

handle_open(_, _, Files) ->
    case start() of
        {ok, Handle} ->
            {ok, Handle, Files};
        Other ->
            Other
    end.

handle_write(Handle, Data, TypeFormat, Params) ->
    gen_server:cast(Handle, {write, Data, TypeFormat, Params}).

handle_close(Handle) ->
    gen_server:cast(Handle, {close, normal}),
    ok.

handle_clear(_Handle) ->
    ok.

handle_rotate(_Handle) -> ok.

handle_check(Handle) when is_atom(Handle) ->
    erlang:whereis(Handle) =/= undefined;

handle_check(Handle) when is_pid(Handle) ->
    erlang:is_process_alive(Handle);

handle_check(_Handle) ->
    false.

start() ->
    case gen_server:start({local, ?MODULE}, ?MODULE, [], []) of
        {ok, _Pid} -> {ok, ?MODULE};
        ignore -> {error, ignore};
        {error, {already_started, _Pid}} -> {ok, ?MODULE};
        {error, Reason} -> {error, Reason}
    end.

init(_) ->
    {ok, #s{nodename = erlang:atom_to_binary(erlang:node(), utf8)}}.

handle_cast({write, Data, binary, _Params}, State = #s{nodename = Node}) ->
    Formated = erlang:binary_to_term(Data),
    {_Time, Priority, Module, Pid, Line, _File, Func, {F,A}} = Formated,
    Msg = unicode:characters_to_binary(io_lib:format(F,A)),
    journald_api:sendv(journald_tags(Msg, Priority, Module, Func, Line, Pid, Node)),
    check_overload(State);

handle_cast({write, _Str, _TypeFormat, Params}, State = #s{nodename = Node}) ->
    {_Time, Priority, Module, Pid, Line, _File, Func, F, A} = Params,
    Msg = unicode:characters_to_binary(io_lib:format(F,A)),
    journald_api:sendv(journald_tags(Msg, Priority, Module, Func, Line, Pid, Node)),
    check_overload(State);

handle_cast({close, Reason}, State) ->
    {stop, Reason, State};

handle_cast(_Unknown, State) ->
    ?INT_ERR("Unhandled cast request ~p", [_Unknown]),
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    ?INT_ERR("Unhandled call request~p", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    ?INT_ERR("Unhandled info ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

check_overload(State) ->
    {message_queue_len, QLen} = process_info(erlang:self(), message_queue_len),
    if
        QLen > ?max_mailbox_len ->
            ?INT_ERR("~n~nTTY is overloaded, skip messages...~n", []),
            restart_tty(),
            {stop, normal, State};
        true -> {noreply, State}
    end.

restart_tty() ->
    Self = erlang:self(),
    erlang:spawn(
        fun () ->
            timer:sleep(?wait_before_restart),
            Ref = erlang:monitor(process, Self),
            receive
                {'DOWN', Ref, _, _, _} -> start()
            after
                ?wait_for_old_proc ->
                    ?INT_ERR("Cant restart tty output cause no DOWN from old proc", []),
                    {error, restart_timeout}
            end
        end).

map_priority(?P_ERROR) -> <<"3">>;
map_priority(?P_WARNING) -> <<"4">>;
map_priority(?P_INFO) -> <<"6">>;
map_priority(?P_TRACE) -> <<"5">>;
map_priority(?P_DEBUG) -> <<"7">>;
map_priority(Other) -> Other.

journald_tags(Msg, Priority, Module, Func, Line, Pid, Node) ->
    [
    {<<"MESSAGE">>, [Msg]},
    {<<"PRIORITY">>, map_priority(Priority)},
    {<<"CODE_FILE">>, Module},
    {<<"CODE_FUNC">>, Func},
    {<<"CODE_LINE">>, Line},
    {<<"PROCESS_PID">>, Pid},
    {<<"SYSLOG_IDENTIFIER">>, Node}
    ].

