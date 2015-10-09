%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-define(DBG(Format),
        log:debug(Format)).
-define(DBG(Format, Args),
        log:debug(Format, Args)).
-define(DEBUG(Format),
        log:debug(Format)).
-define(DEBUG(Format, Args),
        log:debug(Format, Args)).

-define(TRACE(Format),
        log:trace(Format)).
-define(TRACE(Format, Args),
        log:trace(Format, Args)).

-define(INFO(Format),
        log:info(Format)).
-define(INFO(Format, Args),
        log:info(Format, Args)).

-define(WARN(Format),
        log:warning(Format)).
-define(WARN(Format, Args),
        log:warning(Format, Args)).

-define(ERR(Format),
        log:error(Format)).
-define(ERR(Format, Args),
        log:error(Format, Args)).

-define(ERROR(Format),
        log:error(Format)).
-define(ERROR(Format, Args),
        log:error(Format, Args)).

-define(TODO(Format),
        log:todo(Format)).

-define(DBG_TEST(Format),
        ?DBG(Format, Args)).
-define(DBG_TEST(Format, Args),
        ?DBG(Format, Args)).

% Capturing dbg from generic modules
% instead of
%   sys:trace(IHPid, true);
% try this:
%   sys:install(oct_ihO, { ?SYS_DBG_FUN , []})
-define(SYS_DBG_FUN,
        fun(FuncState, Event, ProcState) -> ?DBG("*DBG* "++?MODULE_STRING++" got event ~p", [Event]), FuncState end).
% or just call this macro after start_link
% where Name is registered name of gen_server(gen_event,gen_fsm,...)
-define(CAPTURE_SYS_DBG(Name),
        catch sys:install(Name, {?SYS_DBG_FUN, []})).

