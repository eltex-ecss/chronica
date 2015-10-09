%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_status).

-include_lib("pt_scripts/include/pt_recompilable.hrl").

-export([
        configured/0
        ]).


-spec configured() -> boolean().
%% NOTE: changes when processed with parsetransform
configured() -> false.