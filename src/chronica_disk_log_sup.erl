%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% Current module based on disk_log distributed by Ericsson AB
%%%-------------------------------------------------------------------
-module(chronica_disk_log_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link()->
    supervisor:start_link({local, chronica_disk_log_sup}, chronica_disk_log_sup, []).

init([]) ->
    SupFlags = {simple_one_for_one, 4, 3600},
    Child = {chronica_disk_log, {chronica_disk_log, istart_link, []}, temporary,
         1000, worker, [chronica_disk_log]},
    {ok, {SupFlags, [Child]}}.