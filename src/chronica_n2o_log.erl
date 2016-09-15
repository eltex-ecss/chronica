%%%----------------------------------------------------------------------------
%%% @author platinumthinker <platinumthinker@gmail.com>
%%% @doc
%%% Logging module for n2o
%%% Add in you sys.config:
%%% {n2o,
%%%  [
%%%   {log_modules, chronica_n2o_log}
%%%  ]}
%%% @end
%%%----------------------------------------------------------------------------

-module(chronica_n2o_log).
-include("chronica.hrl").

-export([
    log_level/0,
    log_modules/0,
    info/3,
    warning/3,
    error/3
    ]).


log_level() -> warning.
log_modules() -> any.

info(Module, String, _Args) ->
    NString = io_lib:format("~p:~s", [Module, String]),
    log:info(NString, _Args).
warning(Module, String, _Args) ->
    NString = io_lib:format("~p:~s", [Module, String]),
    log:warning(NString, _Args).
error(Module, String, _Args) ->
    NString = io_lib:format("~p:~s", [Module, String]),
    log:error(NString, _Args).
