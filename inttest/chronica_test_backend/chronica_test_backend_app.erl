-module(chronica_test_backend_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    chronica_test_backend:test().

stop(_State) ->
    ok.