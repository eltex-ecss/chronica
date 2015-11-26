-module(chronica_test_backend_rt).
-export([files/0,
         run/1]).

-define(APP_SRC_FILE, "src/chronica_test_backend.app.src").


files() ->
    [
        {copy, "test.rebar.config", "rebar.config"},
        {copy, "test.sys.config", "release/files/sys.config"},
        {copy, "test.relx.config", "relx.config"},
        {copy, "test.relx.config", "relx.config"},
        {copy, "chronica_test_backend.erl", "src/chronica_test_backend.erl"},
        {copy, "chronica_test_backend_app.erl", "src/chronica_test_backend_app.erl"},
        {create, ?APP_SRC_FILE, app_src(chronica_test_backend, [])}
    ].

run(_Dir) ->
    {ok, _} = retest_sh:run("rebar compile", []),
    {ok, _} = retest_sh:run("relx", []),
    {ok, _} = retest_sh:run("./release/sella/bin/sella-0.0.1 console", []),
    ok.

%%
%% Generate the contents of a simple .app.src file
%%
app_src(Name, Modules) ->
    App_src = {application, Name,
           [{description, atom_to_list(Name)},
            {id, atom_to_list(Name)},
            {vsn, "1.0"},
            {modules, []},
            {registered, []},
            {included_applications,[]},
            {applications,[kernel,stdlib,chronica]},
            {mod, {chronica_test_backend_app,[]}}]},
    io_lib:format("~p.\n", [App_src]).