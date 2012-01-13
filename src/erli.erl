%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011-2012 Moritz Windelen.

%% @doc erli startup code

-module(erli).
-author('Moritz Windelen <moritz@tibidat.com>').

-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
%%------------------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
%% @end
%%------------------------------------------------------------------------------
start_link() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    ensure_started(mnesia),
    erli_storage:init({}), % initialize mnesia
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    erli_sup:start_link().

%%------------------------------------------------------------------------------
%% @spec start() -> ok
%% @doc Start the erli server.
%% @end
%%------------------------------------------------------------------------------
start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    ensure_started(mnesia),
    erli_storage:init({}), % initialize mnesia
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(erli).

%%------------------------------------------------------------------------------
%% @spec stop() -> ok
%% @doc Stop the erli server.
%% @end
%%------------------------------------------------------------------------------
stop() ->
    Res = application:stop(erli),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    application:stop(mnesia),
    Res.
