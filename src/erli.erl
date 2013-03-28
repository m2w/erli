%% @author Moritz Windelen <moritz@tibidat.com>
% @copyright 2012-2013 Moritz Windelen.
%% @doc erli startup code

-module(erli).
-author('Moritz Windelen <moritz@tibidat.com>').

%% API
-export([ensure_deps_started/0, start/0, stop_deps/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts all dependencies and then starts erli.
-spec start() -> ok | {error, Reason :: term()}.
start() ->
    ensure_deps_started(),
    application:start(erli).

%% @doc Starts all applications that erli depends upon
-spec ensure_deps_started() -> ok.
ensure_deps_started() ->
    ensure_started(sasl),
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mnesia),
    ensure_started(mochiweb),
    ensure_started(webmachine),
    ensure_started(egeoip).

%% @doc Stops all applications that erli depends on
-spec stop_deps() -> ok | {error, Reason :: term()}.
stop_deps() ->
    application:stop(egeoip),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(mnesia),
    application:stop(crypto),
    application:stop(inets).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Attempts to start the application and returns `ok', also returns
%% `ok' if the application is already running.
-spec ensure_started(App :: atom()) -> ok.
ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
