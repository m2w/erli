%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2012-2013 Moritz Windelen.
%% @doc Callbacks for the erli application.

-module(erli_app).
-author('Moritz Windelen <moritz@tibidat.com>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @private
%% @doc application start callback for erli.
-spec start(application:start_type(),
	    StartArgs :: term()) ->
		   {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason :: term()}.
start(_Type, _StartArgs) ->
    erli:ensure_deps_started(),
    erli_storage:init([]),
    erli_sup:start_link().

%% @private
%% @doc application termination callback for erli.
-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.
