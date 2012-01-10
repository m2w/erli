%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright Moritz Windelen 2011.

%% @doc Callbacks for the erli application.

-module(erli_app).
-author('Moritz Windelen <moritz@tibidat.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erli.
start(_Type, _StartArgs) ->
    erli_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erli.
stop(_State) ->
    ok.
