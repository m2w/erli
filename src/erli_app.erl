%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011-2012 Moritz Windelen.

%% @doc Callbacks for the erli application.

-module(erli_app).
-author('Moritz Windelen <moritz@tibidat.com>').

-behaviour(application).
-export([start/2,stop/1]).

%%------------------------------------------------------------------------------
%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erli.
%% @end
%%------------------------------------------------------------------------------
start(_Type, _StartArgs) ->
    erli_sup:start_link().

%%------------------------------------------------------------------------------
%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erli.
%% @end
%%------------------------------------------------------------------------------
stop(_State) ->
    ok.
