%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc The erli backend startup code.
%%% @end
%%%==========================================================

-module(erli).

-export([start/0,
	 start_link/0,
	 stop/0]).

%%-----------------------------------------------------------
%% API Methods
%%-----------------------------------------------------------

%% @doc Start the backend for inclusion in a supervisor tree
-spec start_link() -> {ok, pid()}.
start_link() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    ensure_started(webmachine),
    ensure_started(mnesia),
    ensure_started(egeoip),
    erli_storage:setup_tables([node()]),
    erli_sup:start_link().

-spec start() -> ok.
start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    ensure_started(webmachine),
    ensure_started(mnesia),
    ensure_started(egeoip),
    erli_storage:setup_tables([node()]),
    application:start(erli).

-spec stop() -> ok.
stop() ->
    Res = application:stop(erli),
    application:stop(egeoip),
    application:stop(mnesia),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    Res.

%%-----------------------------------------------------------
%% Internal Methods
%%-----------------------------------------------------------

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
	{error, {already_started, App}} ->
            ok
    end.
