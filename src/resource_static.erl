%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2012-2013 Moritz Windelen.
%% @doc Resource to server static contents.

-module(resource_static).
-author('Moritz Windelen <moritz@tibidat.com>').

%% Webmachine resource functions
-export([allowed_methods/2, content_types_provided/2,
	 init/1]).

%% Content provider
-export([maybe_provide_content/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {basedir, fpath}).

init(_Args) ->
    StaticDir = erli_utils:get_env(static_dir, "static"),
    {ok, App} = application:get_application(),
    BaseDir = filename:join(code:priv_dir(App), StaticDir),
    {ok, #state{basedir = BaseDir}}.

allowed_methods(RD, Ctx) ->
    {['GET'], RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[{webmachine_util:guess_mime(wrq:disp_path(RD)),
       maybe_provide_content}],
     RD, Ctx#state{fpath = determine_fpath(RD, Ctx)}}.

%%%=============================================================================
%%% Content Provider
%%%=============================================================================

%% @doc Determines whether to provide the resource, based on whether
%% it exists and is 'safe'.
maybe_provide_content(RD, #state{fpath = Path, _ = _} = Ctx) ->
    case Path of
	undefined ->
	    {{halt, 403}, RD, Ctx};
	P ->
	    FPath = filename:join(Ctx#state.basedir, P),
	    maybe_fetch_content(FPath, RD, Ctx)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @private
%% @doc Determines whether the request should be processed further based on
%% whether the requested resource exists or not.
maybe_fetch_content(FPath, RD, Ctx) ->
    case filelib:is_regular(FPath) of
	true ->
	    fetch_content(RD, Ctx#state{fpath = FPath});
	false ->
	    {{halt, 404}, RD, Ctx}
    end.

%% @private
%% @doc Returns either a sanitized path for to a resource or undefined.
determine_fpath(RD, Ctx) ->
    case mochiweb_util:safe_relative_path(wrq:disp_path(RD)) of
	undefined ->
	    undefined;
	Path ->
	    filename:join(Ctx#state.basedir, Path)
    end.

%% @private
%% @doc Returns the resource contents.
fetch_content(RD, Ctx) ->
    {ok, Content} = file:read_file(Ctx#state.fpath),
    {Content, RD, Ctx}.
