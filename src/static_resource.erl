%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011-2012 Moritz Windelen.
%% @doc Simplistic static content resource.

-module(static_resource).
-author('Moritz Windelen <moritz@tibidat.com>').

% Webmachine resource functions
-export([init/1,
	 allowed_methods/2,
	 content_types_provided/2]).
% Accept handler
-export([maybe_provide_content/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {basedir, fpath}).

init([StaticDir]) ->
    {ok, App} = application:get_application(),
    BaseDir = filename:join(code:priv_dir(App), StaticDir),
    {ok, #state{basedir=BaseDir}}.

allowed_methods(RD, Ctx) ->
    {['GET'], RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[{webmachine_util:guess_mime(wrq:disp_path(RD)), maybe_provide_content}], 
     RD, 
     Ctx#state{fpath=determine_fpath(RD, Ctx)}}.

%%%=============================================================================
%%% Content Provider
%%%=============================================================================
%%------------------------------------------------------------------------------
%% @spec maybe_provide_content(RD::wrq:reqdata(), Ctx::state()) ->
%%                                  {{halt, 403}, wrq:reqdata(), state()} |
%%                                  {binary(), wrq:reqdata(), state()}  | 
%%                                  {{halt, 404}, wrq:reqdata(), state()}
%% @doc Determines whether to provide the requested resource, based on whether
%%      it exists and is 'safe'.
%% @end
%%------------------------------------------------------------------------------
maybe_provide_content(RD, #state{fpath=Path, _=_}=Ctx) ->
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
%%------------------------------------------------------------------------------
%% @private
%% @spec maybe_fetch_content(FPath::string(), RD::wrq:reqdata(), Ctx::state())
%%       -> {binary(), wrq:reqdata(), state()} | 
%%          {{halt, 404}, wrq:reqdata(), state()}
%% @doc Determines whether the request should be processed further based on 
%%      whether the requested resource exists or not.
%% @end
%%------------------------------------------------------------------------------
maybe_fetch_content(FPath, RD, Ctx) ->
    case filelib:is_regular(FPath) of
	true ->
	    fetch_content(RD, Ctx#state{fpath=FPath});
	false ->
	    {{halt, 404}, RD, Ctx}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @spec determine_fpath(RD::wrq:reqdata(), Ctx::state()) -> undefined |
%%                                                           string()
%% @doc Returns either a sanitized path for to a resource or undefined.
%% @end
%%------------------------------------------------------------------------------
determine_fpath(RD, Ctx) ->
    case mochiweb_util:safe_relative_path(wrq:disp_path(RD)) of
	undefined ->
	    undefined;
	Path ->
	    filename:join(Ctx#state.basedir, Path)
    end.

%%------------------------------------------------------------------------------
%% @private
%% @spec fetch_content(RD::wrq:reqdata(), Ctx::state()) -> 
%%                                     {binary(), wrq:reqdata(), state()}
%% @doc Returns the resource contents.
%% @end
%%------------------------------------------------------------------------------
fetch_content(RD, Ctx) ->    
    {ok, Content} = file:read_file(Ctx#state.fpath),
    {Content, RD, Ctx}.
