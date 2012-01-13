%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011 Moritz Windelen.
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
	    case filelib:is_regular(FPath) of
		true ->
		    fetch_content(RD, Ctx#state{fpath=FPath});
		false ->
		    {{halt, 404}, RD, Ctx}
	    end
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
%%------------------------------------------------------------------------------
%% @private
%% @spec determine_fpath(RD::#wm_reqdata{}, Ctx::#state{}) -> undefined |
%%                                                             ::string()
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
%% @spec fetch_content(RD::#wm_reqdata{}, Ctx::#state{}) -> 
%%                                           {binary(), #wm_reqdata{}, #state{}}
%% @doc Returns the resource contents.
%% @end
%%------------------------------------------------------------------------------
fetch_content(RD, Ctx) ->    
    {ok, Content} = file:read_file(Ctx#state.fpath),
    {Content, RD, Ctx}.
