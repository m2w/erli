%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011 Moritz Windelen.
%% @doc Simplistic static content resource.

-module(static_resource).
-author('Moritz Windelen <moritz@tibidat.com>').

% Webmachine resource functions
-export([init/1,
	 allowed_methods/2,
	 content_types_provided/2]).
% Content-Type handlers
-export([maybe_provide_content/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {basedir, fpath}).


init([StaticDir]) ->
    {ok, App} = application:get_application(),
    BaseDir = filename:join(code:priv_dir(App), StaticDir),
    {ok, #state{basedir=BaseDir}}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{webmachine_util:guess_mime(wrq:disp_path(ReqData)), maybe_provide_content}], 
     ReqData, 
     Context#state{fpath=determine_fpath(ReqData, Context)}}.

%%%=============================================================================
%%% Content Handler
%%%=============================================================================
%%------------------------------------------------------------------------------
%% @doc Determines whether to provide the requested resource, based on whether
%%      it exists and is 'safe'.
%% @end
%%------------------------------------------------------------------------------
maybe_provide_content(ReqData, #state{fpath=Path, _=_}=Context) ->
    case Path of
	undefined ->
	    {{halt, 403}, ReqData, Context};
	P ->
	    FPath = filename:join(Context#state.basedir, P),
	    case filelib:is_regular(FPath) of
		true ->
		    fetch_content(ReqData, Context#state{fpath=FPath});
		false ->
		    {{halt, 404}, ReqData, Context}
	    end
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
%%------------------------------------------------------------------------------
%% @doc Returns either a sanitized path for a resource or undefined.
%% @end
%%------------------------------------------------------------------------------
determine_fpath(ReqData, Context) ->
    case mochiweb_util:safe_relative_path(wrq:disp_path(ReqData)) of
	undefined ->
	    undefined;
	Path ->
	    filename:join(Context#state.basedir, Path)
    end.

%%------------------------------------------------------------------------------
%% @doc Returns the resource contents.
%% @end
%%------------------------------------------------------------------------------
fetch_content(ReqData, Context) ->    
    {ok, Content} = file:read_file(Context#state.fpath),
    {Content, ReqData, Context}.
