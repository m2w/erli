%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011 Moritz Windelen.
%% @doc The erli root resource.

-module(root_resource).
-author('Moritz Windelen <moritz@tibidat.com>').

%% Webmachine resource function
-export([init/1,
	 allowed_methods/2,
	 content_types_provided/2,
	 post_is_create/2,
	 process_post/2]).
%% Accept handler functions
-export([to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("erli.hrl").

init([]) ->
    {{trace, "/tmp"}, #target{}}. % debug mode
%    {ok, #target{}}.

allowed_methods(RD, Ctx) ->
    {['GET', 'POST'], RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[{"text/html", to_html}], RD, Ctx}.

to_html(RD, Ctx) ->
    {ok, Content} = index_dtl:render([]),
    {Content, RD, Ctx}.

post_is_create(RD, Ctx) ->
    {false, RD, Ctx}.

process_post(RD, Ctx) ->
    NRD = wrq:set_max_recv_body(1024, RD), % no need to accept large bodies
    case wrq:get_req_header("Content-Type", NRD) of
	"application/json" ->
	    from_json(NRD, Ctx);
	"application/x-www-form-urlencoded" ->
	    from_urlencoded(NRD, Ctx);
	_ ->
	    {{halt, 415}, NRD, Ctx}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
from_json(RD, Ctx) ->
    case mochijson2:decode(wrq:req_body(RD)) of
	{struct, [{<<"url">>, TargetUrl}, {<<"tou_checked">>, true}]} ->
	    maybe_store(RD, #target{target=TargetUrl});
	_ ->
	    {{halt, 400}, RD, Ctx}
    end.

from_urlencoded(RD, Ctx) ->
    case wrq:get_qs_value("tou_checked", RD) of
	true ->
	    case wrq:get_qs_value("url", RD) of
		undefined ->
		    {{halt, 400}, RD, Ctx}; % the duplication of the 400 here is 'ungood'
		TargetUrl ->
		    maybe_store(RD, #target{target=TargetUrl})
	    end;
	_ ->
	    {{halt, 400}, RD, Ctx}
    end.

maybe_store(RD, Ctx) ->
    case erli_util:is_valid_url(Ctx#target.target) of
	false ->
	    % TODO: add request body which contains some kind of info (e.g. needs a schema definition)
	    {{halt, 400}, RD, Ctx};
	true ->
	    case erli_storage:put(Ctx#target.target) of
		error ->
		    {error, RD, Ctx}; % storage errors return a 500
		{target_banned, _Target} ->
		    {{halt, 410}, RD, Ctx}; % banned target urls return a 410
		{ok, Path} ->
		    NRD = wrq:set_resp_header("Location", Path#path.path, RD),
		    {true, NRD, Ctx}
	    end
    end.

