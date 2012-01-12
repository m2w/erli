%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011 Moritz Windelen.
%% @doc The erli resource for handling anything beyond "/".

-module(path_resource).
-author('Moritz Windelen <moritz@tibidat.com>').

%% Webmachine resource function
-export([init/1,
	 service_available/2,
	 allowed_methods/2,
	 content_types_provided/2,
	 content_types_accepted/2,
	 delete_resource/2,
	 delete_completed/2,
	 resource_exists/2]).
%% Accept handler functions
-export([to_html/2,
	 to_json/2]).
%% Content-Type handler functions
-export([from_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("erli.hrl").

init([]) ->
    {ok, #target{}}.

service_available(RD, Ctx) ->
    {not erli_throttle:throttle_req(), RD, Ctx}.

allowed_methods(RD, Ctx) ->
    case wrq:disp_path(RD) of
	"" ->
	    {['GET', 'DELETE', 'PUT'], RD, Ctx};
	_ ->
	    {['GET'], RD, Ctx}
    end.

resource_exists(RD, Ctx) ->
    case erli_storage:read(wrq:path_info(path, RD)) of
	{ok, Target} ->
	    {true, RD, Target};
	_ ->
	    {false, RD, Ctx}
    end.

delete_resource(RD, Ctx) ->
    {ok, _Target} = erli_storage:delete(wrq:path_info(path, RD)),
    {true, RD, Ctx}.

delete_completed(RD, Ctx) ->
    {false, RD, Ctx}. % erli only increments rep_num on a delete -> 202

content_types_provided(RD, Ctx) ->
    {[{"text/html", to_html}, {"application/json", to_json}], RD, Ctx}.

to_html(RD, Ctx) ->
    case wrq:disp_path(RD) of
	"" ->
	    handle_path(html, RD, Ctx); % landing page || redir
	_ ->
	    handle_path_subreq(html, RD, Ctx) % /path/stats || /path/report
    end.

to_json(RD, Ctx) ->
    case wrq:disp_path(RD) of
	"" ->
	    handle_path(json, RD, Ctx); % landing page || redir
	_ ->
	    handle_path_subreq(json, RD, Ctx) % /path/stats || /path/report
    end.

handle_path(Type, RD, Ctx) ->
    NRD = wrq:set_resp_header("Location", binary_to_list(Ctx#target.target), RD),
    case wrq:get_qs_value("landing", RD) of
	undefined -> {{halt, 302}, NRD, Ctx};
	_ -> 
	    case Type of
		html ->
		    {ok, Content} = landing_dtl:render([{target, Ctx#target.target},
							{path, wrq:path_info(path)}]),
		    {Content, NRD, Ctx};
		json ->
		    Content = mochijson2:encode([{target, Ctx#target.target}]),
		    {Content, NRD, Ctx}
	    end
    end.

handle_path_subreq(Type, RD, Ctx) ->
    Path = wrq:path_info(path, RD),
    {[ThePath], _OtherPaths} = 
	lists:partition(fun(#path{path=P, _=_}=_Path) -> Path =:= P end, 
			Ctx#target.paths),
    case wrq:disp_path(RD) of
	"stats" ->
	    case Type of
		html ->
		    {ok, Content} = stats_dtl:render([{target, Ctx#target.target}, 
						      {path, list_to_binary(ThePath#path.path)}, 
						      {total_clicks, ThePath#path.total_clicks}, 
						      {unique_clicks, ThePath#path.unique_clicks}, 
						      {country_lst, ThePath#path.country_lst}]),
		    {Content, RD, Ctx};
		json ->
		    % TODO: switch from mochijson2 to https://github.com/davisp/eep0018 
		    Content = mochijson2:encode({struct, [{target, Ctx#target.target}, 
						 {path, list_to_binary(ThePath#path.path)}, 
						 {total_clicks, ThePath#path.total_clicks}, 
						 {unique_clicks, ThePath#path.unique_clicks}, 
						 {country_lst, ThePath#path.country_lst}]}),
		    {Content, RD, Ctx}
	    end;
	"report" ->
	    case Type of
		html ->
		    {ok, Content} = report_dtl:render([{target, Ctx#target.target}, 
						       {path, list_to_binary(ThePath#path.path)}]),
		    {Content, RD, Ctx};
		json ->
		    {{halt, 202}, RD, Ctx} % with a 202
	    end;
	"check" ->
	    {{halt, 200}, RD, Ctx}; % fake landing for low overhead checking of path availability
	_ ->
	    {{halt, 404}, RD, Ctx}
    end.

content_types_accepted(RD, Ctx) ->
    NRD = wrq:set_max_recv_body(1024, RD), % no need to accept large bodies
    {[{"application/json", from_json}], NRD, Ctx}.

from_json(RD, Ctx) ->
    case mochijson2:decode(wrq:req_body(RD)) of
	{struct, [{<<"url">>, TargetUrl}, {<<"tou_checked">>, true}]} ->
	    case erli_util:is_valid_url(TargetUrl) of
		false ->
		    % TODO: add request body which contains some kind of info (e.g. needs a schema definition)
		    {{halt, 400}, RD, Ctx};
		true ->
		    case erli_storage:put(TargetUrl, 
					  wrq:path_info(path, RD)) of
			{ok, Target} ->
			    {true, RD, Target};
			conflict ->
			    {{halt, 409}, RD, Ctx};
			{target_banned, _Target} ->
			    {{halt, 410}, RD, Ctx}
		    end
		end;
	_ ->
	    {{halt, 400}, RD, Ctx}
    end.
