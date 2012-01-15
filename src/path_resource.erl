%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011-2012 Moritz Windelen.
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
    case erli_throttle:check() of
	false ->
	    {true, RD, Ctx};
	 {true, RetryAfter}->
	    NRD = wrq:set_resp_header("Retry-After",
				      integer_to_list(RetryAfter),
				      RD),
	    {false, NRD, Ctx}
    end.

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

content_types_accepted(RD, Ctx) ->
    NRD = wrq:set_max_recv_body(1024, RD), % no need to accept large bodies
    {[{"application/json", from_json}], NRD, Ctx}.

%%%=============================================================================
%%% Accept Handlers
%%%=============================================================================
to_html(RD, Ctx) ->
    case wrq:disp_path(RD) of
	"" ->
	    handle_path(html, RD, Ctx); % landing page || redir
	_Path ->
	    handle_path_subreq(html, RD, Ctx) % /path/stats || /path/report
    end.

to_json(RD, Ctx) ->
    case wrq:disp_path(RD) of
	"" ->
	    handle_path(json, RD, Ctx); % landing page || redir
	_Path ->
	    handle_path_subreq(json, RD, Ctx) % /path/stats || /path/report
    end.

%%%=============================================================================
%%% Content-Type Handler
%%%=============================================================================
from_json(RD, Ctx) ->
    case mochijson2:decode(wrq:req_body(RD)) of
	{struct, [{<<"url">>, TargetUrl}, {<<"tou_checked">>, true}]} ->
	    process_body(TargetUrl, RD, Ctx);
	_NonConformingBody ->
	    {{halt, 400}, RD, Ctx}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
%%------------------------------------------------------------------------------
%% @private
%% @spec process_body(Url::binary(), RD::wrq:reqdata(), Ctx::target()) ->
%%                                                                {ok, target()}
%% @doc Processes the json contents of a request body, verifying the validity of
%%      the `Url' and attempting to write the new path to mnesia.
%% @end
%%------------------------------------------------------------------------------
process_body(Url, RD, Ctx) ->
    case erli_util:is_valid_url(Url) of
	false ->
	    % TODO: add request body which contains some kind of info (e.g. needs a schema definition)
	    {{halt, 400}, RD, Ctx};
	true ->
	    case erli_storage:put(Url, wrq:path_info(path, RD)) of
		{ok, Target} ->
		    {true, RD, Target};
		{error, conflict} ->
		    {{halt, 409}, RD, Ctx};
		{error, target_banned} ->
		    {{halt, 410}, RD, Ctx}
	    end
    end.

%%------------------------------------------------------------------------------
%% @private
%% @spec handle_path(Type::atom, RD::wrq:reqdata(), Ctx::target()) ->
%%                                           {iolist(), wrq:reqdata(), target()}
%% @doc Checks whether the request should be redirected to its target URL or a
%%      if landing page should be displayed.
%% @end
%%------------------------------------------------------------------------------
handle_path(Type, RD, Ctx) ->
    NRD = wrq:set_resp_header("Location",
			      binary_to_list(Ctx#target.target),
			      RD),
    case wrq:get_qs_value("landing", RD) of
	undefined ->
	    {{halt, 302}, NRD, Ctx};
	_Value ->
	    handle_landing_page(Type, NRD, Ctx)
    end.

%%------------------------------------------------------------------------------
%% @private
%% @spec handle_landing_page(ContentType::atom(), RD::wrq:reqdata(),
%%                            Ctx::target()) ->
%%                                           {iolist(), wrq:reqdata(), target()}
%% @doc Renders a HTML landing page or returns json containing the necessary
%%      information for a path and its target URL.
%% @end
%%------------------------------------------------------------------------------
handle_landing_page(ContentType, RD, Ctx) when ContentType =:= html ->
    {ok, Content} = landing_dtl:render([{target,
					 Ctx#target.target},
					{path,
					 wrq:path_info(path)}
				       ]),
    {Content, RD, Ctx};
handle_landing_page(ContentType, RD, Ctx) when ContentType =:= json->
    Content = mochijson2:encode([{target, Ctx#target.target}]),
    {Content, RD, Ctx};
handle_landing_page(_ContentType, RD, Ctx) ->
    {error, RD, Ctx}.

%%------------------------------------------------------------------------------
%% @private
%% @spec handle_path_subreq(Type::atom(), RD::wrq:reqdata(), Ctx::target()) ->
%%                                           {iolist(), wrq:reqdata(), target()}
%% @doc Renders the HTML or json content of subpaths (/stats, /report, /check)
%% @end
%%------------------------------------------------------------------------------
handle_path_subreq(Type, RD, Ctx) ->
    Path = wrq:path_info(path, RD),
    {[ThePath], _OtherPaths} =
	lists:partition(fun(#path{path=P, _=_}=_Path) -> Path =:= P end,
			Ctx#target.paths),
    case wrq:disp_path(RD) of
	"stats" ->
	    handle_stats_subreq(Type, ThePath, RD, Ctx);
	"report" ->
	    handle_report_subreq(Type, ThePath, RD, Ctx);
	"check" ->
	    {{halt, 200}, RD, Ctx}; % landing for lower overhead checking of path availability
	_ ->
	    {{halt, 404}, RD, Ctx}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @spec handle_stats_subreq(ContentType::atom(), Path::string(),
%%                           RD::wrq:reqdata(), Ctx::target()) ->
%%                                           {iolist(), wrq:reqdata(), target()}
%% @doc Renders the HTML or json response for path statistics.
%% @end
%%------------------------------------------------------------------------------
handle_stats_subreq(ContentType, Path, RD, Ctx) when ContentType =:= html ->
    {ok, Content} = stats_dtl:render([{target, Ctx#target.target},
				      {path, list_to_binary(Path#path.path)},
				      {total_clicks, Path#path.total_clicks},
				      {unique_clicks, Path#path.unique_clicks},
				      {country_lst, Path#path.country_lst},
				      {timeslots,
				       mochiweb_util:record_to_proplist(
					 Path#path.timeslot_visits,
					 record_info(fields, timeslots)
					)
				      }]),
    {Content, RD, Ctx};
handle_stats_subreq(ContentType, Path, RD, Ctx) when ContentType =:= json ->
    % TODO: switch from mochijson2 to https://github.com/davisp/eep0018
    Content = mochijson2:encode({struct,
				 [{target, Ctx#target.target},
				  {path, list_to_binary(Path#path.path)},
				  {total_clicks, Path#path.total_clicks},
				  {unique_clicks, Path#path.unique_clicks},
				  {country_lst, Path#path.country_lst},
				  {timeslots,
				   mochiweb_util:record_to_proplist(
				     Path#path.timeslot_visits,
				     record_info(fields, timeslots)
				    )}
				 ]}),
    {Content, RD, Ctx};
handle_stats_subreq(_ContentType, _Path, RD, Ctx) ->
    {error, RD, Ctx}.

%%------------------------------------------------------------------------------
%% @private
%% @spec handle_report_subreq(ContentType::atom(), Path::string(),
%%                           RD::wrq:reqdata(), Ctx::target()) ->
%%                                           {iolist(), wrq:reqdata(), target()}
%% @doc Renders the HTML or json response for reporting a path.
%% @end
%%------------------------------------------------------------------------------
handle_report_subreq(ContentType, Path, RD, Ctx) when ContentType =:= html ->
    {ok, Content} = report_dtl:render([{target, Ctx#target.target},
				       {path, list_to_binary(Path#path.path)}]),
    {Content, RD, Ctx};
handle_report_subreq(ContentType, _Path, RD, Ctx) when ContentType =:= json ->
    {{halt, 202}, RD, Ctx};
handle_report_subreq(_ContentType, _Path, RD, Ctx) ->
    {error, RD, Ctx}.



