%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2012-2013 Moritz Windelen.
%% @doc Resource to handle shortened URLs.

-module(resource_path).
-author('Moritz Windelen <moritz@tibidat.com>').

%% Webmachine resource function
-export([allowed_methods/2, content_types_accepted/2,
	 content_types_provided/2, delete_completed/2,
	 delete_resource/2, init/1, resource_exists/2]).

%% Content providers
-export([to_html/2, to_json/2]).

%% Content-Type handler
-export([from_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("erli_persistence.hrl").

init([]) ->
    {ok, #path{}}.

allowed_methods(RD, Ctx) ->
    case wrq:disp_path(RD) of
	"" ->
	    {['GET', 'DELETE', 'PUT'], RD, Ctx};
	_ ->
	    {['GET'], RD, Ctx}
    end.

resource_exists(RD, Ctx) ->
    case erli_storage:get(list_to_binary(wrq:path_info(path, RD))) of
	{ok, Path} ->
	    {true, RD, Path};
	{error, deleted} ->
	    {{halt, 410}, RD, Ctx};
	_ ->
	    {false, RD, Ctx}
    end.

delete_resource(RD, Ctx) ->
    erli_storage:report(list_to_binary(wrq:path_info(path, RD))),
    {true, RD, Ctx}.

delete_completed(RD, Ctx) ->
    {false, RD, Ctx}. % erli only increments rep_num on a delete -> 202

content_types_provided(RD, Ctx) ->
    {[{"text/html", to_html},
      {"application/json", to_json}],
     RD, Ctx}.

content_types_accepted(RD, Ctx) ->
    NRD = wrq:set_max_recv_body(1024,
				RD), % no need to accept large bodies
    {[{"application/json", from_json}], NRD, Ctx}.

%%%=============================================================================
%%% Content providers
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
%%% Content-Type handler
%%%=============================================================================

from_json(RD, Ctx) ->
    Body = wrq:req_body(RD),
    NonConforming = {{halt, 400}, RD, Ctx},
    case jsx:is_json(Body) of
	true ->
	    case jsx:decode(Body) of
		[{<<"url">>, TargetUrl}, {<<"tou_checked">>, true}] ->
		    process_body(TargetUrl, RD, Ctx);
		_NonConformingBody ->
		    NonConforming
	    end;
	false ->
	    NonConforming
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @private
%% @doc Processes the json contents of a request body, verifying the validity of
%% the `Url' and attempting to write the new path to mnesia.
process_body(Url, RD, Ctx) ->
    case erli_utils:is_valid_url(Url) of
	false ->
	    {{halt, 400}, RD, Ctx};
	true ->
	    maybe_create_shortened_url(Url, RD, Ctx)
    end.

%% @private
%% @doc Verifies that the custom URL is valid before attempting to write
%% the new shortened URL to mnesia.
maybe_create_shortened_url(Url, RD, Ctx) ->
    CustomPath = wrq:path_info(path, RD),
    TrimmedCustomPath = string:strip(CustomPath),
    case is_valid_custom_path(TrimmedCustomPath) of
	true ->
	    case erli_storage:put(Url, list_to_binary(CustomPath)) of
		{ok, Path} ->
		    {true, RD, Path};
		{error, conflict} ->
		    {{halt, 409}, RD, Ctx};
		{error, target_banned} ->
		    {{halt, 410}, RD, Ctx}
	    end;
	false ->
	    {{halt, 400}, RD, Ctx}
    end.

%% @private
%% @doc Ensure that the custom path is not too long.
%% TODO: it would be nice to add a swearword filter.
is_valid_custom_path(Path) when erlang:length(Path) < 10 ->
    true;
is_valid_custom_path(_Path) ->
    false.

%% @private
%% @doc Checks whether the request should be redirected to its target URL or a
%% if landing page should be displayed.
handle_path(Type, RD, Ctx) ->
    NRD = wrq:set_resp_header("Location",
			      binary_to_list(Ctx#path.target_url), RD),
    case wrq:get_qs_value("landing", RD) of
	undefined ->
	    erli_stats:record_visit(Ctx#path.path, RD),
	    NNRD = wrq:set_resp_header("Cache-Control",
				       "max-age=86400, must-revalidate", NRD), % set an expiry on the 301, in case the link get's banned
	    {{halt, 301}, NNRD, Ctx};
	_Value ->
	    handle_landing_page(Type, NRD, Ctx)
    end.

%% @private
%% @doc Renders a HTML landing page or returns json containing the necessary
%% information for a path and its target URL.
handle_landing_page(html, RD, Ctx) ->
    {ok, Content} = landing_dtl:render([{target,
					 Ctx#path.target_url},
					{path, wrq:path_info(path, RD)}]),
    {Content, RD, Ctx};
handle_landing_page(json, RD, Ctx) ->
    Content = jsx:encode([{<<"target">>,
			   Ctx#path.target_url}]),
    {Content, RD, Ctx};
handle_landing_page(_ContentType, RD, Ctx) ->
    {error, RD, Ctx}.

%% @private
%% @doc Renders the HTML or json content of subpaths (/stats, /report, /check)
handle_path_subreq(Type, RD, Ctx) ->
    case wrq:disp_path(RD) of
	"stats" ->
	    handle_stats_subreq(Type, RD, Ctx);
	"report" ->
	    handle_report_subreq(Type, RD, Ctx);
	"check" ->
	    {{halt, 200}, RD, Ctx}; % landing for lower overhead checking of path availability
	_ ->
	    {{halt, 404}, RD, Ctx}
    end.

%% @private
%% @doc Renders the HTML or json response for path statistics.
handle_stats_subreq(html, RD, Ctx) ->
    {ok, Content} = stats_dtl:render([{target,
				       Ctx#path.target_url},
				      {path, Ctx#path.path},
				      {total_clicks, Ctx#path.total_clicks},
				      {visitors_by_country,
				       sets:to_list(Ctx#path.visitors_by_country)},
				      {visitors_by_time,
				       mochiweb_util:record_to_proplist(Ctx#path.visitors_by_time,
									record_info(fields,
										    timeslots))}]),
    {Content, RD, Ctx};
handle_stats_subreq(json, RD, Ctx) ->
    Content = jsx:encode([{target, Ctx#path.target_url},
			  {path, Ctx#path.path},
			  {total_clicks, Ctx#path.total_clicks},
			  {visitors_by_country,
			   sets:to_list(Ctx#path.visitors_by_country)},
			  {visitors_by_time,
			   tl(mochiweb_util:record_to_proplist(
				Ctx#path.visitors_by_time,
				record_info(fields,
					    timeslots)))}]),
    {Content, RD, Ctx};
handle_stats_subreq(_ContentType, RD, Ctx) ->
    {error, RD, Ctx}.

%% @private
%% @doc Renders the HTML or json response for reporting a path.
handle_report_subreq(html, RD, Ctx) ->
    {ok, Content} = report_dtl:render([{target,
					Ctx#path.target_url},
				       {path, Ctx#path.path}]),
    {Content, RD, Ctx};
handle_report_subreq(json, RD, Ctx) ->
    {{halt, 202}, RD, Ctx};
handle_report_subreq(_ContentType, RD, Ctx) ->
    {error, RD, Ctx}.
