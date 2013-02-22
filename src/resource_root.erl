%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2012-2013 Moritz Windelen.
%% @doc The erli root resource.

-module(resource_root).
-author('Moritz Windelen <moritz@tibidat.com>').

%% Webmachine resource functions
-export([allowed_methods/2, content_types_provided/2,
	 init/1, post_is_create/2, process_post/2]).

%% Content provider
-export([to_html/2]).

%% Content-Type handlers
-export([from_json/2, from_urlencoded/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("erli_persistence.hrl").

init([]) ->
    {ok, #path{}}.

allowed_methods(RD, Ctx) ->
    {['GET', 'POST'], RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[{"text/html", to_html}], RD, Ctx}.

post_is_create(RD, Ctx) ->
    {false, RD, Ctx}.

process_post(RD, Ctx) ->
    H = wrq:get_req_header("Content-Type", RD),
    case H of
	undefined ->
	    {{halt, 400}, RD, Ctx};
	Header ->
	    CTH = mochiweb_util:parse_header(Header),
	    case CTH of
		{"application/json", _MediaType} ->
		    from_json(RD, Ctx);
		{"application/x-www-form-urlencoded", _MediaType} ->
		    from_urlencoded(RD, Ctx);
		_Unrecognized ->
		    {{halt, 415}, RD, Ctx}
	    end
    end.

%%%=============================================================================
%%% Content Provider
%%%=============================================================================

to_html(RD, Ctx) ->
    {ok, Content} = index_dtl:render([]),
    {Content, RD, Ctx}.

%%%=============================================================================
%%% Accept Handlers
%%%=============================================================================

from_json(RD, Ctx) ->
    case mochijson2:decode(wrq:req_body(RD)) of
	{struct, [{<<"url">>, TargetUrl}, {<<"tou_checked">>, true}]} ->
	    maybe_store(RD, #path{target_url = TargetUrl});
	_NonConformingBody ->
	    {{halt, 400}, RD, Ctx}
    end.

from_urlencoded(RD, Ctx) ->
    case is_proper_submission(RD) of
	{true, TargetUrl} ->
	    maybe_store(RD, #path{target_url = TargetUrl});
	{false, undefined} ->
	    {{halt, 400}, RD, Ctx}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @private
%% @doc Returns whether the POST submission contains all necessary KVPs.
is_proper_submission(RD) ->
    {Bool, TargetUrl} = check_url_provided(RD),
    {is_tou_checked(RD) andalso Bool, TargetUrl}.

%% @private
%% @doc Returns whether the client agreed to the ToU.
is_tou_checked(RD) ->
    case wrq:get_qs_value("tou_checked", RD) of
	"true" ->
	    true;
	undefined ->
	    false
    end.

%% @private
%% @doc Returns the value of the URL request parameter.
check_url_provided(RD) ->
    case wrq:get_qs_value("url", RD) of
	undefined ->
	    {false, undefined};
	Value ->
	    {true, Value}
    end.

%% @private
%% @doc Verified the URL to shorten, if valid procceeds with an attempt to store
%% said URL. If invalid, aborts request evaluation and returns a 400 status code.
maybe_store(RD, Ctx) ->
    case erli_utils:is_valid_url(Ctx#path.target_url) of
	false ->
	    {{halt, 400}, RD, Ctx};
	true ->
	    attempt_store(RD, Ctx)
    end.

%% @private
%% @doc Attempts to generate a shortened URL for the target URL and store it in mnesia.
attempt_store(RD, Ctx) ->
    case erli_storage:put(Ctx#path.target_url) of
	{error, path_generation_failed} ->
	    {{error, path_generation_failed}, RD, Ctx}; % storage errors return a 500
	{error, target_banned} ->
	    {{halt, 410}, RD, Ctx}; % banned target urls return a 410
	{ok, Path} ->
	    NRD = wrq:set_resp_header("Location",
				      binary_to_list(Path#path.path), RD),
	    {true, NRD, Ctx}
    end.
