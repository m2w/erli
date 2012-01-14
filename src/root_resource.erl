%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011-2012 Moritz Windelen.
%% @doc The erli root resource.

-module(root_resource).
-author('Moritz Windelen <moritz@tibidat.com>').

%% Webmachine resource function
-export([init/1,
	 service_available/2,
	 allowed_methods/2,
	 content_types_provided/2,
	 post_is_create/2,
	 process_post/2]).
%% Accept handler functions
-export([to_html/2]).

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
	_NonAcceptableContentType ->
	    {{halt, 415}, NRD, Ctx}
    end.

%%%=============================================================================
%%% Accept Handlers
%%%=============================================================================
from_json(RD, Ctx) ->
    case mochijson2:decode(wrq:req_body(RD)) of
	{struct, [{<<"url">>, TargetUrl}, {<<"tou_checked">>, true}]} ->
	    maybe_store(RD, #target{target=TargetUrl});
	_NonConformingBody ->
	    {{halt, 400}, RD, Ctx}
    end.

from_urlencoded(RD, Ctx) ->
    case is_proper_submission(RD) of
	{true, TargetUrl} ->
	    maybe_store(RD, #target{target=TargetUrl});
	{false, undefined} ->
	    {{halt, 400}, RD, Ctx}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
%%------------------------------------------------------------------------------
%% @private
%% @spec is_proper_submission(RD::wrq:reqdata()) -> {true, string()} |
%%                                                  {false, undefined}
%% @doc Returns whether the POST submission is as required.
%% @end
%%------------------------------------------------------------------------------
is_proper_submission(RD) ->
    {Bool, Target} = check_url_provided(RD),
    {is_tou_checked(RD) andalso Bool, Target}.

%%------------------------------------------------------------------------------
%% @private
%% @spec is_tou_checked(RD::wrq:reqdata()) -> true | false
%% @doc Returns whether the client agreed to the ToU.
%% @end
%%------------------------------------------------------------------------------
is_tou_checked(RD) ->
    case wrq:get_qs_value("tou_checked", RD) of
	"true" ->
	    true;
	undefined ->
	    false
    end.

%%------------------------------------------------------------------------------
%% @private
%% @spec check_url_provided(RD::wrq:reqdata()) -> {false, undefined} |
%%                                                {true, string()}
%% @doc Returns whether a URL has been provided.
%% @end
%%------------------------------------------------------------------------------
check_url_provided(RD) ->
    case wrq:get_qs_value("url", RD) of
	undefined ->
	    {false, undefined};
	Value ->
	    {true, Value}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @spec maybe_store(RD::wrq:reqdata(), Ctx::target()) -> true | false
%% @doc Returns whether the creation of a new path was successful.
%% @end
%%------------------------------------------------------------------------------
maybe_store(RD, Ctx) ->
    case erli_util:is_valid_url(Ctx#target.target) of
	false ->
	    % TODO: add request body which contains some kind of info 
	    % (e.g. needs a schema definition)
	    {{halt, 400}, RD, Ctx};
	true ->
	    attempt_store(RD, Ctx)
    end.

%%------------------------------------------------------------------------------
%% @private
%% @spec attempt_store(RD::wrq:reqdata(), Ctx::target()) -> true | error
%% @doc Attempts to create a new path for the target URL and store it in mnesia.
%% @end
%%------------------------------------------------------------------------------
attempt_store(RD, Ctx) ->
    case erli_storage:put(Ctx#target.target) of
	{error, path_generation_failed} ->
	    {error, RD, Ctx}; % storage errors return a 500
	{error, target_banned} ->
	    {{halt, 410}, RD, Ctx}; % banned target urls return a 410
	{ok, Path} ->
	    NRD = wrq:set_resp_header("Location", Path#path.path, RD),
	    {true, NRD, Ctx}
    end.
