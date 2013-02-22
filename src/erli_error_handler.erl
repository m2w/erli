%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2012-2013 Moritz Windelen.
%% @doc erlydtl based error handlers.

-module(erli_error_handler).
-author('Moritz Windelen <moritz@tibidat.com>').

%% API
-export([render_error/3]).

-include_lib("webmachine/include/wm_reqstate.hrl").

-type status_code() :: 400 | 403 | 404 | 410 | 500 | 501 | 502 | 503.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Choses the appropriate representation for an error message.
-spec render_error(status_code(), Req :: #wm_reqstate{}, Reason :: atom()) ->
			  {Contents :: iolist() | binary(), Req :: #wm_reqstate{}}.
render_error(Code, Req, Reason) ->
    case Req:has_response_body() of
	{true, _} -> Req:response_body();
	{false, _} ->
	    {AcceptHeaderVal, _Req} = Req:get_header_value("Accept"),
	    {Types, _Options} = mochiweb_util:parse_header(AcceptHeaderVal),
	    NReq = Req:trim_state(),
	    case webmachine_util:choose_media_type(
		   ["text/html", "application/json"], Types) of
		none ->
		    render(unsupported, Code, NReq, Reason);
		T ->
		    render(T, Code, NReq, Reason)
	    end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Renders the error message, depending on which format the response
%% should take.
-spec render(atom(), status_code(),
	     Req :: #wm_reqstate{}, Reason :: atom()) ->
		    {Contents :: iolist() | binary(), Req :: #wm_reqstate{}}.
render("text/html", StatusCode, Req, Reason) ->
    {ok, NReq} = Req:add_response_header("Content-Type", "text/html"),
    {ok, Content} =
	case StatusCode of
	    400 ->
		html_badrequest_dtl:render();
	    403 ->
		html_forbidden_dtl:render();
	    404 ->
		html_notfound_dtl:render();
	    410 ->
		html_gone_dtl:render();
	    500 ->
		maybe_log_serverfault(NReq, Reason),
		html_serverfault_dtl:render();
	    501 ->
		log_unsupported_method(NReq),
		html_notimplemented_dtl:render();
	    502 ->
		log_badgateway(NReq),
		html_badgateway_dtl:render();
	    503 ->
		log_unavailable(NReq),
		html_unavailable_dtl:render();
	    _Other ->
		log_alien_status_code(Req),
		html_serverfault_dtl:render()
	end,
    {Content, NReq};
render("application/json", StatusCode, Req, Reason) ->
    {ok, NReq} = Req:add_response_header("Content-Type", "application/json"),
    {ok, Content} =
	case StatusCode of
	    400 ->
		json_badrequest_dtl:render();
	    403 ->
		json_forbidden_dtl:render();
	    404 ->
		json_notfound_dtl:render();
	    410 ->
		json_gone_dtl:render();
	    500 ->
		maybe_log_serverfault(NReq, Reason),
		json_serverfault_dtl:render();
	    501 ->
		log_unsupported_method(NReq),
		json_notimplemented_dtl:render();
	    502 ->
		log_badgateway(NReq),
		json_badgateway_dtl:render();
	    503 ->
		log_unavailable(NReq),
		json_unavailable_dtl:render();
	    _Other ->
		log_alien_status_code(Req),
		json_serverfault_dtl:render()
	end,
    {Content, NReq};
render(_Other, StatusCode, Req, _Reason) ->
    {ok, NReq} = Req:add_response_header("Content-Type", "text/plain"),
    Resp = ["*Unable to render error message for a ",
	    integer_to_list(StatusCode),
	    " since the specified accept type(s) are not supported"],
    {iolist_to_binary(Resp), NReq}.

%% @private
log_alien_status_code(Req) ->
    error_logger:error_msg("[ERLI] Attepting to generate a response for "
			   "~p failed.~n", [Req:status_code()]).

%% @private
log_unavailable(_Req) ->
    error_logger:error_msg("[WEBMACHINE] Throttle threshold reached.~n").

%% @private
log_badgateway(Req) ->
    error_logger:error_msg("[WEBMACHINE] Bad Gateway: ~p~n", [Req]).

%% @private
log_unsupported_method(Req) ->
    {Method, _} = Req:method(),
    error_logger:error_msg("[WEBMACHINE] Method not supported: ~p~n", [Method]).

%% @private
maybe_log_serverfault(Req, Reason) ->
    {Path, _} = Req:path(),
    case Reason of
	{error, {exit, normal, _Stack}} ->
	    ok;
	_ ->
	    error_logger:error_msg("[WEBMACHINE] Internal Error: path=~p~n~p~n",
				   [Path, Reason])
    end.
