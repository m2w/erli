%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011-2012 Moritz Windelen.

%% @doc erlydtl based error handlers.

-module(erli_error_handler).
-author('Moritz Windelen <moritz@tibidat.com>').

-export([render_error/3]).

render_error(Code, Req, Reason) ->
    case Req:has_response_body() of
        {true,_} ->
	    Req:response_body();
	{false,_} ->
	    render_error_body(Code, Req:trim_state(), Reason)
    end.

%%%=============================================================================
%%% @doc HTTP status code handlers.
%%%      Currently implements: 400, 403, 404, 500, 501, 502, 503
%%% @end
%%%=============================================================================
render_error_body(400, Req, _Reason) ->
    {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
    {ok, Content} = badrequest_dtl:render(),
    {Content, ReqState};

render_error_body(403, Req, _Reason) ->
    {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
    {ok, Content} = badrequest_dtl:render(),
    {Content, ReqState};

render_error_body(404, Req, _Reason) ->
    {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
    {ok, Content} = notfound_dtl:render(),
    {Content, ReqState};

render_error_body(500, Req, Reason) ->
    {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
    {Path,_} = Req:path(),
    case Reason of
        {error, {exit, normal, _Stack}} ->
            ok;
        _ ->
            error_logger:error_msg("[WEBMACHINE] Internal Error: "
				   "path=~p~n~p~n", [Path, Reason])
    end,
    {ok, Content} = serverfault_dtl:render(),
    {Content, ReqState};

render_error_body(501, Req, _Reason) ->
    {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
    {Method,_} = Req:method(),
    error_logger:error_msg("[WEBMACHINE] Method not supported: ~p~n",
                           [Method]),
    {ok, Content} = notimplemented_dtl:render(),
    {Content, ReqState};

render_error_body(502, Req, _Reason) ->
    {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
    error_logger:error_msg("[WEBMACHINE] Bad Gateway: ~p~n",
                           [Req]),
    {ok, Content} = badgateway_dtl:render(),
    {Content, ReqState};

render_error_body(503, Req, _Reason) ->
    {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
    error_logger:error_msg("[WEBMACHINE] Throttle threshold reached.~n"),
    {ok, Content} = unavailable_dtl:render(),
    {Content, ReqState}.
