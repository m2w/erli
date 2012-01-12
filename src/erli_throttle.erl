%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011-2012 Moritz Windelen.

%% @doc gen_server that handle throttling related state

% TODO: check if this could/should be merged with erli_util

-module(erli_throttle).
-author('Moritz Windelen <moritz@tibidat.com>').

-behaviour(gen_server).

%% API
-export([start_link/0,
	 throttle_req/0]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include("erli.hrl").

-record(state, {reqs_handled=0, span, last_reset}).

start_link() ->
    case whereis(erli_throttle) of
	undefined ->
	    {ok, Pid} = gen_server:start_link(?MODULE, [?THROTTLE_TIME_SPAN], []),
	    %register the name locally since we can't pass dynamic 
	    %information to our webmachine resource (limited to dispatcher args)
	    register(erli_throttle, Pid), 
	    {ok, Pid};
	Pid ->
	    {ok, Pid}
    end.

throttle_req() ->
    gen_server:call(erli_throttle, is_throttled).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================
init([?THROTTLE_TIME_SPAN]) when is_list(?THROTTLE_TIME_SPAN) ->
    {_Date, {H, M, S} = Time} = calendar:universal_time(),
    % calculate the initial offset to make the interval 'nice'
    case ?THROTTLE_TIME_SPAN of % for now these are hard-coded
	"hour" ->
	    S2Go = 60 - S,
	    M2Go = 60 - M,
	    T = M2Go * 60 + S2Go,
	    erlang:send_after(T * 1000, self(), reset), % trigger the reset cycle
	    {ok, #state{span=3600, 
			last_reset=calendar:time_to_seconds(Time) - S - M * 60}};
	"day" ->
	    T = 86400 - calendar:time_to_seconds({H, M, S}),
	    erlang:send_after(T * 1000, self(), reset),
	    {ok, #state{span=86400, last_reset=86400 - calendar:time_to_seconds(Time)}}
    end;
init([?THROTTLE_TIME_SPAN]) ->
    % you're on your own here...
    {ok, #state{span=?THROTTLE_TIME_SPAN}, 0}.

handle_call(is_throttled, _From, #state{reqs_handled=RH, 
					span=Span, 
					last_reset=LR}=State) ->
    C = RH + 1,
    NewState = State#state{reqs_handled=C},
    if
	C > ?REQ_LIMIT ->
	    {_, CurTime} = calendar:universal_time(),
	    RetryAfter = LR + Span - calendar:time_to_seconds(CurTime),
	    {reply, {true, RetryAfter}, NewState};
	C =< ?REQ_LIMIT ->
	    {reply, false, NewState}
    end.

handle_cast(_Req, State) ->
    {noreply, State}.

% reset the req count after a set timespan
handle_info(reset, State) ->
    error_logger:info_msg("[ERLI] handled a total of ~p requests", 
			  [State#state.reqs_handled]),
    {_, T} = calendar:universal_time(),
    Time = calendar:time_to_seconds(T),
    erlang:send_after(State#state.span * 1000, self(), reset),
    {noreply, State#state{reqs_handled=0, last_reset=Time}};
handle_info(timeout, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





