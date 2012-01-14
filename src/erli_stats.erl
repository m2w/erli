%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011-2012 Moritz Windelen.

%% @doc gen_server that handles periodic parsing of 
%%      webmachine logfiles to extract usage statistics
%% @end

-module(erli_stats).
-author('Moritz Windelen <moritz@tibidat.com>').

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2, 
	 terminate/2, 
	 code_change/3]).

-include("erli.hrl").

%%------------------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid}
%% @doc Api call to initialize the gen_server.
%% @end
%%------------------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================
init([]) ->
    {ok, {}, 0}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(parse_eval, State) ->
    {ok, App} = application:get_application(),
    PyCmd = "python -u " ++ filename:join([code:priv_dir(App), 
					   "scripts", 
					   ?SCRIPT_NAME]),
    Port = open_port({spawn, PyCmd}, [{packet, 1}, binary, use_stdio]),
    case retrieve_path_stats(Port, erli_storage:path_list()) of
	{parsing_complete, true} ->
	    erlang:send_after(?STAT_COLLECT_INTERVAL, self(), parse_eval);
	{reschedule, Time} ->
	    erlang:send_after(Time, self(), parse_eval)
    end,
    {noreply, State};
handle_info(timeout, State) ->
    erlang:send_after(0, self(), parse_eval), % triggers the initial parsing cycle
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
%%------------------------------------------------------------------------------
%% @private
%% @spec retrieve_path_stats(Port::port(), list()) -> 
%%                                               {parsing_complete, true} |
%%                                               {reschedule, Time::integer()} |
%%                                               {error, timeout}
%% @doc Communicates with the port to recursively collect the statistics for all
%%      paths, updating mnesia accordingly. The port may/can ask the process to
%%      reschedule, for example if there is no log file to parse at this moment.
%% @end
%%------------------------------------------------------------------------------
retrieve_path_stats(Port, [Path | RemPaths]) ->
    port_command(Port, term_to_binary({path, Path#path.path})),
    receive
        {Port, {data, RespData}} ->
	    case binary_to_term(RespData) of
		{reschedule, Time} ->
		    retrieve_path_stats(Port, []), % close the port before
						   % rescheduling 
		    {reschedule, Time};
		{Countries, UniqueIPs, ClickCount, TimeStamp} ->
		    erli_storage:update_path_stats(Path, 
						   Countries, 
						   UniqueIPs, 
						   ClickCount, 
						   TimeStamp),
		    retrieve_path_stats(Port, RemPaths)
	    end
    after
        5000 ->
	    error_logger:warning_msg("[ERLI] ~s timed out on retrieve_path_stats/2"
				     " for path ~s~n", 
				     [?MODULE, Path]),
            {error, timeout}
    end;
retrieve_path_stats(Port, []) ->
    true = port_close(Port),
    {parsing_complete, true}.
