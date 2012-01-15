%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011-2012 Moritz Windelen.

%% @doc gen_server that handles periodic parsing of
%%      webmachine logfiles to extract usage statistics.
%% @end

-module(erli_stats).
-author('Moritz Windelen <moritz@tibidat.com>').

-behaviour(gen_server).

%% API
-export([start_link/0,
	 start_link/1]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include("erli.hrl").

-record(state, {script_dir, script_file, interval}).

%%------------------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid}
%% @doc Api call to initialize the gen_server with default values.
%%      Uses the scripts/parse_logs.py script to parse the webmachine access
%%      logs every 3600000 ms (i.e. every hour).
%% @end
%%------------------------------------------------------------------------------
start_link() ->
    start_link(["scripts", "parse_logs.py", 3600000]).

%%------------------------------------------------------------------------------
%% @spec start_link(list()) -> {ok, Pid}
%% @doc Api call to initialize the gen_server with custom values.
%%      Notes:
%%      ScriptDir is the relative path of the script containing dir to
%%      code:priv_dir/1
%%      ScriptFile is the filename of the script to parse the logs
%%      Interval represents the amount of time between calls to the
%%      script, in ms. (Note that the current implementation does <b>NOT</b>
%%      prevent calling the parser multiple times during an hour, resulting in
%%      inflated visitor counts.
%% @end
%%------------------------------------------------------------------------------
start_link([ScriptDir, ScriptFile, Interval]) ->
    gen_server:start_link(?MODULE, [ScriptDir, ScriptFile, Interval], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================
init([Dir, File, Interval]) ->
    {ok, #state{script_dir=Dir, script_file=File, interval=Interval}, 0}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(parse_eval, State) ->
    {ok, App} = application:get_application(),
    PyCmd = "python -u " ++ filename:join([code:priv_dir(App),
					   State#state.script_dir,
					   State#state.script_file]),
    Port = open_port({spawn, PyCmd}, [{packet, 1}, binary, use_stdio]),
    case retrieve_path_stats(Port, erli_storage:path_list()) of
	{parsing_complete, true} ->
	    erlang:send_after(State#state.interval, self(), parse_eval);
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
