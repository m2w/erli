%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011 Moritz Windelen.

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
    PyCmd = "python -u " ++ filename:join([code:priv_dir(App), "scripts", ?SCRIPT_NAME]),
    Port = open_port({spawn, PyCmd}, [{packet, 1}, binary, use_stdio]),
    case grab_path_stats(Port, erli_storage:path_list()) of
	true ->
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
grab_path_stats(Port, [Path | RemPaths]) ->
    port_command(Port, term_to_binary({path, Path#path.path})),
    receive
        {Port, {data, RespData}} ->
	    case binary_to_term(RespData) of
		{reschedule, Time} ->
		    grab_path_stats(Port, []),
		    {reschedule, Time};
		{Countries, UniqueIPs, ClickCount} ->
		    erli_storage:update_path_stats(Path, Countries, UniqueIPs, ClickCount),
		    grab_path_stats(Port, RemPaths)
	    end
    after
        5000 ->
	    error_logger:warning_msg("[ERLI] ~s timed out on grab_path_stats/2"
				     " for path ~s~n", 
				     [?MODULE, Path]),
            {error, timeout}
    end;
grab_path_stats(Port, []) ->
    port_close(Port).
