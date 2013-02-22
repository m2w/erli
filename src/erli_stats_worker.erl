%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2012-2013 Moritz Windelen.
%% @doc Poolboy worker to run an IP address against egeoip and record
%% the visit in mnesia.

-module(erli_stats_worker).
-author('Moritz Windelen <moritz@tibidat.com>').

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-include("erli_persistence.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init(_Args) ->
    {ok, #state{}}.

%% @private
%% @doc Handling call messages
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
%% @doc Handling cast messages
handle_cast({visit, {Path, Peer}}, State) ->
    {ok, GeoipRecord} = egeoip:lookup(Peer),
    CountryCode = case egeoip:get(GeoipRecord, country_code) of
		      "" ->
			  <<"unknown">>;
		      Code ->
			  Code
		  end,
    {_, {Hour, _, _}} = calendar:now_to_universal_time(now()),
    erli_storage:visit(Path,
		       #visitor{country_code = CountryCode, hour = Hour}),
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
handle_info(_Info, State) -> {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
terminate(_Reason, _State) -> ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) -> {ok, State}.
