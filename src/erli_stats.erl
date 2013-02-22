%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2012-2013 Moritz Windelen.
%% @doc API to interface with the erli stats workerpool

-module(erli_stats).
-author('Moritz Windelen <moritz@tibidat.com>').

%% API
-export([record_visit/2]).

-include_lib("webmachine/include/webmachine.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Triggers a call to the erli_stats worker pool to record the visit
%% to a shortened path.
-spec record_visit(Path :: binary(), #wm_reqdata{}) -> any().
record_visit(Path, #wm_reqdata{peer = Peer, _ = _} = _ReqData) ->
    poolboy:transaction(stats_pool,
			fun (Worker) ->
				gen_server:cast(Worker, {visit, {Path, Peer}})
			end).
