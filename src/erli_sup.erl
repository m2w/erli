%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2012-2013 Moritz Windelen.
%% @doc The root supervisor of the erli application.

-module(erli_sup).
-author('Moritz Windelen <moritz@tibidat.com>').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the erli supervisor
-spec start_link() -> application:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Initializes the supervisor tree for erli.
-spec init(Args :: term()) ->
		  {ok, {{RestartStrategy :: atom(), MaxR :: integer(),
			 MaxT :: integer()}, [ChildSpec :: supervisor:child_spec()]}} | ignore.
init(_Args) ->
    Pools = erli_utils:get_env(pools),
    PoolSpecs = [gen_pool_spec(P) || P <- Pools],
    IP = case os:getenv("WEBMACHINE_IP") of
	     false -> "0.0.0.0";
	     Any -> Any
	 end,
    {ok, App} = application:get_application(),
    {ok, Dispatcher} =
	file:consult(filename:join(code:priv_dir(App),
				   "dispatch.conf")),
    WebmachineConfig = [{ip, IP}, {port, 8000},
			{dispatch, Dispatcher},
			{error_handler, erli_error_handler}],
    Webmachine = {webmachine_mochiweb,
		  {webmachine_mochiweb, start, [WebmachineConfig]},
		  permanent, 5000, worker, [mochiweb_socket_server]},
    {ok, {{one_for_one, 10, 10}, [Webmachine | PoolSpecs]}}.

%% @private
%% @doc Generates poolboy child specs from a pool spec
-spec gen_pool_spec({Name :: atom(),
		     [SizeArgs :: tuple()],
		     [WorkerArgs :: tuple()]}) ->
			   supervisor:child_spec().
gen_pool_spec({Name, SizeArgs, WorkerArgs}) ->
    PoolArgs = [{name, {local, Name}},
		{worker_module, erli_stats_worker}]
	++ SizeArgs,
    poolboy:child_spec(Name, PoolArgs, WorkerArgs).
