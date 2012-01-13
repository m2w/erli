%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011 Moritz Windelen.

%% @doc Supervisor for the erli application.

-module(erli_sup).
-author('Moritz Windelen <moritz@tibidat.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%%------------------------------------------------------------------------------
%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
%% @end
%%------------------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
%% @end
%%------------------------------------------------------------------------------
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%%------------------------------------------------------------------------------
%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
%% @end
%%------------------------------------------------------------------------------
init([]) ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, Dispatch} = file:consult(filename:join(
                         [filename:dirname(code:which(?MODULE)),
                          "..", "priv", "dispatch.conf"])),
    WebConfig = [
                 {ip, Ip},
                 {port, 8000},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch},
		 {error_handler, erli_error_handler}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    Stats = {erli_stats, {erli_stats, start_link, []},
	     permanent, 5000, worker, [erli_stats]},
    % currently supported are "hour" | "day"
    % the throttle interval indicates over which time frame requests are 
    % throttled => e.g N Requests / ThrottleInterval
    ThrottleInterval = "hour",
    Throttle = {erli_throttle, {erli_throttle, start_link, [ThrottleInterval]},
		permanent, 5000, worker, [erli_throttle]},
    Processes = [Web, Stats, Throttle],
    {ok, { {one_for_one, 10, 10}, Processes} }.
