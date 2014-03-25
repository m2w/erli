%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc The erli supervisor.
%%% @end
%%%==========================================================

-module(erli_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,
         upgrade/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%-----------------------------------------------------------
%% API Methods
%%-----------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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

%%-----------------------------------------------------------
%% Supervisor Callbacks
%%-----------------------------------------------------------

init([]) ->
    {ok, Ip} = application:get_env(webmachine, ip),
    {ok, Port} = application:get_env(webmachine, port),
    {ok, App} = application:get_application(?MODULE),
    DispatchDir = case application:get_env(webmachine, dispatch_dir,
                                           erli_utils:priv_dir(App)) of
                      {ok, Dir} -> Dir;
                      Dir -> Dir
                  end,
    {ok, Dispatch} = file:consult(filename:join([DispatchDir, "dispatch.conf"])),
    Config = [{ip, Ip},
              {port, Port},
              {log_dir, "priv/log"},
              {dispatch, Dispatch}],
    Webmachine = {webmachine_mochiweb,
                  {webmachine_mochiweb, start, [Config]},
                  permanent, 5000, worker, [mochiweb_socket_server]},
    Thumbs = {erli_thumbnails,
                  {erli_thumbnails, start, []},
                  permanent, 5000, worker, []},
    Processes = [Webmachine, Thumbs],
    {ok, {{one_for_one, 10, 10}, Processes}}.
