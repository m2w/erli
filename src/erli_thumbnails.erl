%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc A gen_server that functions as port to phantomjs
%%% to generate thumbnails for target resources.
%%% @end
%%%==========================================================

-module(erli_thumbnails).

-behaviour(gen_server).

%% API
-export([start/0,
	 generate_thumbnails/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("models.hrl").

%%-----------------------------------------------------------
%% API Methods
%%-----------------------------------------------------------

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

generate_thumbnails() ->
    error_logger:info_msg("[INFO] Initiating thumbnail collection"),
    gen_server:cast(?SERVER, gen_thumbs).

%%-----------------------------------------------------------
%% gen_server Callbacks
%%-----------------------------------------------------------

init([]) ->
    Priv = erli_utils:priv_dir(?MODULE),
    Exec = filename:join([Priv, <<"screen_grab.js">>]),
    Dir = filename:join([Priv, erli_utils:get_env(thumbnail_dir)]),
    {ok, {Exec, Dir}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(gen_thumbs, State) ->
    Targets = erli_storage:targets_requiring_thumbnail(),
    grab(Targets, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------
%% Internal Methods
%%-----------------------------------------------------------

-spec grab([#target{}], {binary(), binary()}) -> ok.
grab([T|Targets], {Exe, ThumbnailDir} = Ctx) ->
    TUrl = T#target.url,
    ThumbnailPath = filename:join([ThumbnailDir,
				   <<(T#target.id)/bitstring, ".jpeg">>]),
    Cmd = <<Exe/bitstring, " ",  TUrl/bitstring, " ", ThumbnailPath/bitstring>>,
    case erli_utils:run(binary_to_list(Cmd)) of
	{0, _} ->
	    error_logger:info_msg("[INFO] Generated thumbnail"),
	    erli_storage:thumbnail_generated(T);
	{_, _ErrorDesc} ->
	    error_logger:info_msg(
	      "[ERROR] Generation of a thumbnail for ~s failed~n", [TUrl])
    end,
    grab(Targets, Ctx);
grab([], _) ->
    error_logger:info_msg("[INFO] Finished thumbnail generation"),
    ok.
