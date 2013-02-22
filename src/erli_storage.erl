%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2012-2013 Moritz Windelen.
%% @doc API to interface with the erli storage backend.

-module(erli_storage).
-author('Moritz Windelen <moritz@tibidat.com>').

%% API
-export([get/1, init/1, put/1, put/2, report/1,
	 visit/2, ban/1, list_paths_for_target/1]).

-include("erli_persistence.hrl").

%%%===================================================================
%%% Management API
%%%===================================================================

%% @doc Bans a URL
ban(Url) ->
    case erli_storage:get({target, Url}) of
	{ok, Target} ->
	    BannedTarget = Target#target{rep_num = ?FLAG_LIMIT + 1},
	    mnesia:transaction(
	      fun () ->
		      mnesia:write(target, BannedTarget, write)
	      end),
	    Paths = list_paths_for_target(Target#target.target),
	    lists:map(fun(P) ->
			      NewPath = P#path{deleted=true},
			      mnesia:dirty_write(path, NewPath)
		      end, Paths),
	    ok;
	Error ->
	    Error
    end.

%% some ideas for further development:
%% list top n paths
%% unban targets
%%

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Initializes the mnesia backend, creating the schema and tables if
%% necessary.
-spec init(Config :: any()) -> ok.
init(_Config) ->
    case is_fresh_startup(node()) of
	{is_fresh, true} ->
	    mnesia:stop(),
	    case mnesia:create_schema([node()]) of
		ok ->
		    ok;
		{error, Reason} ->
		    error_logger:info_msg("Creation of schema failed: ~p~n", [Reason])
	    end,
	    mnesia:start(),
	    create_tables();
	{is_fresh, Tables} ->
	    ok = mnesia:wait_for_tables(Tables, 60000)
    end.

%% @doc Attempt to store a randomly generated path for `TargetUrl' in mnesia.
-spec put(TargetUrl :: binary()) -> {ok, #path{}} | {error, Reason :: atom()}.
put(TargetUrl) ->
    case make_unique_path(TargetUrl) of
	{ok, Path} ->
	    erli_storage:put(TargetUrl, Path);
	{error, Reason} ->
	    {error, Reason}
    end.

%% @doc Attempts to save a new `Path' for `TargetUrl' to mnesia.
-type put_errors() :: conflict | target_banned.
-spec put(TargetUrl :: binary(), Path :: binary()) -> {ok, #path{}} | {error, put_errors()}.
put(TargetUrl, Path) ->
    case erli_storage:get({path, Path}) of
	{error, not_found} ->
	    case erli_storage:get({target, TargetUrl}) of
		{error, not_found} ->
		    ThePath = #path{path = Path, target_url = TargetUrl},
		    {atomic, _Res} =
			mnesia:transaction(
			  fun () ->
				  mnesia:write(target,
					       #target{target=TargetUrl},
					       write),
				  mnesia:write(path, ThePath, write)
			  end),
		    {ok, ThePath};
		{error, Error} ->
		    {error, Error};
		{ok, _Target} ->
		    ThePath = #path{path = Path, target_url = TargetUrl},
		    {atomic, _Res} =
			mnesia:transaction(
			  fun () ->
				  mnesia:write(path, ThePath, write)
			  end),
		    {ok, ThePath}
	    end;
	_PathExistsOrDeleted ->
	    {error, conflict}
    end.

%% @doc Attempts to return the record associated with the search term.
-type rec() :: target | path.
-type key() :: binary().
-type res() :: #target{} | #path{}.
-type get_errors() :: not_found | target_banned | deleted.
-type comp_search() :: {rec(), key()}.
-type search_term() :: comp_search() | binary().
-spec get(search_term()) -> {ok, res()} | {error, get_errors()}.
get({target, TargetUrl}) ->
    case mnesia:dirty_read(target, TargetUrl) of
	[] ->
	    {error, not_found};
	[Res] when Res#target.rep_num < (?FLAG_LIMIT) ->
	    {ok, Res};
	_Res ->
	    {error, target_banned}
    end;
get({path, Path}) ->
    case mnesia:dirty_read(path, Path) of
	[] ->
	    {error, not_found};
	[Res] when Res#path.deleted ->
	    {error, deleted};
	[Res] ->
	    {ok, Res}
    end;
get(Path) ->
    erli_storage:get({path, Path}).

%% @doc Increases the number of reports for the target URL of the path.
-spec report(Path :: binary()) -> request_ignored | ok.
report(Path) ->
    case erli_storage:get({path, Path}) of
	{error, _Error} ->
	    request_ignored;
	{ok, P} ->
	    case erli_storage:get({target, P#path.target_url}) of
		{error, not_found} ->
		    error_logger:info_msg("[ERLI] Path ~p has no associated Target "
					  "record", [P#path.path]),
		    request_ignored;
		{error, target_banned} ->
		    error_logger:info_msg("[ERLI] Trying to report path ~p with "
					  "banned target (~p)", [P#path.path,
								 P#path.target_url]),
		    request_ignored;
		{ok, Target} ->
		    NewTarget = Target#target{rep_num = Target#target.rep_num + 1},
		    case NewTarget#target.rep_num >= (?FLAG_LIMIT) of
			true ->
			    ban(Target);
			false ->
			    mnesia:dirty_write(target, NewTarget)
		    end
	    end
    end.

%% @doc Updates visit statistics for a path.
-spec visit(Path :: binary(), VisitorData :: #visitor{}) -> request_ignored | #path{}.
visit(Path, VisitorData) ->
    case erli_storage:get({path, Path}) of
	{error, _Error} ->
	    request_ignored;
	{ok, P} ->
	    Clicks = P#path.total_clicks,
	    Timeslots = update_timeslots(VisitorData#visitor.hour,
					 P#path.visitors_by_time),
	    Countries =
		sets:add_element(VisitorData#visitor.country_code,
				 P#path.visitors_by_country),
	    UpdatedPath = P#path{total_clicks = Clicks + 1,
				 visitors_by_country = Countries,
				 visitors_by_time = Timeslots},
	    mnesia:dirty_write(path, UpdatedPath),
	    UpdatedPath
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Classifies a request according to the server-time at which it was
%% received and updates the existing request classification for the path.
-spec update_timeslots(Hour :: integer(), #timeslots{}) -> #timeslots{}.
update_timeslots(Hour, #timeslots{night = N, morning = M, afternoon = A,
				  evening = E} = TS) ->
    case Hour of
	H when H =< 6 ->
	    TS#timeslots{night = N + 1};
	H when H > 6, H =< 12 ->
	    TS#timeslots{morning = M + 1};
	H when H > 12, H =< 18 ->
	    TS#timeslots{afternoon = A + 1};
	H when H > 18 ->
	    TS#timeslots{evening = E + 1}
    end.

%% @private
%% @doc Trigger generation of a random path. Generation is based on
%% base64 encoding of a hash and limited by an upper threshhold.
-spec make_unique_path(Url :: binary()) -> {ok, Path :: binary} | {error, path_generation_failed}.
make_unique_path(Url) ->
    make_unique_path(Url, 0).

-spec make_unique_path(Url :: binary(), HashConflicts :: integer()) ->
			      {ok, Path :: binary} | {error, path_generation_failed}.
make_unique_path(TargetUrl, NrOfHashConflicts) ->
    {MegaSec, Sec, MiniSec} = erlang:now(),
    B = <<TargetUrl/bytes, MegaSec, Sec, MiniSec>>,
    Base64Path = string:to_lower(binary_to_list(base64:encode(crypto:md5(B)), 1, 5)),
    CleanPath = re:replace(Base64Path, "[/?=]", "+", [{return, binary}, global]),
    case mnesia:dirty_read(path, CleanPath) of
	[] ->
	    {ok, CleanPath};
	_Res ->
	    if NrOfHashConflicts < (?MAX_CONFLICTS) ->
		    make_unique_path(TargetUrl, NrOfHashConflicts + 1);
	       true ->
		    {error, path_generation_failed}
	    end
    end.

%% @private
%% @doc Returns whether tables have already been created on the node.
%% Modified from: http://goo.gl/GlpV5
-type table_names() :: [atom()].
-spec is_fresh_startup(node()) -> {is_fresh, true} | {is_fresh, table_names()}.
is_fresh_startup(Node) ->
    case mnesia:system_info(tables) of
	[schema] ->
	    {is_fresh, true};
	Tables ->
	    case mnesia:table_info(schema, cookie) of
		{_, Node} ->
		    {is_fresh, Tables};
		_ ->
		    {is_fresh, true}
	    end
    end.

%% @private
%% @doc Creates the necessary mnesia tables.
-spec create_tables() -> [ok].
create_tables() ->
    [create_table(Record, Fields)
     || {Record, Fields} <- [{target, record_info(fields, target)},
			     {path, record_info(fields, path)}]].

-spec create_table(RecordName :: atom(), RecordFields :: [atom()]) -> ok.
create_table(RecordName, RecordFields) ->
    case mnesia:create_table(RecordName,
			     [{attributes, RecordFields},
			      {disc_copies, [node()]}]) of
	{atomic, ok} ->
	    ok;
	{aborted, Cause} ->
	    error_logger:info_msg("[ERLI] creation of table target failed: "
				  "~p~n", [Cause]),
	    ok
    end.

%% @private
%% @doc Returns all records that reference the specified Target
list_paths_for_target(Target) ->
    mnesia:dirty_select(path, [{#path{target_url='$1', _='_'}, [{'=:=', '$1', Target}], ['$_']}]).
