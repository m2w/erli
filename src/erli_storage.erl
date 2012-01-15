%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011-2012 Moritz Windelen.
%% @doc The API to the database backed storage of URLs

-module(erli_storage).
-author('Moritz Windelen <moritz@tibidat.com>').

%% API
-export([init/1,
	 put/1,
	 put/2,
	 read/1,
	 delete/1,
	 path_list/0,
	 update_path_stats/5]).

-include_lib("stdlib/include/qlc.hrl").
-include("erli.hrl").

%%------------------------------------------------------------------------------
%% @spec init(_Config) -> ok
%% @doc Initializes the mnesia backend, creating the schema and tables if
%%      necessary.
%% @end
%%------------------------------------------------------------------------------
init(_Config) ->
    case is_fresh_startup(node()) of
        {is_fresh, true} ->
	    mnesia:stop(),
	    mnesia:create_schema([node()]),
	    mnesia:start(),
	    {atomic, ok} = create_tables(),
	    ok;
        {is_fresh, Tables} ->
            ok = mnesia:wait_for_tables(Tables, 60000)
    end.

%%------------------------------------------------------------------------------
%% @spec put(TargetUrl::binary()) -> {ok, target()} |
%%                                   {error, target_banned} |
%%                                   {error, path_generation_failed}
%% @doc Creates a new entry for the target URL or returns the current shortened
%%      path for the URL.
%% @end
%%------------------------------------------------------------------------------
put(TargetUrl) ->
    {atomic, MatchingTarget} = mnesia:transaction(
      fun() ->
	      Table = mnesia:table(target),
	      QueryHandle = qlc:q([T || T <- Table,
					T#target.target =:= TargetUrl]),
	      qlc:eval(QueryHandle)
      end),
    make_path(TargetUrl, MatchingTarget).

%%------------------------------------------------------------------------------
%% @spec put(TargetUrl::binary(), PathS::string()) -> {error, conflict} |
%%                                                    {error, target_banned} |
%%                                                    {ok, target()}
%% @doc Attempts to create a new path for the given target URL using the specified
%%      path.
%% @end
%%------------------------------------------------------------------------------
put(TargetUrl, PathS) ->
    Path = #path{path=PathS},
    case read(PathS) of
	{error, not_found} ->
	    {atomic, MatchingTarget} =
		mnesia:transaction(
		  fun() ->
			  Table = mnesia:table(target),
			  QueryHandle = qlc:q([T || T <- Table,
						    T#target.target =:= TargetUrl]),
			  qlc:eval(QueryHandle)
		  end),
	    case MatchingTarget of
		[T] when T#target.rep_num > ?FLAG_LIMIT ->
		    {error, target_banned};
		[#target{paths=ExistingPaths, _=_} = T] ->
		    NewTarget = T#target{paths=[Path|ExistingPaths]},
		    {atomic, _Ret} = mnesia:transaction(
				       fun() ->
					       mnesia:write(target,
							    NewTarget,
							    write)
				       end),
		    {ok, NewTarget};
		[] ->
		    NewTarget = #target{target=TargetUrl, paths=[Path]},
		    {atomic, _Ret} = mnesia:transaction(
				       fun() ->
					       mnesia:write(target,
							    NewTarget,
							    write)
				       end),
		    {ok, NewTarget}
	    end;
	_ ->
	    {error, conflict}
    end.

%%------------------------------------------------------------------------------
%% @spec read(PathS::string()) -> {ok, target()} |
%%                                {error, target_banned} |
%%                                {error, not_found}
%% @doc Retrieves the complete short_url record for a shortened URL.
%% @end
%%------------------------------------------------------------------------------
read(PathS) ->
    {atomic, Result} = mnesia:transaction(
      fun() ->
	      Table = mnesia:table(target),
	      QueryHandle = qlc:q([T || T <- Table, path_in_target(T#target.paths,
								     PathS)]),
	      qlc:eval(QueryHandle)
      end),
    case Result of
	[T] when T#target.rep_num > ?FLAG_LIMIT ->
	    {error, target_banned};
	[T] ->
	    {ok, T};
	_ ->
	    {error, not_found}
    end.

%%------------------------------------------------------------------------------
%% @spec delete(PathS::string()) -> {ok, target()} |
%%                                  {error, not_found}
%% @doc Flags the URL linked by Path.
%% @end
%%------------------------------------------------------------------------------
delete(PathS) ->
    case read(PathS) of
	{_, #target{rep_num = RepNum, _=_} = Target} ->
	    NewTarget = Target#target{reported=true,
				      rep_num=RepNum+1},
	    ok = mnesia:dirty_write(target, NewTarget),
	    {ok, NewTarget};
	{error, not_found} ->
	    {error, not_found}
    end.

%%------------------------------------------------------------------------------
%% @spec path_list() -> list()
%% @doc Returns a list of all paths.
%% @end
%%------------------------------------------------------------------------------
path_list() ->
    PathRecs = mnesia:dirty_select(target,
				   [{#target{paths='$1', _='_'}, [], ['$1']}]),
    lists:flatten(PathRecs).

%%------------------------------------------------------------------------------
%% @spec update_path_stats(Path::path(), Countries::list(), UniqueIPs::list(),
%%                         ClickCount::integer(), {_Date, {H::integer(), _M, _S}})
%%       -> ok
%% @doc Update a shortened URL's visit statistics.
%% @end
%%------------------------------------------------------------------------------
update_path_stats(Path, Countries, UniqueIPs, ClickCount, {_Date, {H, _M, _S}}) ->
    % grab the target record
    {ok, #target{paths=Paths, _=_} = Target} = read(Path#path.path),
    % filter out the important path (not doing this in the read call leaves it 'cheap'
    {[#path{country_lst=CL, total_clicks=TC, unique_clicks=UC, timeslot_visits=TSV, _=_}=ThePath],
     OtherPaths} =
	lists:partition(fun(P) ->
				Path =:= P
			end,
			Paths),

    % check if there is a new ip for this path
    UniqueClicks = length([IP || IP <- UniqueIPs, is_ip_unique_for_path(Path, IP)]),

    % merge old and new country lists
    CountryUnion = sets:to_list(
		     sets:union(
		       sets:from_list(CL),
		       sets:from_list(Countries)
		      )
		    ),

    % update the path stats
    NewPath = ThePath#path{country_lst=CountryUnion,
			   unique_clicks=UC + UniqueClicks,
			   total_clicks=TC + ClickCount,
			   timeslot_visits=classify_timeslot(H, ClickCount, TSV)},
    NewTarget = Target#target{paths = [NewPath | OtherPaths]},
    % flush to mnesia
    mnesia:dirty_write(NewTarget).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
%%------------------------------------------------------------------------------
%% @private
%% @spec classify_timeslot(Hour::integer(), Clicks::integer(), TS::timeslots())
%%       -> timeslots()
%% @doc Classifies `Clicks' in `Hour' according to its time-slot and increments
%%      the total click counts for the specific time-slot accordingly.
%%      Timeslots are as follows:
%%      night (0-6) | morning (6-12) | afternoon (12-18) | evening (18-24)
%% @end
%%------------------------------------------------------------------------------
classify_timeslot(Hour,
		  Clicks,
		  #timeslots{night=N, morning=M, afternoon=A, evening=E}=TS) ->
    case Hour of
	H when H =< 6 ->
	    TS#timeslots{night=N + Clicks};
	H when H > 6, H =< 12 ->
	    TS#timeslots{morning=M + Clicks};
	H when H > 12, H =< 18 ->
	    TS#timeslots{afternoon=A + Clicks};
	H when H > 18 ->
	    TS#timeslots{evening=E + Clicks}
    end.

%%------------------------------------------------------------------------------
%% @private
%% @spec is_ip_unique_for_path(Path::path(), IP::binary()) -> true | false
%% @doc Returns whether an client with the IP-Address `IP' already visited
%%      `Path'.
%% @end
%%------------------------------------------------------------------------------
is_ip_unique_for_path(Path, IP) ->
    case mnesia:dirty_read(visitor_ip, #visitor_ip{visitor_ip=IP, _='_'}) of
	[] ->
	    mnesia:dirty_write(visitor_ip, #visitor_ip{visitor_ip=IP,
						       paths=[Path]}),
	    true;
	[VisitedPaths] ->
	    case lists:member(Path, VisitedPaths) of
		true ->
		    false;
		false ->
		    mnesia:dirty_write(visitor_ip, #visitor_ip{visitor_ip=IP,
							       paths=[Path|VisitedPaths]}),
		    true
	    end
    end.

%%------------------------------------------------------------------------------
%% @private
%% @spec path_in_target(PathList::list(), SearchPath::path()) -> true |
%%                                                                   false
%% @doc Returns whether the path `SearchPath' is contained in `PathList'.
%% @end
%%------------------------------------------------------------------------------
path_in_target(PathList, SearchPath) ->
    Path = lists:filter(
	     fun(#path{path=ThePath, _=_}) -> ThePath =:= SearchPath end,
			PathList),
    case Path of
	[] ->
	    false;
	_Any ->
	    true
    end.

%%------------------------------------------------------------------------------
%% @private
%% @spec make_path(Url::binary(), MatchingTarget::list())
%%       -> {error, target_banned} |
%%          {ok, path()} |
%%          {error, path_generation_failed}
%% @doc Given a URL create a new target record or update an existing one by
%%      generating a new path record for it.
%% @end
%%------------------------------------------------------------------------------
make_path(Url, MatchingTarget) ->
    case MatchingTarget of
	[T] when T#target.rep_num > ?FLAG_LIMIT ->
	    {error, target_banned};
	[#target{paths=ExistingPaths, _=_}=T] ->
	    case make_unique_path(Url) of
		{error, path_generation_failed} ->
		    {error, path_generation_failed};
		PathS ->
		    Path = #path{path=PathS},
		    NewTarget = T#target{paths=[Path|ExistingPaths]},
		    {atomic, _Ret} = mnesia:transaction(
				       fun() ->
					       mnesia:write(target,
							    NewTarget,
							    write)
				       end),
		    {ok, Path}
	    end;
	[] ->
	    case make_unique_path(Url) of
		{error, path_generation_failed} ->
		    {error, path_generation_failed};
		PathS ->
		    Path = #path{path=PathS},
		    T = #target{target=Url, paths=[Path]},
		    {atomic, _Ret} = mnesia:transaction(
				       fun() ->
					       mnesia:write(target,
							    T,
							    write)
				       end),
		    {ok, Path}
	    end
    end.

%%------------------------------------------------------------------------------
%% @private
%% @spec make_unique_path(URL::binary()) -> {ok, string()} |
%%                                          {error, path_generation_failed}
%% @doc Generate a unique path using the base64 encoded md5 of the URL
%%      concatenated to the current system time.
%% @end
%%------------------------------------------------------------------------------
make_unique_path(Url) ->
    make_unique_path(Url, 0).
make_unique_path(TargetUrl, NrOfHashConflicts) ->
    {MegaSec, Sec, MiniSec} = erlang:now(),
    B = <<TargetUrl/bytes, MegaSec, Sec, MiniSec>>,
    Base64Path = string:to_lower(binary_to_list(base64:encode(crypto:md5(B)), 1, 5)),
    CleanPath = re:replace(Base64Path, "[/?=]","+", [{return, list}, global]),
    case mnesia:dirty_read(target, CleanPath) of
	[] ->
	    {ok, CleanPath};
	_Res ->
	    if
		NrOfHashConflicts < ?MAX_CONFLICTS ->
		    make_unique_path(TargetUrl, NrOfHashConflicts + 1);
		true ->
		    {error, path_generation_failed}
	    end
    end.


%%------------------------------------------------------------------------------
%% @private
%% spec is_fresh_startup(Node::node()) -> {is_fresh, true} |
%%                                        {is_fresh, list()}
%% @doc Returns whether tables have already been created on the node.
%%      Modified from: http://goo.gl/GlpV5
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @private
%% @spec create_tables() -> {atomic, ok}
%% @doc Creates the necessary mnesia tables.
%% @end
%%------------------------------------------------------------------------------
create_tables() ->
    {atomic, ok} = mnesia:create_table(target, [{attributes,
						 record_info(fields, target)},
						{disc_copies, [node()]}]),
    {atomic, ok} = mnesia:create_table(visitor_ip, [{attributes,
						     record_info(fields,
								 visitor_ip)},
						    {disc_copies, [node()]}]).
