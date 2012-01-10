%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011 Moritz Windelen.
%% @doc The API to the database backed storage of URLs

-module(erli_storage).

%% API
-export([init/1,
	 put/1,
	 put/2,
	 read/1,
	 delete/1,
	 path_list/0,
	 update_path_stats/4]).

-include_lib("stdlib/include/qlc.hrl").
-include("erli.hrl").

%%------------------------------------------------------------------------------
%% @doc Initializes the mnesia backend, includes setting up necessary tables, etc.
%%------------------------------------------------------------------------------
init(_Config) ->
    case is_fresh_startup(node()) of
        true -> 
	    mnesia:stop(),
	    mnesia:create_schema([node()]),
	    mnesia:start(),
	    create_tables();
        {exists, Tables} -> 
            ok = mnesia:wait_for_tables(Tables, 60000) 
    end.

%%------------------------------------------------------------------------------
%% @doc Creates a new entry for the target URL or returns the current shortened 
%%      path for the URL.
%% @spec put(Target :: binary) -> {ok, #target} | {target_banned, #target}| error 
%% @end
%%------------------------------------------------------------------------------
put(TargetUrl) ->
    {atomic, MatchingTarget} = mnesia:transaction(
      fun() -> 
	      Table = mnesia:table(target),
	      QueryHandle = qlc:q([T || T <- Table, 
					T#target.target =:= binary_to_list(TargetUrl)]),
	      qlc:eval(QueryHandle)
      end),
    make_target(TargetUrl, MatchingTarget).

put(TargetUrl, PathS) ->
    Path = #path{path=PathS},
    case read(PathS) of
	not_found ->
	    {atomic, MatchingTarget} = 
		mnesia:transaction(
		  fun() -> 
			  Table = mnesia:table(target),
			  QueryHandle = qlc:q([T || T <- Table, 
						    T#target.target =:= binary_to_list(TargetUrl)]),
			  qlc:eval(QueryHandle)
		  end),
	    case MatchingTarget of
		[T] when T#target.rep_num > ?FLAG_LIMIT ->
		    target_banned;
		[#target{paths=[ExistingPaths], _=_} = T] ->
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
	    conflict
    end.
    

%%------------------------------------------------------------------------------
%% @doc Retrieves the complete short_url record for a shortened URL.
%% @spec read(Path :: Binary) -> {ok, #target} | {target_banned, #target} | not_found
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
	    {target_banned, T};
	[T] ->
	    {ok, T};
	_ ->
	    not_found
    end.

%%------------------------------------------------------------------------------
%% @doc Flags the URL linked by Path.
%% @spec delete(Path :: Binary) -> {ok, #target} | not_found
%% @todo implement
%% @end
%%------------------------------------------------------------------------------
delete(PathS) ->
    case read(PathS) of
	{_, #target{rep_num = RepNum, _=_} = Target} ->
	    NewTarget = Target#target{reported=true, 
				      rep_num=RepNum+1},
	    ok = mnesia:dirty_write(target, NewTarget),
	    {ok, NewTarget};
	not_found ->
	    not_found
    end.

%%------------------------------------------------------------------------------
%% @doc Returns a list of all current paths.
%% @end
%%------------------------------------------------------------------------------
path_list() ->
    PathRecs = mnesia:dirty_select(target, [{#target{paths='$1', _='_'}, [], ['$1']}]),
    lists:flatten(PathRecs).

%%------------------------------------------------------------------------------
%% @doc Update a shortened URL's visit statistics
%% @end
%%------------------------------------------------------------------------------
update_path_stats(Path, Countries, UniqueIPs, ClickCount) ->
    % grab the target record
    {ok, #target{paths=Paths, _=_} = Target} = read(Path#path.path),
    % filter out the important path (not doing this in the read call leaves it 'cheap'
    {[#path{country_lst=CL, total_clicks=TC, unique_clicks=UC, _=_}=ThePath], 
     OtherPaths} = 
	lists:partition(fun(P) -> 
				Path =:= P 
			end, 
			Paths),

    % check if there is a new ip for this path
    UniqueClicks = length([IP || IP <- UniqueIPs, is_unique_for_path(Path, IP)]),

    % merge old and new country lists
    CountryUnion = sets:to_list(
		     sets:union(
		       sets:from_list(CL), 
		       sets:from_list(Countries)
		      )
		    ),
    % update the path stats
    NewPath = ThePath#path{country_lst = CountryUnion, 
			   unique_clicks = UC + UniqueClicks,
			   total_clicks = TC + ClickCount},
    NewTarget = Target#target{paths = [NewPath | OtherPaths]},
    % flush to mnesia
    mnesia:dirty_write(NewTarget).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
is_unique_for_path(Path, IP) ->
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

path_in_target(TargetPaths, SearchPath) ->
    Path = lists:filter(
	     fun(#path{path=ThePath, _=_}) -> ThePath =:= SearchPath end, 
			TargetPaths),
    case Path of
	[] ->
	    false;
	_ ->
	    true
    end.

make_target(TargetUrl, MatchingTarget) ->
    case MatchingTarget of
	[T] when T#target.rep_num > ?FLAG_LIMIT ->
	    target_banned;
	[#target{paths=[ExistingPaths], _=_} = T] ->
	    case make_unique_path(TargetUrl) of
		error ->
		    error;
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
	    case make_unique_path(TargetUrl) of
		error ->
		    error;
		PathS ->
		    Path = #path{path=PathS},
		    T = #target{target=TargetUrl, paths=[Path]},
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
%% @doc Generate a unique path using the base64 encoded md5 of the current time +
%%      the target URL.
%% @end
%%------------------------------------------------------------------------------

make_unique_path(TargetUrl) ->
    make_unique_path(TargetUrl, 0).
make_unique_path(TargetUrl, NrOfHashConflicts) ->
    {MegaSec, Sec, MiniSec} = erlang:now(),
    B = <<TargetUrl/bytes, MegaSec, Sec, MiniSec>>,
    Base64Path = string:to_lower(binary_to_list(base64:encode(crypto:md5(B)), 1, 5)),
    CleanPath = re:replace(Base64Path, "[/?=]","+", [{return, list}, global]),
    case mnesia:dirty_read(target, CleanPath) of
	[] ->
	    CleanPath;
	_Res ->
	    if 
		NrOfHashConflicts < ?MAX_CONFLICTS ->
		    make_unique_path(TargetUrl, NrOfHashConflicts + 1);
		true ->
		    error
	    end
    end.

% modified from: http://erlang.2086793.n4.nabble.com/When-to-create-mnesia-schema-for-OTP-applications-td2115607.html
is_fresh_startup(Node) ->
    case mnesia:system_info(tables) of 
        [schema] ->
	    true;
        Tbls -> 
            case mnesia:table_info(schema, cookie) of 
                {_, Node} -> 
		    {exists, Tbls}; 
                _ -> 
		    true 
            end 
    end.

create_tables() ->
    mnesia:create_table(target, [{attributes, record_info(fields, target)},
				 {disc_copies, [node()]}]),
    mnesia:create_table(visitor_ip, [{attributes, record_info(fields, visitor_ip)},
				     {disc_copies, [node()]}]).
