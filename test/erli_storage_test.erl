%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2012-2013 Moritz Windelen.

%% @doc Test suite for erli utility methods.

-module(erli_storage_test).

-include_lib("eunit/include/eunit.hrl").
-include("erli_persistence.hrl").

-define(T, #timeslots{night=13, morning=25, afternoon=41, evening=89}).
-define(P, #path{total_clicks=1337,
		 visitors_by_country=sets:from_list(["UK", "DE", "JP"]),
		 visitors_by_time=#timeslots{}}).

setup_mock_db() ->
    meck:new(mnesia),
    meck:expect(mnesia,
		dirty_read,
		fun(Table, Key) ->
			case Table of
			    path -> mock_paths(Key);
			    target -> mock_targets(Key)
			end
		end),
    meck:expect(mnesia, dirty_write, fun(_Table, _Value) -> ok end),
    meck:expect(mnesia, transaction, fun(_Input) -> {atomic, value} end),
    meck:expect(mnesia, delete, fun(_Table, _Key, _Lock) -> ok end).

cleanup(_Args) ->
    meck:unload(mnesia).

get_test_() ->
    {foreach,
     fun() -> setup_mock_db() end,
     fun(Args) -> cleanup(Args) end,
     [?_assertEqual({error, not_found},
		    erli_storage:get({path, <<"doesnotexist">>})),
      ?_assertEqual({ok, #path{path= <<"path1">>}},
		    erli_storage:get({path, <<"path1">>})),
      ?_assertEqual({ok, #path{path= <<"path1">>}},
		    erli_storage:get(<<"path1">>)),
      ?_assertEqual({ok, #target{target= <<"http://target1.com">>, rep_num=4}},
		    erli_storage:get({target, <<"http://target1.com">>})),
      ?_assertEqual({error, target_banned},
		    erli_storage:get({target, <<"http://target2.com">>})),
      ?_assertEqual({error, not_found},
		    erli_storage:get({target, <<"http://idonotexist.com">>}))]}.

visit_test_() ->
    {foreachx,
     fun(_X) -> setup_mock_db() end,
     fun(_X, Args) -> cleanup(Args) end,
     [{void,
       fun(_Nothing, _Args) ->
	       ?_assertEqual(request_ignored, erli_storage:visit(<<"doesnotexist">>, #visitor{}))
       end},
      {{<<"path2">>, "UK", {evening, 19}},
       fun({Path, Country, {Slot, Hour}}, _Args) ->
	       ?_assertEqual(visit_path(Path, Country, Slot),
			     erli_storage:visit(Path,
						#visitor{country_code=Country, hour=Hour}))
       end},
      {{<<"path2">>, "IN", {morning, 7}},
       fun({Path, Country, {Slot, Hour}}, _Args) ->
	       ?_assertEqual(visit_path(Path, Country, Slot),
			     erli_storage:visit(Path,
						#visitor{country_code=Country, hour=Hour}))
       end},
      {{<<"path2">>, "UK", {night, 2}},
       fun({Path, Country, {Slot, Hour}}, _Args) ->
	       ?_assertEqual(visit_path(Path, Country, Slot),
			     erli_storage:visit(Path,
						#visitor{country_code=Country, hour=Hour}))
       end},
      {{<<"path2">>, "US", {afternoon, 16}},
       fun({Path, Country, {Slot, Hour}}, _Args) ->
	       ?_assertEqual(visit_path(Path, Country, Slot),
			     erli_storage:visit(Path,
						#visitor{country_code=Country, hour=Hour}))
       end}]}.

report_test_() ->
    {foreach,
     fun() -> setup_mock_db() end,
     fun(Args) -> cleanup(Args) end,
     [?_assertEqual(request_ignored,
		    erli_storage:report(<<"doesnotexist">>)),
      ?_assertEqual(request_ignored,
		    erli_storage:report(<<"path3">>)),
      ?_assertEqual(ok, erli_storage:report(<<"path4">>)),
      ?_assertEqual(ok, erli_storage:report(<<"path6">>)),
      ?_assertEqual(request_ignored, erli_storage:report(<<"path5">>))]}.

put_test_() ->
    {foreachx,
    fun(_X) -> setup_mock_db() end,
    fun(_X, Args) -> cleanup(Args) end,
    [{{<<"http://target1.com">>, <<"path1">>},
      fun({Target, Path}, _Args) ->
	      ?_assertEqual({error, conflict}, erli_storage:put(Target, Path))
      end},
     {{<<"http://target1.com">>, <<"new">>},
      fun({Target, Path}, _Args) ->
	     ?_assertEqual({ok, #path{path=Path, target_url=Target}},
			   erli_storage:put(Target, Path))
     end},
    {<<"http://target1.com">>,
      fun(Target, _Args) ->
	     ?_assertEqual(5,
			   get_generated_path_len(erli_storage:put(Target)))
      end},
    {<<"http://newtarget.com">>,
      fun(Target, _Args) ->
	      true = is_target_equal(Target, erli_storage:put(Target)),
	      ?_assertEqual(5,
			    get_generated_path_len(erli_storage:put(Target)))
      end}]}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
mock_paths(Key) ->
    case Key of
	<<"path1">> = P ->
	    [#path{path=P}];
	<<"path2">> = Path ->
	    P = ?P,
	    [P#path{path=Path, visitors_by_time=?T}];
	<<"path3">> = P->
	    [#path{path=P, target_url= <<"doesnotreallyexist">>}];
	<<"path4">> = P ->
	    [#path{path=P, target_url= <<"http://target1.com">>}];
	<<"path5">> = P ->
	    [#path{path=P, target_url= <<"http://target2.com">>}];
	<<"path6">> = P ->
	    [#path{path=P, target_url= <<"http://target3.com">>}];
	_Other ->
	    []
    end.

mock_targets(Key) ->
    case Key of
	<<"http://target1.com">> = T ->
	    [#target{target=T, rep_num=4}];
	<<"http://target2.com">> = T ->
	    [#target{target=T, rep_num=1000}];
	<<"http://target3.com">> = T ->
	    [#target{target=T, rep_num=2}];
	_Other ->
	    []
    end.

visit_path(Path, Country, Slot) ->
    T = ?T,
    NT = update_timeslot(T, Slot),
    P = ?P,
    NPath = P#path{path=Path},
    update_path(NPath, Country, NT).

update_path(#path{total_clicks=TC, visitors_by_country=VC}=Path, Country, Timeslots) ->
    Path#path{total_clicks=TC+1,
	      visitors_by_time=Timeslots,
	      visitors_by_country=sets:add_element(Country, VC)}.

update_timeslot(#timeslots{night=N}=T, night) ->
    T#timeslots{night=N+1};
update_timeslot(#timeslots{morning=M}=T, morning) ->
    T#timeslots{morning=M+1};
update_timeslot(#timeslots{afternoon=A}=T, afternoon) ->
    T#timeslots{afternoon=A+1};
update_timeslot(#timeslots{evening=E}=T, evening) ->
    T#timeslots{evening=E+1}.

get_generated_path_len({ok, Path}) ->
    length(binary_to_list(Path#path.path)).

is_target_equal(Target, {ok, Path}) ->
    Target =:= Path#path.target_url.
