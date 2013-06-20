%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc Test suite for functional testing of the path
%%% resource.
%%% @end
%%%==========================================================

-module(path_resource_SUITE).

-compile(export_all).

-include("models.hrl").
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    Priv = ?config(priv_dir, Config),

    application:set_env(mnesia, dir, Priv),
    application:set_env(webmachine, dispatch_dir, DataDir),
    erli:start(),

    {ok, DefaultOffset} = application:get_env(erli, default_collection_offset),
    {ok, MaxOffset} = application:get_env(erli, max_collection_offset),
    {ok, Port} = application:get_env(webmachine, port),

    PortS = integer_to_list(Port),
    RootUrl = "http://localhost:" ++ PortS ++ "/api/paths/",

    [{default_offset, DefaultOffset}, {max_offset, MaxOffset},
     {port, PortS}, {port_int, Port}, {root_url, RootUrl} | Config].

end_per_suite(_Config) ->
    erli:stop().

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    test_utils:clear_all_mnesia_data().

groups() ->
    [].

all() ->
    [idempotent_calls_to_collection,
     idempotent_calls_to_empty_collection,
     post_json_data,
     idempotent_calls_to_entity,
     relationship_lookups,
     delete_entity].

relationship_lookups(Config) ->
    T = #target{url= <<"http://google.com">>},
    Target = erli_storage:create(T),
    Path = test_utils:generate_paths(1, Target),
    Visits = test_utils:generate_visits(35, Path),
    ExpectedMethods = [<<"GET">>, <<"HEAD">>, <<"OPTIONS">>],
    Id = (hd(Path))#path.id,
    Tid = Target#target.id,

    %% OPTIONS
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, OptionsHeaders, []}} =
	httpc:request(options, {?config(root_url, Config) ++
				    binary_to_list(Id) ++ "/visits",
				[]}, [], []),
    "0" = proplists:get_value("content-length", OptionsHeaders),
    "visits" = proplists:get_value("accept-ranges", OptionsHeaders),
    Methods = re:split(proplists:get_value("allow", OptionsHeaders), ",?\s"),
    true = lists:all(fun(M) -> lists:member(M, ExpectedMethods) end, Methods),

    %% Ranges
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers, Body}} =
	httpc:request(get, {?config(root_url, Config) ++
				binary_to_list(Id) ++ "/visits",
			    []}, [], []),
    B = jsx:decode(list_to_binary(Body)),
    Default = ?config(default_offset, Config),
    Default = length(proplists:get_value(<<"visits">>, B)),
    Meta = proplists:get_value(<<"meta">>, B),
    test_utils:validate_meta(35, 25, 0,
			     Default,
			     ?config(max_offset, Config), Meta),

    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers1, Body1}} =
	httpc:request(get, {?config(root_url, Config) ++
				binary_to_list(Id) ++ "/target",
			    [{"Range", "paths=4-"}]}, [], []),
    B1 = jsx:decode(list_to_binary(Body1)),
    Tid = proplists:get_value(<<"id">>, proplists:get_value(<<"targets">>, B1)).

idempotent_calls_to_collection(Config) ->
    test_utils:generate_paths(30),

    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers, Body}} =
	test_utils:build_request(get, Config),
    B = jsx:decode(list_to_binary(Body)),
    DefaultOffset = ?config(default_offset, Config),
    DefaultOffset = length(proplists:get_value(<<"paths">>, B)),
    Meta = proplists:get_value(<<"meta">>, B),
    test_utils:validate_meta(30, 25, 0,
		  DefaultOffset,
		  ?config(max_offset, Config), Meta),

    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers1, Body1}} =
	test_utils:build_request(get, [{"Range", "paths=3-18"}], Config),
    B1 = jsx:decode(list_to_binary(Body1)),
    Meta1 = proplists:get_value(<<"meta">>, B1),
    test_utils:validate_meta(30, 15, 3, 18, ?config(max_offset, Config), Meta1),

    {ok, {{"HTTP/1.1", 400, _400ReasonPhrase}, _400Headers, []}} =
	test_utils:build_request(get, [{"Range", "paths=100-90"}], Config).

idempotent_calls_to_empty_collection(Config) ->
    ExpectedMethods = [<<"GET">>, <<"POST">>, <<"HEAD">>, <<"OPTIONS">>],

    %% GET
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, Headers, Body}} =
	test_utils:build_request(get, Config),
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, Headers, []}} =
	test_utils:build_request(head, Config),
    "application/json" = proplists:get_value("content-type", Headers),
    "paths 0-0/0" = proplists:get_value("content-range", Headers),
    B = jsx:decode(list_to_binary(Body)),
    [] = proplists:get_value(<<"paths">>, B),
    Meta = proplists:get_value(<<"meta">>, B),
    test_utils:validate_meta(0, 0, 0, 0, ?config(max_offset, Config), Meta),

    %% OPTIONS
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, OptionsHeaders, []}} =
	test_utils:build_request(options, Config),
    "0" = proplists:get_value("content-length", OptionsHeaders),
    "paths" = proplists:get_value("accept-ranges", OptionsHeaders),
    Methods = re:split(proplists:get_value("allow", OptionsHeaders), ",?\s"),
    true = lists:all(fun(M) -> lists:member(M, ExpectedMethods) end, Methods).

post_json_data(Config) ->
    TargetUrl = <<"http://google.com">>,
    Target = erli_storage:create(#target{url=TargetUrl}),

    Payload = jsx:encode([{<<"target_id">>, Target#target.id}]),
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers, Body}} =
	test_utils:post_request(Payload, Config),
    B = jsx:decode(list_to_binary(Body)),
    Id = proplists:get_value(<<"id">>, proplists:get_value(<<"paths">>, B)),
    StoredObj = erli_storage:read(path, Id),
    true = is_record(StoredObj, path),

    CustomId = <<"custom-id">>,
    Payload1 = jsx:encode([{<<"target_id">>, Target#target.id},
			  {<<"custom_id">>, CustomId}]),
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers1, Body1}} =
	test_utils:post_request(Payload1, Config),
    B1 = jsx:decode(list_to_binary(Body1)),
    ChosenId = proplists:get_value(<<"id">>,
				   proplists:get_value(<<"paths">>, B1)),
    StoredObj2 = erli_storage:read(path, CustomId),
    true = is_record(StoredObj2, path),

    %% invalid target id
    InvalidUrl = <<"asdf">>,
    Payload2 = jsx:encode([{<<"target_id">>, <<"does-not-exist">>}]),
    {ok, {{"HTTP/1.1", 422, _422ReasonPhrase}, _Headers2, Body2}} =
	test_utils:post_request(Payload2, Config),
    [{<<"formErrors">>,
      [{<<"target_id">>,
	[<<"target record with id 'does-not-exist' does not exist">>]}]}] =
	jsx:decode(list_to_binary(Body2)),

    %% missing required field
    Payload3 = jsx:encode([{<<"rubbish">>, <<>>}]),
    {ok, {{"HTTP/1.1", 422, _422ReasonPhrase}, _Headers3, Body3}} =
	test_utils:post_request(Payload3, Config),
    [{<<"formErrors">>,
      [{<<"target_id">>, [<<"field is required">>]}]}] =
	jsx:decode(list_to_binary(Body3)),

    %% conflicting id
    Payload4 = jsx:encode([{<<"target_id">>, Target#target.id},
			  {<<"custom_id">>, CustomId}]),
    %% TODO: validate the returned conflic message.
    {ok, {{"HTTP/1.1", 409, _409ReasonPhrase}, _Headers4, _Body4}} =
	test_utils:post_request(Payload4, Config).


idempotent_calls_to_entity(Config) ->
    ExpectedMethods = [<<"GET">>, <<"DELETE">>, <<"HEAD">>, <<"OPTIONS">>],
    Url = <<"http://erlang.org">>,
    BannedUrl = <<"http://example.org">>,
    Target = erli_storage:create(#target{url=Url}),
    Path = erli_storage:create(#path{target_id=Target#target.id}),

    T = #target{url=BannedUrl, is_banned=true, flag_count=9000},
    BannedTarget = erli_storage:create(T),
    BannedPath = erli_storage:create(#path{target_id=BannedTarget#target.id,
					  is_banned=true}),

    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, Headers, Body}} =
	test_utils:build_request(get, Path#path.id, Config),
    B = jsx:decode(list_to_binary(Body)),
    Rels = proplists:get_value(
	     <<"rels">>, proplists:get_value(<<"paths">>, B)),
    Id = Target#target.id,
    TUrl = proplists:get_value(<<"target">>, Rels),
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, Headers1, Body1}} =
	httpc:request(
	  get,
	  {"http://localhost:" ++ ?config(port, Config) ++ binary_to_list(TUrl),
	   []}, [], []),

    B1 = jsx:decode(list_to_binary(Body1)),
    Id = proplists:get_value(<<"id">>, proplists:get_value(<<"targets">>, B1)),

    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, Headers, []}} =
	test_utils:build_request(head, Path#path.id, Config),
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, OptionsHeaders, []}} =
	test_utils:build_request(options, Path#path.id, Config),

    "0" = proplists:get_value("content-length", OptionsHeaders),
    Methods = re:split(proplists:get_value("allow", OptionsHeaders), ",?\s"),
    true = lists:all(fun(M) -> lists:member(M, ExpectedMethods) end, Methods),

    %% missing resource
    {ok, {{"HTTP/1.1", 404, _404ReasonPhrase}, _404Headers, _404Body}} =
	test_utils:build_request(get, <<"rubbish-id">>, Config),
    {ok, {{"HTTP/1.1", 404, _404ReasonPhrase}, _404Headers, []}} =
	test_utils:build_request(head, <<"rubbish-id">>, Config),
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, OptionsHeaders, []}} =
	test_utils:build_request(options, <<"rubbish-id">>, Config),

    %% banned resource
    {ok, {{"HTTP/1.1", 410, _410ReasonPhrase}, _410Headers, _Body410}} =
	test_utils:build_request(get, BannedPath#path.id, Config),
    {ok, {{"HTTP/1.1", 410, _410ReasonPhrase}, _410Headers, []}} =
	test_utils:build_request(head, BannedPath#path.id, Config),
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, OptionsHeaders, []}} =
	test_utils:build_request(options, BannedPath#path.id, Config).

delete_entity(Config) ->
    Url = <<"http://google.com">>,
    Url1 = <<"http://google.com/ban-me">>,
    {ok, FL} = application:get_env(erli, flag_limit),

    Target = erli_storage:create(#target{url=Url}),
    Path = erli_storage:create(#path{target_id=Target#target.id}),
    AlmostBannedTarget = erli_storage:create(
			   #target{url=Url1, flag_count=FL}),
    AlmostBannedPath = erli_storage:create(
			 #path{target_id=AlmostBannedTarget#target.id}),

    {ok, {{"HTTP/1.1", 202, _202ReasonPhrase}, _202Headers, []}} =
	test_utils:build_request(delete, Path#path.id, Config),
    {ok, {{"HTTP/1.1", 204, _204ReasonPhrase}, _204Headers, []}} =
	test_utils:build_request(delete, AlmostBannedPath#path.id, Config),

    {ok, {{"HTTP/1.1", 410, _410ReasonPhrase}, _410Headers, []}} =
	test_utils:build_request(get, AlmostBannedPath#path.id, Config),

    StoredObj = erli_storage:read(path,
				  AlmostBannedPath#path.id),
    true = is_record(StoredObj, path),
    StoredObj#path.is_banned =:= true.
