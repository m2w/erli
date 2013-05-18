%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc Test suite for functional testing of the path
%%% resource.
%%% @end
%%%==========================================================

-module(visit_resource_SUITE).

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
    RootUrl = "http://localhost:" ++ PortS ++ "/api/visits/",

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
     idempotent_calls_to_entity].

idempotent_calls_to_collection(Config) ->
    test_utils:generate_visits(60),

    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers, Body}} =
	test_utils:build_request(get, Config),
    B = jsx:decode(list_to_binary(Body)),
    DefaultOffset = ?config(default_offset, Config),
    DefaultOffset = length(proplists:get_value(<<"visits">>, B)),
    Meta = proplists:get_value(<<"meta">>, B),
    test_utils:validate_meta(60, 25, 0,
		  DefaultOffset,
		  ?config(max_offset, Config), Meta),

    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers1, Body1}} =
	test_utils:build_request(get, [{"Range", "visits=9-50"}], Config),
    B1 = jsx:decode(list_to_binary(Body1)),
    Meta1 = proplists:get_value(<<"meta">>, B1),
    test_utils:validate_meta(60, 41, 9, 50, ?config(max_offset, Config), Meta1),

    {ok, {{"HTTP/1.1", 400, _400ReasonPhrase}, _400Headers, []}} =
	test_utils:build_request(get, [{"Range", "visits=0-100"}], Config).

idempotent_calls_to_empty_collection(Config) ->
    ExpectedMethods = [<<"GET">>, <<"HEAD">>, <<"OPTIONS">>],

    %% GET
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, Headers, Body}} =
	test_utils:build_request(get, Config),
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, Headers, []}} =
	test_utils:build_request(head, Config),
    "application/json" = proplists:get_value("content-type", Headers),
    "visits 0-0/0" = proplists:get_value("content-range", Headers),
    B = jsx:decode(list_to_binary(Body)),
    [] = proplists:get_value(<<"visits">>, B),
    Meta = proplists:get_value(<<"meta">>, B),
    test_utils:validate_meta(0, 0, 0, 0, ?config(max_offset, Config), Meta),

    %% OPTIONS
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, OptionsHeaders, []}} =
	test_utils:build_request(options, Config),
    "0" = proplists:get_value("content-length", OptionsHeaders),
    "visits" = proplists:get_value("accept-ranges", OptionsHeaders),
    Methods = re:split(proplists:get_value("allow", OptionsHeaders), ",?\s"),
    true = lists:all(fun(M) -> lists:member(M, ExpectedMethods) end, Methods).

idempotent_calls_to_entity(Config) ->
    ExpectedMethods = [<<"GET">>, <<"HEAD">>, <<"OPTIONS">>],

    Visits = test_utils:generate_visits(3),

    Visit = lists:nth(1, Visits),
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, Headers, Body}} =
	test_utils:build_request(get, Visit#visit.id, Config),
    B = jsx:decode(list_to_binary(Body)),
    Rels = proplists:get_value(
	     <<"rels">>, proplists:get_value(<<"visits">>, B)),
    Id = Visit#visit.path_id,
    <<"/api/paths/", Id/bitstring>> =
	proplists:get_value(<<"path">>, Rels),

    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, Headers, []}} =
	test_utils:build_request(head, Visit#visit.id, Config),
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, OptionsHeaders, []}} =
	test_utils:build_request(options, Visit#visit.id, Config),

    "0" = proplists:get_value("content-length", OptionsHeaders),
    Methods = re:split(proplists:get_value("allow", OptionsHeaders), ",?\s"),
    true = lists:all(fun(M) -> lists:member(M, ExpectedMethods) end, Methods),

    %% missing resource
    {ok, {{"HTTP/1.1", 404, _404ReasonPhrase}, _404Headers, _404Body}} =
	test_utils:build_request(get, <<"rubbish-id">>, Config),
    {ok, {{"HTTP/1.1", 404, _404ReasonPhrase}, _404Headers, []}} =
	test_utils:build_request(head, <<"rubbish-id">>, Config),
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, OptionsHeaders, []}} =
	test_utils:build_request(options, <<"rubbish-id">>, Config).
