%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc Test suite for functional testing of the target
%%% resource.
%%% @end
%%%==========================================================

-module(target_resource_SUITE).

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
    erli_storage:setup_tables([node()]),

    {ok, DefaultOffset} = application:get_env(erli, default_collection_offset),
    {ok, MaxOffset} = application:get_env(erli, max_collection_offset),
    {ok, Port} = application:get_env(webmachine, port),

    PortS = integer_to_list(Port),
    RootUrl = "http://localhost:" ++ PortS ++ "/api/targets/",

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
     delete_entity].

idempotent_calls_to_collection(Config) ->
    ok = test_utils:generate_targets(55),

    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers, Body}} =
	build_request(get, Config),
    B = jsx:decode(list_to_binary(Body)),
    DefaultOffset = length(proplists:get_value(<<"targets">>, B)),
    Meta = proplists:get_value(<<"meta">>, B),
    validate_meta(55, 25, 0,
		  ?config(default_offset, Config),
		  ?config(max_offset, Config), Meta),

    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers1, Body1}} =
	build_request(get, [{"Range", "targets=10-15"}], Config),
    B1 = jsx:decode(list_to_binary(Body1)),
    Meta1 = proplists:get_value(<<"meta">>, B1),
    validate_meta(55, 5, 10, 15, ?config(max_offset, Config), Meta1),

    {ok, {{"HTTP/1.1", 400, _400ReasonPhrase}, _400Headers, []}} =
	build_request(get, [{"Range", "targets=-90000"}], Config).

idempotent_calls_to_empty_collection(Config) ->
    ExpectedMethods = [<<"GET">>, <<"POST">>, <<"HEAD">>, <<"OPTIONS">>],

    %% GET
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, Headers, Body}} =
	build_request(get, Config),
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, Headers, []}} =
	build_request(head, Config),
    "application/json" = proplists:get_value("content-type", Headers),
    "targets 0-0/0" = proplists:get_value("content-range", Headers),
    B = jsx:decode(list_to_binary(Body)),
    [] = proplists:get_value(<<"targets">>, B),
    Meta = proplists:get_value(<<"meta">>, B),
    validate_meta(0, 0, 0, 0, ?config(max_offset, Config), Meta),

    %% OPTIONS
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, OptionsHeaders, []}} =
	build_request(options, Config),
    "0" = proplists:get_value("content-length", OptionsHeaders),
    "targets" = proplists:get_value("accept-ranges", OptionsHeaders),
    Methods = re:split(proplists:get_value("allow", OptionsHeaders), ",?\s"),
    true = lists:all(fun(M) -> lists:member(M, ExpectedMethods) end, Methods).

post_json_data(Config) ->
    TargetUrl = <<"http://google.com">>,
    Payload = jsx:encode([{<<"target_url">>, TargetUrl}]),
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers, Body}} =
	post_request(Payload, Config),
    B = jsx:decode(list_to_binary(Body)),
    Id = proplists:get_value(<<"id">>, proplists:get_value(<<"targets">>, B)),
    StoredObj = erli_storage:read(target, Id),
    true = is_record(StoredObj, target),
    TargetUrl = StoredObj#target.url,

    %% invalid url
    InvalidUrl = <<"asdf">>,
    Payload1 = jsx:encode([{<<"target_url">>, InvalidUrl}]),
    {ok, {{"HTTP/1.1", 422, _422ReasonPhrase}, _Headers1, Body1}} =
	post_request(Payload1, Config),
    [{<<"formErrors">>,
      [{<<"target_url">>, [<<"is not a valid URL">>]}]}] =
	jsx:decode(list_to_binary(Body1)),

    %% missing required field
    Payload2 = jsx:encode([{<<"rubbish">>, <<>>}]),
    {ok, {{"HTTP/1.1", 422, _422ReasonPhrase}, _Headers2, Body2}} =
	post_request(Payload2, Config),
    [{<<"formErrors">>,
      [{<<"target_url">>, [<<"field is required">>]}]}] =
	jsx:decode(list_to_binary(Body2)),

    %% conflicting target
    ConflictingUrl = <<"http://gooogle.com">>,
    ConflictingTarget = #target{url=ConflictingUrl,
				is_banned=true, flag_count=100},
    erli_storage:create(ConflictingTarget),
    Payload3 = jsx:encode([{<<"target_url">>, ConflictingUrl}]),
    %% TODO: validate the returned conflic message.
    {ok, {{"HTTP/1.1", 409, _409ReasonPhrase}, _Headers3, _Body3}} =
	post_request(Payload3, Config).


idempotent_calls_to_entity(Config) ->
    ExpectedMethods = [<<"GET">>, <<"DELETE">>, <<"HEAD">>, <<"OPTIONS">>],
    Url = <<"http://erlang.org">>,
    BannedUrl = <<"http://example.org">>,
    Obj = erli_storage:create(#target{url=Url}),
    BannedTarget = #target{url=BannedUrl, is_banned=true,
			   flag_count=9000},
    BannedObj = erli_storage:create(BannedTarget),

    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, Headers, Body}} =
	build_request(get, Obj#target.id, Config),
    B = jsx:decode(list_to_binary(Body)),
    Rels = proplists:get_value(
	     <<"rels">>, proplists:get_value(<<"targets">>, B)),
    Url = proplists:get_value(<<"targetUrl">>, Rels),

    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, Headers, []}} =
	build_request(head, Obj#target.id, Config),
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, OptionsHeaders, []}} =
	build_request(options, Obj#target.id, Config),

    "0" = proplists:get_value("content-length", OptionsHeaders),
    Methods = re:split(proplists:get_value("allow", OptionsHeaders), ",?\s"),
    true = lists:all(fun(M) -> lists:member(M, ExpectedMethods) end, Methods),

    %% missing resource
    {ok, {{"HTTP/1.1", 404, _404ReasonPhrase}, _404Headers, _404Body}} =
	build_request(get, <<"rubbish-id">>, Config),
    {ok, {{"HTTP/1.1", 404, _404ReasonPhrase}, _404Headers, []}} =
	build_request(head, <<"rubbish-id">>, Config),
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, OptionsHeaders, []}} =
	build_request(options, <<"rubbish-id">>, Config),

    %% banned resource
    {ok, {{"HTTP/1.1", 410, _410ReasonPhrase}, _410Headers, _Body410}} =
	build_request(get, BannedObj#target.id, Config),
    {ok, {{"HTTP/1.1", 410, _410ReasonPhrase}, _410Headers, []}} =
	build_request(head, BannedObj#target.id, Config),
    {ok, {{"HTTP/1.1", 200, _ReasonPhrase}, OptionsHeaders, []}} =
	build_request(options, BannedObj#target.id, Config).

delete_entity(Config) ->
    Url = <<"http://google.com">>,
    Url1 = <<"http://google.com/ban-me">>,
    {ok, FL} = application:get_env(erli, flag_limit),

    Target = erli_storage:create(#target{url=Url}),
    AlmostBannedTarget = erli_storage:create(
			   #target{url=Url1, flag_count=FL}),

    {ok, {{"HTTP/1.1", 202, _202ReasonPhrase}, _202Headers, []}} =
	build_request(delete, Target#target.id, Config),
    {ok, {{"HTTP/1.1", 204, _204ReasonPhrase}, _204Headers, []}} =
	build_request(delete, AlmostBannedTarget#target.id, Config),

    StoredObj = erli_storage:read(target,
				  AlmostBannedTarget#target.id),
    true = is_record(StoredObj, target),
    StoredObj#target.is_banned =:= true.


%%----------------------------------------------------------
%% Helpers
%%----------------------------------------------------------

%% @TODO: possibly refactor into test_utils

validate_meta(TotalSize, ObjCount, RangeStart, RangeEnd, MaxOffset, Meta) ->
    TotalSize = proplists:get_value(<<"totalCollectionSize">>, Meta),
    ObjCount = proplists:get_value(<<"objectCount">>, Meta),
    RangeStart = proplists:get_value(<<"rangeStart">>, Meta),
    RangeEnd = proplists:get_value(<<"rangeEnd">>, Meta),
    MaxOffset = proplists:get_value(<<"maxCollectionOffset">>, Meta).


post_request(Payload, Config) ->
    httpc:request(post, {?config(root_url, Config), [],
			 "application/json", Payload}, [], []).

build_request(Method, Config) ->
    build_request(Method, "", [], Config).

build_request(Method, Id, Config) when is_binary(Id) ->
    build_request(Method, binary_to_list(Id), [], Config);
build_request(Method, Headers, Config) when is_list(Headers) ->
    build_request(Method, "", Headers, Config).

build_request(Method, Id, Headers, Config) ->
    httpc:request(Method, {?config(root_url, Config) ++ Id, Headers}, [], []).
