%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc Utility functions to simplify testing.
%%% @end
%%%==========================================================

-module(test_utils).

%% API
-export([init_suite/2,
         clear_all_mnesia_data/0,
         generate_targets/1,
         generate_paths/1,
         generate_paths/2,
         generate_visits/1,
         generate_visits/2,
         validate_meta/6,
         post_request/2,
         build_request/4,
         build_request/3,
         build_request/2]).

-include("models.hrl").
-include_lib("common_test/include/ct.hrl").

%%-----------------------------------------------------------
%% API Methods
%%-----------------------------------------------------------
%% @TODO: make this a gen_server so env vars can be kept in state

init_suite(Config, Resource) ->
    DataDir = ?config(data_dir, Config),
    Priv = ?config(priv_dir, Config),

    application:set_env(mnesia, dir, Priv),
    application:set_env(webmachine, dispatch_dir, DataDir),
    erli:start(),

    {ok, DefaultOffset} = application:get_env(erli, default_collection_offset),
    {ok, MaxOffset} = application:get_env(erli, max_collection_offset),
    {ok, Port} = application:get_env(webmachine, port),

    PortS = integer_to_list(Port),
    RootUrl = "http://localhost:" ++ PortS ++ "/api/" ++
        atom_to_list(Resource) ++ "/",

    [{default_offset, DefaultOffset}, {max_offset, MaxOffset},
     {port, PortS}, {port_int, Port}, {root_url, RootUrl} | Config].

clear_all_mnesia_data() ->
    mnesia:clear_table(counters),
    mnesia:clear_table(targets),
    mnesia:clear_table(paths),
    mnesia:clear_table(visits).


generate_paths(N) ->
    Targets = generate_targets(random:uniform(N)),
    generate_paths(N, Targets, []).


generate_paths(N, Target) when is_record(Target, target) ->
    generate_paths(N, [Target], []).


generate_targets(N) ->
    generate_targets(N, []).


generate_visits(N) ->
    Paths = generate_paths(random:uniform(N)),
    generate_visits(N, Paths, []).


generate_visits(N, Paths) when is_list(Paths) ->
    generate_visits(N, Paths, []);
generate_visits(N, Record) when is_record(Record, target) ->
    Paths = generate_paths(random:uniform(N), Record),
    generate_visits(N, Paths, []).


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

%%----------------------------------------------------------
%% Internal methods
%%----------------------------------------------------------

generate_paths(0, _Targets, Acc) ->
    Acc;
generate_paths(N, Targets, Acc) ->
    X = random:uniform(length(Targets)),
    Id = (lists:nth(X, Targets))#target.id,
    case erli_storage:create(#path{target_id=Id}) of
        {error, Error} ->
            generate_paths(N, Targets, Acc);
        P ->
            generate_paths(N - 1, Targets, [P|Acc])
    end.


generate_visits(0, _Paths, Acc) ->
    Acc;
generate_visits(N, Paths, Acc) ->
    X = random:uniform(length(Paths)),
    Id = (lists:nth(X, Paths))#path.id,
    Loc = erli_utils:get_location(gen_rand_ip()),
    case erli_storage:create(#visit{path_id=Id, geo_location=Loc}) of
        {error, Error} ->
            generate_visits(N, Paths, Acc);
        V ->
            generate_visits(N - 1, Paths, [V|Acc])
    end.

gen_rand_ip() ->
    Tuple = [integer_to_list(X) ||
                X <- [random:uniform(255), random:uniform(255),
                      random:uniform(255), random:uniform(255)]],
    string:join(Tuple, ".").


generate_targets(0, Acc) ->
    Acc;
generate_targets(N, Acc) ->
    Target = make_target(),
    case erli_storage:create(Target) of
        {error, _} ->
            generate_targets(N, Acc);
        T ->
            generate_targets(N - 1, [T|Acc])
    end.


make_target() ->
    {ok, FL} = application:get_env(erli, flag_limit),
    Domain = generate_domain(),
    TLD = generate_tld(),
    Url = <<"http://", Domain/bitstring, ".", TLD/bitstring>>,
    FC = random:uniform(25),
    #target{url=Url, flag_count=FC, is_banned=FC>FL}.


generate_domain() ->
    generate_bin(random:uniform(30)).
generate_tld() ->
    generate_bin(random:uniform(3)).


generate_bin(Len) ->
    re:replace(base64:encode(crypto:rand_bytes(Len)),
               "[\/+=]", "",
               [global, {return, binary}]).
