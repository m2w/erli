%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc A utility library for erli.
%%% Contains a number of resource related utility functions.
%%% @end
%%%==========================================================

-module(erli_utils).

%% API
-export([to_proplist/1,
         get_env/1,
         priv_dir/1,
         obj_type_to_col_type/1,
         get_location/1,
         add_json_response/2,
         int_to_bitstring/1,
         meta_proplist/2,
         generate_etag/1,
         unix_timestamp/0,
         parse_range_header/2,
         build_content_range_header/2,
         run/1]).

-include("models.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%-----------------------------------------------------------
%% Types
%%-----------------------------------------------------------

-type file_path() :: string().
-type maybe_int() :: [] | integer().
-type etag() :: string().
-type exit_status() :: non_neg_integer().

%%-----------------------------------------------------------
%% API Methods
%%-----------------------------------------------------------

-spec build_content_range_header(collection_type(), meta_data()) -> string().
build_content_range_header(Type, Meta) ->
    ColSize = proplists:get_value(<<"totalCollectionSize">>, Meta),
    End = proplists:get_value(<<"rangeEnd">>, Meta),
    Start = proplists:get_value(<<"rangeStart">>, Meta),
    atom_to_list(Type) ++ " " ++ integer_to_list(Start) ++ "-" ++
        integer_to_list(End) ++ "/" ++ integer_to_list(ColSize).


-spec priv_dir(module()) -> file_path().
priv_dir(Mod) ->
    case code:priv_dir(Mod) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(Mod)),
            filename:join(filename:dirname(Ebin), "priv");
        PrivDir ->
            PrivDir
    end.


-spec get_env(atom()) -> undefined | term().
get_env(Key) ->
    case application:get_env(Key) of
        undefined ->
            undefined;
        {ok, Val} ->
            Val
    end.


-spec add_json_response(#wm_reqdata{}, bitstring()) -> #wm_reqdata{}.
add_json_response(RD, Body) ->
    wrq:set_resp_body(Body,
                      wrq:set_resp_header("Content-Type",
                                          "application/json", RD)).


-spec meta_proplist(collection_type(), range()) -> meta_data().
meta_proplist(CollectionType, {Start, End}) ->
    Total = erli_storage:count(CollectionType),
    M = [{<<"totalCollectionSize">>, Total},
         {<<"objectCount">>, End-Start},
         {<<"rangeStart">>, Start},
         {<<"rangeEnd">>, End},
         {<<"maxCollectionOffset">>, erli_utils:get_env(max_collection_offset)}],
    %% special case: less total objects than the default offset
    %% @TODO: this should be hotswapped out once the demo reaches over 25 records
    if End > Total ->
            M1 = lists:keyreplace(<<"objectCount">>,
                                  1, M, {<<"objectCount">>, Total}),
            lists:keyreplace(<<"rangeEnd">>,
                             1, M1, {<<"rangeEnd">>, Total});
       true ->
            M
    end.


-spec to_proplist(object()) -> proplist().
to_proplist(#target{id=Id, record_number=_RN, url=Url, last_modified=LM,
                    is_banned=B, flag_count=FC, has_thumbnail=SC}) ->
    Thumbnail =
        case SC of
            false -> get_env(thumbnail_placeholder);
            true -> <<Id/bitstring, ".jpeg">>
        end,
    [{<<"id">>, Id},
     {<<"href">>, <<"/api/targets/", Id/bitstring>>},
     {<<"bannedStatus">>, B},
     {<<"flagCount">>, FC},
     {<<"lastModified">>, LM},
     {<<"rels">>,
      [{<<"targetUrl">>, Url},
       {<<"paths">>, <<"/api/targets/", Id/bitstring, "/paths">>},
       {<<"visits">>, <<"/api/targets/", Id/bitstring, "/visits">>},
       {<<"thumbnail">>, <<"/static/thumbnails/", Thumbnail/bitstring>>}]}];
to_proplist(#path{id=Id, record_number=_RN, target_id=_, is_banned=B}) ->
    [{<<"id">>, Id},
     {<<"href">>, <<"/api/paths/", Id/bitstring>>},
     {<<"bannedStatus">>, B},
     {<<"rels">>,
      [{<<"target">>, <<"/api/paths/", Id/bitstring, "/target">>},
       {<<"visits">>, <<"/api/paths/", Id/bitstring, "/visits">>}]}];
to_proplist(#visit{id=Id, path_id=PId, geo_location=Loc, time=Time}) ->
    [{<<"id">>, Id},
     {<<"href">>, <<"/api/visits/", Id/bitstring>>},
     {<<"visitTime">>, Time},
     {<<"visitorOrigin">>, Loc},
     {<<"rels">>,
      [{<<"path">>, <<"/api/paths/", PId/bitstring>>}]}];
to_proplist(Collection) when is_list(Collection) ->
    lists:foldl(fun(Obj, Acc) -> [to_proplist(Obj)|Acc]
                end, [], Collection).


-spec int_to_bitstring(integer()) -> bitstring().
int_to_bitstring(Int) ->
    list_to_binary(integer_to_list(Int)).


-spec obj_type_to_col_type(path) -> paths;
                          (target) -> targets;
                          (visit) -> visits.
obj_type_to_col_type(path) ->
    paths;
obj_type_to_col_type(target) ->
    targets;
obj_type_to_col_type(visit) ->
    visits.


-spec generate_etag({object_type(), object()} | {collection_type(), {meta_data(), collection()}}) -> etag().
generate_etag({ObjectType, Object}) when ?is_object(ObjectType) ->
    mochihex:to_hex(erlang:md5(jsx:encode(to_proplist(Object))));
generate_etag({targets, {Meta, Collection}}) ->
    LMS = lists:sum(
            lists:map(fun(Obj) -> Obj#target.last_modified end,
                      Collection)),
    MetaMD5 = erlang:md5(jsx:encode(Meta)),
    mochihex:to_hex(<<MetaMD5/binary, LMS/integer>>);
generate_etag({paths, {Meta, Collection}}) ->
    Banned = length([X || X <- Collection, X#path.is_banned]),
    MetaMD5 = erlang:md5(jsx:encode(Meta)),
    mochihex:to_hex(<<MetaMD5/binary, Banned/integer>>);
generate_etag({visits, {Meta, Collection}}) ->
    Times = lists:sum([X#visit.time || X <- Collection]),
    MetaMD5 = erlang:md5(jsx:encode(Meta)),
    mochihex:to_hex(<<MetaMD5/binary, Times/integer>>).


-spec unix_timestamp() -> pos_integer().
unix_timestamp() ->
    UTC = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    UnixEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    UTC - UnixEpoch.


-spec parse_range_header(#wm_reqdata{}, collection_type()) ->
                                range() |
                                {error, invalid_range}.
parse_range_header(RD, CollectionType) ->
    Default = get_env(default_collection_offset),
    RH = wrq:get_req_header("Range", RD),
    case RH of
        undefined -> {0, Default};
        Val -> parse_range(string:strip(Val), CollectionType)
    end.

-spec get_location(string()) -> bitstring().
get_location(Ip) ->
    {ok, Rec} = egeoip:lookup(Ip),
    list_to_binary(egeoip:get(Rec, country_code)).

-spec run(string()) -> {exit_status(), iolist()}.
run(Cmd) ->
    Opt = [stream, exit_status, use_stdio, stderr_to_stdout, in, eof],
    P = open_port({spawn, Cmd}, Opt),
    get_data(P, []).


%%-----------------------------------------------------------
%% Internal Methods
%%-----------------------------------------------------------

-spec get_data(port(), iolist()) -> {exit_status(), iolist()}.
get_data(P, D) ->
    receive
        {P, {data, D1}} ->
            get_data(P, [D|D1]);
        {P, eof} ->
            port_close(P),
            receive
                {P, {exit_status, N}} ->
                    {N, lists:reverse(D)}
            end
    end.

-spec parse_range(string(), collection_type()) ->
                         range() |
                         {error, invalid_range}.
parse_range(HeaderValue, CollectionType) ->
    M = atom_to_list(CollectionType),
    case re:run(HeaderValue, "(\\w+)=(\\d*)-(\\d*)",
                [{capture, all_but_first, list}]) of
        {match, [Unit, X, Y]} when Unit =:= M ->
            Max = erli_storage:count(CollectionType),
            MaxOffset = erli_utils:get_env(max_collection_offset),
            extract_range(to_int(X), to_int(Y), MaxOffset, Max);
        _ -> % wrong unit specified or otherwise invalid range spec
            {error, invalid_range}
    end.


-spec to_int(string()) -> maybe_int().
to_int([]) ->
    [];
to_int(String) ->
    try list_to_integer(String)
    catch _ -> []
    end.

-spec extract_range(maybe_int(), maybe_int(), pos_integer(), pos_integer()) ->
                           range() |
                           {error, invalid_range}.
extract_range([], [], _, _) -> % unit=-
    {error, invalid_range};
extract_range([], Y, MaxOffset, Max)
  when Max-Y =< MaxOffset andalso Y =< Max -> % unit=-Y
    {Y, Max-Y};
extract_range(X, [], MaxOffset, Max)
  when X+MaxOffset =< Max -> % unit=X-
    {X, X+MaxOffset};
extract_range(X, [], _, Max) -> % unit=X-
    {X, Max};
extract_range(X, Y, MaxOffset, Max)
  when Y > X andalso Y-X =< MaxOffset andalso Y =< Max -> % unit=X-Y
    {X, Y};
extract_range(_, _, _, _) ->
    {error, invalid_range}.
