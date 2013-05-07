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
	 add_json_response/2,
	 meta_proplist/2,
	 generate_etag/1,
	 unix_timestamp/0,
	 parse_range_header/2]).

-include("models.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%-----------------------------------------------------------
%% Types
%%-----------------------------------------------------------
-type file_path() :: string().
-type maybe_int() :: [] | integer().

%%-----------------------------------------------------------
%% API Methods
%%-----------------------------------------------------------

-spec priv_dir(atom()) -> file_path().
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


-spec meta_proplist(model_name(), query_range()) -> [{bitstring(), term()}].
meta_proplist(Model, {Start, End}) ->
    [{<<"totalCollectionSize">>, erli_storage:count(Model)},
     {<<"objectCount">>, End-Start},
     {<<"rangeStart">>, Start},
     {<<"rangeEnd">>, End},
     {<<"maxCollectionOffset">>, erli_utils:get_env(max_collection_offset)}].

-spec to_proplist(model()) -> [{bitstring(), term()}].
to_proplist(#target{id=Id, record_number=_RN, url=Url, last_modified=LM,
		    is_banned=B, flag_count=FC, screenshot_id=SC}) ->
    Thumbnail = case SC of
		    undefined ->
			get_env(thumbnail_placeholder_id);
		    Id ->
			Id
		end,
    [{<<"id">>, Id},
     {<<"href">>, <<"/api/targets/", Id/bitstring>>},
     {<<"bannedStatus">>, B},
     {<<"flagCount">>, FC},
     {<<"lastModified">>, LM},
     {<<"rels">>,
      [{<<"targetUrl">>, Url},
       {<<"thumbnail">>, <<"/static/thumbnails/", Thumbnail/bitstring>>}]}];
to_proplist(Collection) when is_list(Collection) ->
    lists:foldl(fun(Obj, Acc) ->
		      to_proplist(Obj) ++ Acc
		end, [], Collection).

-spec generate_etag(model() | {list(), list()}) -> bitstring().
generate_etag(Record) when is_record(Record, target) ->
    mochihex:to_hex(erlang:md5(jsx:encode(to_proplist(Record))));
generate_etag({Meta, Objects}) ->
    LMS = lists:sum(
	    lists:map(fun(O) ->
			      proplists:get_value(<<"lastModified">>, O)
		      end, Objects)),
    MetaMD5 = erlang:md5(jsx:encode(Meta)),
    mochihex:to_hex(<<MetaMD5/binary, LMS/integer>>).


-spec unix_timestamp() -> pos_integer().
unix_timestamp() ->
    UTC = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    UnixEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    UTC - UnixEpoch.

-spec parse_range_header(#wm_reqdata{}, model_name()) ->
				query_range() |
				{error, invalid_range}.
parse_range_header(RD, Model) ->
    Default = get_env(default_collection_offset),
    RH = wrq:get_req_header("Range", RD),
    case RH of
	undefined ->
	    {0, Default};
	Val ->
	    parse_range(string:strip(Val), Model)
    end.

%%-----------------------------------------------------------
%% Internal Methods
%%-----------------------------------------------------------

-spec parse_range(string(), model_name()) ->
			 query_range() |
			 {error, invalid_range}.
parse_range(HeaderValue, Model) ->
    M = atom_to_list(Model),
    case re:run(HeaderValue, "(\\w+)=(\\d*)-(\\d*)",
		[{capture, all_but_first, list}]) of
	{match, [Unit, X, Y]} when Unit =:= M ->
	    Max = erli_storage:count(Model),
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
			   query_range() |
			   {error, invalid_range}.
extract_range([], [], _, _) -> % unit=-
    {error, invalid_range};
extract_range([], Y, MaxOffset, Max) when Max-Y =< MaxOffset -> % unit=-Y
    {Y, Max-Y};
extract_range(X, [], MaxOffset, Max) when X+MaxOffset =< Max -> % unit=X-
    {X, X+MaxOffset};
extract_range(X, [], _, Max) -> % unit=X-
    {X, Max};
extract_range(X, Y, MaxOffset, Max)
  when Y > X andalso Y-X =< MaxOffset andalso Y =< Max -> % unit=X-Y
    {X, Y};
extract_range(_, _, _, _) ->
    {error, invalid_range}.
