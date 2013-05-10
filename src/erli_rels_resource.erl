%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc A generic resource that provides to related record
%%% information.
%%% The main motivation behind the resource is to prevent further
%%% "code-strain" on the generic resource.
%%% @end
%%%==========================================================

-module(erli_rels_resource).

%% Webmachine Callbacks
-export([init/1,
	 allowed_methods/2,
	 malformed_request/2,
	 options/2,
	 resource_exists/2,
	 previously_existed/2,
	 generate_etag/2,
	 content_types_provided/2,
	 as_json/2]).

-include("models.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%-----------------------------------------------------------
%% Types
%%-----------------------------------------------------------

-type initial_resource_state() :: {path, visits} |
				  {target, paths} | {target, visits}.

%%-----------------------------------------------------------
%% Webmachine Callbacks
%%-----------------------------------------------------------

-spec init([initial_resource_state()]) -> {ok, initial_resource_state()}.
init([InitialState]) ->
    {ok, InitialState}.


allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD', 'OPTIONS'], RD, Ctx}.


malformed_request(RD, {_ObjType, RelType}=Ctx) ->
    case erli_utils:parse_range_header(RD, RelType) of
	{error, invalid_range} ->
	    ContentRange = "*/" ++ integer_to_list(erli_storage:count(RelType)),
	    NRD = wrq:set_resp_header("Content-Range", ContentRange, RD),
	    {true, NRD, Ctx};
	Range ->
	    {false, RD, {Ctx, Range}}
    end.


options(RD, {_, RelType}=Ctx) ->
    {[{"Content-Length", "0"},
      {"Accept-Ranges", RelType},
      {"Allow", "GET, HEAD, OPTIONS"}], RD, Ctx}.


resource_exists(RD, {{ObjType, RelType}, Range}=Ctx) ->
    Id = list_to_binary(wrq:path_info(id, RD)),
    case root_elem_exists(ObjType, Id) of
	false ->
	    {false, RD, Ctx};
	{false, Obj} ->
	    {false, RD, {ObjType, Obj}};
	{true, _Obj} ->
	    Data = erli_storage:read_multiple(RelType, Range),
	    Meta = erli_utils:meta_proplist(RelType, Range),
	    {true, RD, {RelType, {Meta, Data}}}
	end.


previously_existed(RD, Ctx) when is_record(Ctx, path)->
    {Ctx#path.is_banned, RD, Ctx};
previously_existed(RD, Ctx) when is_record(Ctx, target) ->
    {Ctx#target.is_banned, RD, Ctx}.


generate_etag(RD, Ctx) ->
    Etag = erli_utils:generate_etag(Ctx),
    {Etag, RD, Ctx}.


content_types_provided(RD, Ctx) ->
    {[{"application/json", as_json}], RD, Ctx}.


as_json(RD, {RelType, {Meta, Collection}}=Ctx) ->
    Key = atom_to_binary(RelType, latin1),
    Data = jsx:encode([{Key, erli_utils:to_proplist(Collection)},
		       {<<"meta">>, Meta}]),
    %% @CHECK: move this up into the resource_exists call? check where HEAD ends
    ContentRangeHeader = erli_utils:build_content_range_header(RelType, Meta),
    NRD = wrq:set_resp_header("Content-Range", ContentRangeHeader, RD),
    {Data, NRD, Ctx};
as_json(RD, {ObjectType, Rec}=Ctx) ->
    CollectionType = erli_utils:obj_type_to_col_type(ObjectType),
    Key = atom_to_binary(CollectionType, latin1),
    Data = jsx:encode([{Key, erli_utils:to_proplist(Rec)}]),
    {Data, RD, Ctx}.


%%----------------------------------------------------------
%% Internal Methods
%%----------------------------------------------------------

root_elem_exists(ObjType, Id) ->
    case erli_storage:read(ObjType, Id) of
	{error, _Error} ->
	    false;
	Obj when is_record(Obj, target) ->
	    {not Obj#target.is_banned, Obj};
	Obj when is_record(Obj, path) ->
	    {not Obj#path.is_banned, Obj}
    end.
