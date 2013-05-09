%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc A generic resource used to represent target and path
%%% objects.
%%% @end
%%%==========================================================

-module(erli_generic_resource).

%% Webmachine Callbacks
-export([init/1,
	 allowed_methods/2,
	 options/2,
	 malformed_request/2,
	 resource_exists/2,
	 previously_existed/2,
	 generate_etag/2,
	 content_types_provided/2,
	 delete_resource/2,
	 delete_completed/2,
	 as_json/2,
	 process_post/2]).

-include("models.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%-----------------------------------------------------------
%% Types
%%-----------------------------------------------------------

-type initial_resource_state() :: target | targets | path | paths.

%%-----------------------------------------------------------
%% Webmachine Callbacks
%%-----------------------------------------------------------

-spec init([initial_resource_state()]) -> {ok, initial_resource_state()}.
init([InitialState]) ->
    {ok, InitialState}.


allowed_methods(RD, Ctx) when ?is_collection(Ctx) ->
    {['GET', 'POST', 'HEAD', 'OPTIONS'], RD, Ctx};
allowed_methods(RD, Ctx) ->
    {['GET', 'DELETE', 'HEAD', 'OPTIONS'], RD, Ctx}.


malformed_request(RD, Ctx) when ?is_collection(Ctx) ->
    case erli_utils:parse_range_header(RD, Ctx) of
	{error, invalid_range} ->
	    ContentRange = "*/" ++ integer_to_list(erli_storage:count(Ctx)),
	    NRD = wrq:set_resp_header("Content-Range", ContentRange, RD),
	    {true, NRD, Ctx};
	Range ->
	    {false, RD, {Ctx, Range}}
    end;
malformed_request(RD, Ctx) ->
    {false, RD, Ctx}.


options(RD, {CollectionType, _Range}=Ctx) ->
    ColType = atom_to_list(CollectionType),
    {[{"Content-Length", "0"},
      {"Accept-Ranges", ColType},
      {"Allow", "GET, POST, HEAD, OPTIONS"}], RD, Ctx};
options(RD, Ctx) ->
    {[{"Content-Length", "0"},
      {"Allow", "GET, DELETE, HEAD, OPTIONS"}], RD, Ctx}.


resource_exists(RD, {CollectionType, Range}) ->
    Data = erli_storage:read_multiple(CollectionType, Range),
    Meta = erli_utils:meta_proplist(CollectionType, Range),
    {true, RD, {CollectionType, {Meta, Data}}};
resource_exists(RD, ObjectType) ->
    Id = list_to_binary(wrq:path_info(id, RD)),
    case erli_storage:read(ObjectType, Id) of
	{error, _Error} ->
	    {false, RD, ObjectType};
	Obj when is_record(Obj, target) ->
	    {not Obj#target.is_banned, RD, {ObjectType, Obj}};
	Obj when is_record(Obj, path) ->
	    {not Obj#path.is_banned, RD, {ObjectType, Obj}}
    end.


previously_existed(RD, {path, Rec}=Ctx) ->
    {Rec#path.is_banned, RD, Ctx};
previously_existed(RD, {target, Rec}=Ctx) ->
    {Rec#target.is_banned, RD, Ctx};
previously_existed(RD, Ctx) ->
    {false, RD, Ctx}.


generate_etag(RD, Ctx) ->
    Etag = erli_utils:generate_etag(Ctx),
    {Etag, RD, Ctx}.


content_types_provided(RD, Ctx) ->
    {[{"application/json", as_json}], RD, Ctx}.


as_json(RD, {ObjectType, Rec}=Ctx) when ?is_object(ObjectType) ->
    CollectionType = obj_type_to_col_type(ObjectType),
    Key = atom_to_binary(CollectionType, latin1),
    Data = jsx:encode([{Key, erli_utils:to_proplist(Rec)}]),
    {Data, RD, Ctx};
as_json(RD, {CollectionType, {Meta, Collection}}=Ctx) ->
    Key = atom_to_binary(CollectionType, latin1),
    Data = jsx:encode([{Key, erli_utils:to_proplist(Collection)},
		       {<<"meta">>, Meta}]),
    ContentRangeHeader = erli_utils:build_content_range_header(CollectionType,
							       Meta),
    NRD = wrq:set_resp_header("Content-Range", ContentRangeHeader, RD),
    {Data, NRD, Ctx}.


process_post(RD, Ctx) ->
    case mochiweb_util:parse_header(wrq:get_req_header("Content-Type", RD)) of
	{"application/x-www-form-urlencoded", _} ->
	    Form = erli_forms:http_body_to_form(wrq:req_body(RD)),
	    handle_post(Form, RD, Ctx);
	{"application/json", _} ->
	    Form = jsx:decode(wrq:req_body(RD)),
	    handle_post(Form, RD, Ctx);
	_ ->
	    {{halt, 415}, RD, Ctx}
    end.


delete_resource(RD, {ObjectType, Obj}) when ?is_object(ObjectType) ->
    {_RemovalState, NewObj} =  erli_storage:request_removal(Obj),
    {true, RD, {ObjectType, NewObj}}.


delete_completed(RD, {_ObjectType, Obj}=Ctx) when is_record(Obj, target) ->
    {Obj#target.is_banned, RD, Ctx};
delete_completed(RD, {_ObjectType, Obj}=Ctx) when is_record(Obj, path) ->
    {Obj#path.is_banned, RD, Ctx}.


%%----------------------------------------------------------
%% Internal Methods
%%----------------------------------------------------------

-spec handle_post(proplist(), #wm_reqdata{}, {targets, collection_data()}) ->
			 {true | {halt, 422}, #wm_reqdata{},
			  {targets, collection_data()}};
		 (proplist(), #wm_reqdata{}, {paths, collection_data()}) ->
			 {true | {halt, 422}, #wm_reqdata{},
			  {paths, collection_data()}}.
handle_post(Form, RD, {targets, {_Meta, _Collection}}=Ctx) ->
    case erli_forms:validate(Form, [{<<"target_url">>, [required, is_url]}]) of
	valid ->
	    Target = #target{url=proplists:get_value(<<"target_url">>, Form)},
	    maybe_store(targets, Target, RD, Ctx);
	Errors ->
	    Body = jsx:encode([{<<"formErrors">>, Errors}]),
	    NRD = erli_utils:add_json_response(RD, Body),
	    {{halt, 422}, NRD, Ctx}
    end;
handle_post(Form, RD, {paths, {_Meta, _Collection}}=Ctx) ->
    case erli_forms:validate(Form, [{<<"target_id">>,
				     [required, is_target_id]}]) of
	valid ->
	    Path = #path{target_id=proplists:get_value(<<"target_id">>, Form)},
	    maybe_store(paths, Path, RD, Ctx);
	Errors ->
	    Body = jsx:encode([{<<"formErrors">>, Errors}]),
	    NRD = erli_utils:add_json_response(RD, Body),
	    {{halt, 422}, NRD, Ctx}
    end.

maybe_store(CollectionType, Record, RD, Ctx) ->
    case erli_storage:create(Record) of
	{error, {conflict, {SubmittedRecord, ExistingRecord}}} ->
	    Body = jsx:encode([{<<"conflictingEntity">>,
				erli_utils:to_proplist(ExistingRecord)},
			       {<<"submittedEntity">>,
				erli_utils:to_proplist(SubmittedRecord)}]),
	    NRD = erli_utils:add_json_response(RD, Body),
	    {{halt, 409}, NRD, Ctx};
	{error, Error} ->
	    %% @TODO: improve the error handling/format
	    Body = jsx:encode([{<<"errors">>,
				atom_to_list(Error)}]),
	    NRD = erli_utils:add_json_response(RD, Body),
	    {{halt, 422}, NRD, Ctx};
	SavedTarget ->
	    Body = jsx:encode([{atom_to_binary(CollectionType, latin1),
				erli_utils:to_proplist(SavedTarget)}]),
	    NRD = erli_utils:add_json_response(RD, Body),
	    {true, NRD, Ctx}
    end.

-spec obj_type_to_col_type(path) -> paths;
			  (target) -> targets.
obj_type_to_col_type(path) ->
    paths;
obj_type_to_col_type(target) ->
    targets.
