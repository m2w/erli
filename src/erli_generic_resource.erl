%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc A generic resource used to represent visit, target
%%% and path objects.
%%% @end
%%%==========================================================

-module(erli_generic_resource).

%% Webmachine Callbacks
-export([allowed_methods/2, as_json/2,
	 content_types_provided/2, delete_completed/2,
	 delete_resource/2, generate_etag/2, init/1,
	 malformed_request/2, options/2, previously_existed/2,
	 process_post/2, resource_exists/2]).

-include("models.hrl").

-include_lib("webmachine/include/webmachine.hrl").

%%-----------------------------------------------------------
%% Types
%%-----------------------------------------------------------

-type initial_resource_state() :: object_type() | collection_type() |
				  relation_type().
-type rd() :: #wm_reqdata{}.

%%-----------------------------------------------------------
%% Webmachine Callbacks
%%-----------------------------------------------------------

-spec init([initial_resource_state()]) -> {ok, initial_resource_state()}.
init([InitialState]) -> {ok, InitialState}.

-spec allowed_methods(rd(), initial_resource_state()) ->
			     {[atom()], rd(), initial_resource_state()}.
allowed_methods(RD, {_Resource, _Relation} = Ctx) ->
    {['GET', 'HEAD', 'OPTIONS'], RD, Ctx};
allowed_methods(RD, Ctx) when ?is_visit_type(Ctx) ->
    {['GET', 'HEAD', 'OPTIONS'], RD, Ctx};
allowed_methods(RD, Ctx) when ?is_collection(Ctx) ->
    {['GET', 'POST', 'HEAD', 'OPTIONS'], RD, Ctx};
allowed_methods(RD, Ctx) ->
    {['GET', 'DELETE', 'HEAD', 'OPTIONS'], RD, Ctx}.

-spec malformed_request(rd(), object_type()) ->
			       {boolean(), rd(), object_type()};
		       (rd(), collection_type()) ->
			       {boolean(), rd(), {collection_type(), range()}};
		       (rd(), {object_type(), object_type()}) ->
			       {boolean(), rd(), {object_type(), object_type()}};
		       (rd(), {object_type(), collection_type()}) ->
			       {boolean(), rd(), {collection_type(), range()}}.
malformed_request(RD, {_Resource, Relation} = Ctx) when ?is_object(Relation) ->
    {false, RD, Ctx};
malformed_request(RD, {_Resource, Relation} = Ctx)
  when ?is_collection(Relation) ->
    case erli_utils:parse_range_header(RD, Relation) of
	{error, invalid_range} ->
	    ContentRange = "*/" ++
		integer_to_list(erli_storage:count(Relation)),
	    NRD = wrq:set_resp_header("Content-Range", ContentRange,
				      RD),
	    {true, NRD, Ctx};
	Range -> {false, RD, {Ctx, Range}}
    end;
malformed_request(RD, Ctx) when ?is_collection(Ctx) ->
    case erli_utils:parse_range_header(RD, Ctx) of
	{error, invalid_range} ->
	    ContentRange = "*/" ++
		integer_to_list(erli_storage:count(Ctx)),
	    NRD = wrq:set_resp_header("Content-Range", ContentRange,
				      RD),
	    {true, NRD, Ctx};
	Range -> {false, RD, {Ctx, Range}}
    end;
malformed_request(RD, Ctx) -> {false, RD, Ctx}.

-spec options(rd(), object_type()) ->
		     {[{string(), string()}], rd(), object_type()};
	     (rd(), {collection_type(), range()}) ->
		     {[{string(), string()}], rd(), {collection_type(), range()}}.
options(RD, {visits, _Range} = Ctx) ->
    {[{"Content-Length", "0"}, {"Accept-Ranges", visits},
      {"Allow", "GET, HEAD, OPTIONS"}],
     RD, Ctx};
options(RD, {CollectionType, _Range} = Ctx) ->
    ColType = atom_to_list(CollectionType),
    {[{"Content-Length", "0"}, {"Accept-Ranges", ColType},
      {"Allow", "GET, POST, HEAD, OPTIONS"}],
     RD, Ctx};
options(RD, visit = Ctx) ->
    {[{"Content-Length", "0"},
      {"Allow", "GET, HEAD, OPTIONS"}],
     RD, Ctx};
options(RD, Ctx) ->
    {[{"Content-Length", "0"},
      {"Allow", "GET, DELETE, HEAD, OPTIONS"}],
     RD, Ctx}.

-spec resource_exists(rd(), object_type()) ->
			     {boolean(), rd(), {object_type(), object()}};
		     (rd(), {A :: object_type(), B :: object_type()}) ->
			     {boolean(), rd(), {object_type(), B::object()}};
		     (rd(), {collection_type(), range()}) ->
			     {boolean(), rd(), {collection_type(), collection()}};
		     (rd(), {{object_type(), collection_type()}, range()}) ->
			     {boolean(), rd(), {collection_type(), collection()}};
		     (rd(), {object_type(), object_type()}) ->
			     {boolean(), rd(), {object_type(), object()}}.
resource_exists(RD, ObjectType) when ?is_object(ObjectType) ->
    Id = list_to_binary(wrq:path_info(id, RD)),
    case erli_storage:read(ObjectType, Id) of
	{error, _Error} -> {false, RD, ObjectType};
	Obj when is_record(Obj, target) ->
	    {not Obj#target.is_banned, RD, {ObjectType, Obj}};
	Obj when is_record(Obj, path) ->
	    {not Obj#path.is_banned, RD, {ObjectType, Obj}};
	Obj -> {true, RD, {ObjectType, Obj}}
    end;
resource_exists(RD, {path, target} = Ctx) ->
    Id = list_to_binary(wrq:path_info(id, RD)),
    case root_elem_exists(path, Id) of
	false -> {false, RD, Ctx};
	{false, Obj} -> {false, RD, {path, Obj}};
	{true, Obj} ->
	    %% this assumes that the many-to-one relationship is enforced
	    Record = erli_storage:read(target, Obj#path.target_id),
	    {true, RD, {target, Record}}
    end;
resource_exists(RD, {{ObjectType, Relation}, Range} = Ctx)
  when ?is_collection(Relation) ->
    Id = list_to_binary(wrq:path_info(id, RD)),
    case root_elem_exists(ObjectType, Id) of
	false -> {false, RD, Ctx};
	{false, Obj} -> {false, RD, {ObjectType, Obj}};
	{true, _Obj} ->
	    Data = erli_storage:read_multiple(Relation, Range),
	    Meta = erli_utils:meta_proplist(Relation, Range),
	    {true, RD, {Relation, {Meta, Data}}}
    end;
resource_exists(RD, {CollectionType, Range}) ->
    Data = erli_storage:read_multiple(CollectionType,
				      Range),
    Meta = erli_utils:meta_proplist(CollectionType, Range),
    ContentRangeHeader =
	erli_utils:build_content_range_header(CollectionType,
					      Meta),
    NRD = wrq:set_resp_header("Content-Range",
			      ContentRangeHeader, RD),
    {true, NRD, {CollectionType, {Meta, Data}}}.

-spec previously_existed(rd(), {object_type(), object()}) ->
				{boolean(), rd(), {object_type(), object()}};
			(rd(), {collection_type(), collection()}) ->
				{false, rd(), {collection_type(), collection()}}.
previously_existed(RD, {path, Rec} = Ctx) ->
    {Rec#path.is_banned, RD, Ctx};
previously_existed(RD, {target, Rec} = Ctx) ->
    {Rec#target.is_banned, RD, Ctx};
previously_existed(RD, Ctx) -> {false, RD, Ctx}.

-spec generate_etag(rd(), term()) -> {string(), rd(), term()}.
generate_etag(RD, Ctx) ->
    Etag = erli_utils:generate_etag(Ctx),
    {Etag, RD, Ctx}.

-spec content_types_provided(rd(), term()) ->
				    {[{string(), atom()}], rd(), term()}.
content_types_provided(RD, Ctx) ->
    {[{"application/json", as_json}], RD, Ctx}.

-spec as_json(rd(), {object_type(), object()}) ->
		     {bitstring(), rd(), {object_type(), object()}};
	     (rd(), {collection_type(), collection()}) ->
		     {bitstring(), rd(), {collection_type(), collection()}}.
as_json(RD, {ObjectType, Rec} = Ctx) when ?is_object(ObjectType) ->
    maybe_record_visit(RD, Ctx),
    CollectionType =
	erli_utils:obj_type_to_col_type(ObjectType),
    Key = atom_to_binary(CollectionType, latin1),
    Data = jsx:encode([{Key, erli_utils:to_proplist(Rec)}]),
    {Data, RD, Ctx};
as_json(RD,
	{CollectionType, {Meta, Collection}} = Ctx) ->
    Key = atom_to_binary(CollectionType, latin1),
    Data = jsx:encode([{Key,
			erli_utils:to_proplist(Collection)},
		       {<<"meta">>, Meta}]),
    {Data, RD, Ctx}.

-spec process_post(rd(), term()) ->
			  {true, rd(), term()} |
			  {{halt, 409}, rd(), term()} |
			  {{halt, 415}, rd(), term()} |
			  {{halt, 422}, rd(), term()}.
process_post(RD, Ctx) ->
    case
	mochiweb_util:parse_header(wrq:get_req_header("Content-Type",
						      RD))
    of
	{"application/x-www-form-urlencoded", _} ->
	    Form = erli_forms:http_body_to_form(wrq:req_body(RD)),
	    handle_post(Form, RD, Ctx);
	{"application/json", _} ->
	    Form = jsx:decode(wrq:req_body(RD)),
	    handle_post(Form, RD, Ctx);
	_ -> {{halt, 415}, RD, Ctx}
    end.

-spec delete_resource(rd(), {object_type(), object()}) ->
			     {true, rd(), {object_type(), object()}}.
delete_resource(RD, {ObjectType, Obj}) when ?is_object(ObjectType) ->
    {_RemovalState, NewObj} =
	erli_storage:request_removal(Obj),
    {true, RD, {ObjectType, NewObj}}.

-spec delete_completed(rd(), {object_type(), object()}) ->
			      {boolean(), rd(), {object_type(), object()}}.
delete_completed(RD, {_ObjectType, Obj} = Ctx) when is_record(Obj, target) ->
    {Obj#target.is_banned, RD, Ctx};
delete_completed(RD, {_ObjectType, Obj} = Ctx) when is_record(Obj, path) ->
    {Obj#path.is_banned, RD, Ctx}.


%%----------------------------------------------------------
%% Internal Methods
%%----------------------------------------------------------

-spec maybe_record_visit(rd(), {path, #path{}}) ->
				ignore | #visit{}.
maybe_record_visit(RD, {path, Record}) ->
    Loc = erli_utils:get_location(wrq:peer(RD)),
    Visit = #visit{path_id = Record#path.id,
		   geo_location = Loc},
    erli_storage:create(Visit);
maybe_record_visit(_RD, _) -> ignore.

-spec handle_post(proplist(), rd(), {collection_type(), collection()}) ->
			 {true, rd(), term()} |
			 {{halt, 409}, rd(), term()} |
			 {{halt, 422}, rd(), term()}.
handle_post(Form, RD, {targets, {_Meta, _Collection}} = Ctx) ->
    case erli_forms:validate(Form,
			     [{<<"target_url">>, [required, is_url]}])
    of
	valid ->
	    Target = #target{url =
				 proplists:get_value(<<"target_url">>, Form)},
	    maybe_store(targets, Target, RD, Ctx);
	Errors ->
	    Body = jsx:encode([{<<"formErrors">>, Errors}]),
	    NRD = erli_utils:add_json_response(RD, Body),
	    {{halt, 422}, NRD, Ctx}
    end;
handle_post(Form, RD, {paths, {_Meta, _Collection}} = Ctx) ->
    case erli_forms:validate(Form,
			     [{<<"target_id">>, [required, is_target_id]},
			      {<<"custom_id">>, [is_id]}])
    of
	valid ->
	    Path = build_record(path, Form),
	    maybe_store(paths, Path, RD, Ctx);
	Errors ->
	    Body = jsx:encode([{<<"formErrors">>, Errors}]),
	    NRD = erli_utils:add_json_response(RD, Body),
	    {{halt, 422}, NRD, Ctx}
    end.

-spec build_record(path, proplist()) -> #path{}.
build_record(path, Form) ->
    TargetId = proplists:get_value(<<"target_id">>, Form),
    case proplists:get_value(<<"custom_id">>, Form) of
	undefined -> #path{target_id = TargetId};
	Id -> #path{target_id = TargetId, id = Id}
    end.

-spec maybe_store(collection_type(), object(), rd(), term()) ->
			 {true, rd(), term()} |
			 {{halt, 409}, rd(), term()} |
			 {{halt, 422}, rd(), term()}.
maybe_store(CollectionType, Record, RD, Ctx) ->
    case erli_storage:create(Record) of
	{error,
	 {conflict, {SubmittedRecord, ExistingRecord}}} ->
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
	    Body = jsx:encode([{atom_to_binary(CollectionType,
					       latin1),
				erli_utils:to_proplist(SavedTarget)}]),
	    NRD = erli_utils:add_json_response(RD, Body),
	    {true, NRD, Ctx}
    end.

-spec root_elem_exists(object_type(), id()) ->
			      false | {boolean(), object()}.
root_elem_exists(ObjType, Id) ->
    case erli_storage:read(ObjType, Id) of
	{error, _Error} -> false;
	Obj when is_record(Obj, target) ->
	    {not Obj#target.is_banned, Obj};
	Obj when is_record(Obj, path) ->
	    {not Obj#path.is_banned, Obj}
    end.
