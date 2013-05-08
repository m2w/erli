%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc A resource representing target URLs.
%%% @end
%%%==========================================================

-module(erli_target_resource).

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
%% Webmachine Callbacks
%%-----------------------------------------------------------

-spec init([initial_state()]) -> {ok, initial_state()}.
init([InitialState]) ->
    {ok, InitialState}.

allowed_methods(RD, collection) ->
    {['GET', 'POST', 'HEAD', 'OPTIONS'], RD, collection};
allowed_methods(RD, entity) ->
    {['GET', 'DELETE', 'HEAD', 'OPTIONS'], RD, entity}.

malformed_request(RD, collection) ->
    case erli_utils:parse_range_header(RD, target) of
	{error, invalid_range} ->
	    ContentRange = "*/" ++ integer_to_list(erli_storage:count(target)),
	    NRD = wrq:set_resp_header("Content-Range", ContentRange, RD),
	    {true, NRD, collection};
	Range ->
	    {false, RD, {collection, Range}}
    end;
malformed_request(RD, entity) ->
    {false, RD, entity}.

options(RD, {collection, _}) ->
    {[{"Content-Length", "0"},
      {"Accept-Ranges", "targets"},
      {"Allow", "GET, POST, HEAD, OPTIONS"}], RD, collection};
options(RD, entity) ->
    {[{"Content-Length", "0"},
      {"Allow", "GET, DELETE, HEAD, OPTIONS"}], RD, entity}.

resource_exists(RD, {collection, Range}) ->
    Data = erli_storage:read_multiple(targets, Range),
    Meta = erli_utils:meta_proplist(target, Range),
    {true, RD, {Meta, Data}};
resource_exists(RD, entity) ->
    Id = list_to_binary(wrq:path_info(id, RD)),
    case erli_storage:read(target, Id) of
	{error, _Error} ->
	    {false, RD, entity};
	Record ->
	    {not Record#target.is_banned, RD, Record}
    end.

previously_existed(RD, Record) when is_record(Record, target) ->
    {Record#target.is_banned, RD, Record};
previously_existed(RD, Ctx) ->
    {false, RD, Ctx}.

generate_etag(RD, Ctx) ->
    Etag = erli_utils:generate_etag(Ctx),
    {Etag, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", as_json}], RD, Ctx}.

as_json(RD, Record) when is_record(Record, target) ->
    Data = jsx:encode([{<<"posts">>, erli_utils:to_proplist(Record)}]),
    {Data, RD, Record};
as_json(RD, {Meta, Objects}=Ctx) ->
    Data = jsx:encode([{<<"posts">>, erli_utils:to_proplist(Objects)},
		       {<<"meta">>, Meta}]),
    ContentRangeHeader = erli_utils:build_content_range_header(targets, Meta),
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

delete_resource(RD, Record) when is_record(Record, target) ->
    {_State, UpdatedRecord} =  erli_storage:request_removal(Record),
    {true, RD, UpdatedRecord}.

delete_completed(RD, Record) when is_record(Record, target) ->
    {Record#target.is_banned, RD, Record}.

%%----------------------------------------------------------
%% Internal Methods
%%----------------------------------------------------------

-spec handle_post([{bitstring(), term()}], #wm_reqdata{}, term()) ->
			 {true | {halt, 422}, #wm_reqdata{}, term()}.
handle_post(Form, RD, Ctx) ->
    case erli_forms:validate(Form, [{target_url, [required, is_url]}]) of
	valid ->
	    Target = #target{url=proplists:get_value(target_url, Form)},
	    maybe_store(Target, RD);
	Errors ->
	    Body = jsx:encode([{<<"formErrors">>, Errors}]),
	    NRD = erli_utils:add_json_response(RD, Body),
	    {{halt, 422}, NRD, Ctx}
    end.

-spec maybe_store(#target{}, #wm_reqdata{}) ->
			 {true | {halt, 422}, #wm_reqdata{}, term()}.
maybe_store(Target, RD) ->
    %% @TODO: rethink the use of the 422 and the body contents when storage fails
    case erli_storage:create(Target) of
	{error, Error} ->
	    Body = jsx:encode([{<<"formErrors">>,
				atom_to_list(Error)}]),
	    NRD = erli_utils:add_json_response(RD, Body),
	    {{halt, 422}, NRD, Target};
	SavedTarget ->
	    Body = jsx:encode([{<<"posts">>,
				erli_utils:to_proplist(SavedTarget)}]),
	    NRD = erli_utils:add_json_response(RD, Body),
	    {true, NRD, Target}
    end.
