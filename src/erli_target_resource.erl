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

resource_exists(RD, {collection, {Start, End}=Range}) ->
    Data = erli_storage:read_multiple(targets, Range),
    M = erli_utils:meta_proplist(target, Range),
    ColSize = proplists:get_value(<<"totalCollectionSize">>, M),
    %% special case: less total objects than the default offset
    %% @TODO: this should be hotswapped out once the demo reaches over 25 records
    {E, Meta} = if End > ColSize ->
		M2 = lists:keyreplace(<<"objectCount">>,
				      1, M, {<<"objectCount">>, ColSize}),
		M3 = lists:keyreplace(<<"rangeEnd">>,
				      1, M2, {<<"rangeEnd">>, ColSize}),
		{ColSize, M3};
	   true -> {End, M}
	end,
    CR = "targets " ++ integer_to_list(Start) ++ "-" ++
	integer_to_list(E) ++ "/" ++ integer_to_list(ColSize),
    NRD = wrq:set_resp_header("Content-Range", CR, RD),
    {true, NRD, {Meta, Data}};
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
    {Data, RD, Ctx}.

process_post(RD, collection=Ctx) ->
    case mochiweb_util:parse_header(wrq:get_req_header("Content-Type", RD)) of
	{"application/x-www-form-urlencoded", _} ->
	    handle_post(url_form, RD, Ctx);
	{"application/json", _} ->
	    handle_post(json, RD, Ctx)
    end.

delete_resource(RD, Record) when is_record(Record, target) ->
    {_State, UpdatedRecord} =  erli_storage:request_removal(Record),
    {true, RD, UpdatedRecord}.

delete_completed(RD, Record) when is_record(Record, target) ->
    {Record#target.is_banned, RD, Record}.

%%----------------------------------------------------------
%% Internal Methods
%%----------------------------------------------------------

-spec handle_post(url_form | json, #wm_reqdata{}, term()) ->
			 {true | {halt, 422}, #wm_reqdata{}, term()}.
handle_post(url_form, RD, Ctx) ->
    Form = erli_forms:http_body_to_form(wrq:req_body(RD)),
    case erli_forms:validate(Form, [{target_url, [required, is_url]}]) of
	valid ->
	    Target = #target{url=proplists:get_value(target_url, Form)},
	    maybe_store(Target, RD);
	Errors ->
	    Body = jsx:encode([{<<"formErrors">>, Errors}]),
	    NRD = erli_utils:add_json_response(RD, Body),
	    {{halt, 422}, NRD, Ctx}
    end;
handle_post(json, RD, Ctx) ->
    FormData = jsx:decode(wrq:req_body(RD)),
    TargetUrl = proplists:get_value(<<"target_url">>, FormData),
    case erli_forms:is_url(TargetUrl) of
	true ->
	    Target = #target{url=TargetUrl},
	    maybe_store(Target, RD);
	false ->
	    Body = jsx:encode([{<<"formErrors">>,
				[{<<"target_url">>,
				  <<"is not a valid URL">>}]}]),
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
	    Body = jsx:encode(erli_utils:to_proplist(SavedTarget)),
	    NRD = erli_utils:add_json_response(RD, Body),
	    {true, NRD, Target}
    end.
