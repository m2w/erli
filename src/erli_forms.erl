%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc A modular form validation library.
%%% @end
%%%==========================================================

-module(erli_forms).

%% API
-export([http_body_to_form/1,
	 validate/2,
	 is_url/1]).

-export_type([validator_fun/0, validator/0]).

%%-----------------------------------------------------------
%% Types
%%-----------------------------------------------------------

-type value() :: term().
-type key() :: value().
-type form() :: [{bitstring(), term()}].
-type validator() :: atom().
-type error_description() :: string() | bitstring().
-type validator_fun() :: fun((value()) -> valid | error_description()).
-type validation_error() :: {atom(), error_description()}.

%%-----------------------------------------------------------
%% API Methods
%%-----------------------------------------------------------

-spec http_body_to_form(bitstring() | string()) -> form().
http_body_to_form(Body) when is_binary(Body) ->
    KVPs = binary:split(Body, <<"&">>, [global, trim]),
    lists:map(fun(KVP) ->
		      case binary:split(KVP, <<"=">>, [trim]) of
			  [Key, Value] -> {Key, Value};
			  [Key] -> {Key, true}
		      end
	      end, KVPs);
http_body_to_form(Body) when is_list(Body) ->
    KVPs = re:split(Body, <<"&">>, [trim]),
    lists:map(fun(KVP) ->
		      case re:split(KVP, <<"=">>, [trim]) of
			  [Key, Value] -> {Key, Value};
			  [Key] -> {Key, true}
		      end
	      end, KVPs).

-spec validate(form(), [{key(), [validator() | validator_fun()]}]) ->
		      valid | [validation_error()].
validate(Form, ValidatorSpecs) ->
    validate(Form, ValidatorSpecs, []).

validate(Form, [{Key, Validators}|RemSpecs], Issues) ->
    Value = proplists:get_value(Key, Form),
    IsRequired = lists:member(required, Validators),
    case {Value, IsRequired}  of
	{undefined, true} ->
	    validate(Form, RemSpecs, [{Key, [<<"field is required">>]}|Issues]);
	{undefined, false} ->
	    validate(Form, RemSpecs, Issues);
	{Val, _} ->
	    case run_validators(Val, Validators) of
		[] ->
		    validate(Form, RemSpecs, Issues);
		NewIssues ->
		    IssuesForKey = {Key, NewIssues},
		    validate(Form, RemSpecs, [IssuesForKey|Issues])
	    end
    end;
validate(_Form, [], []) ->
    valid;
validate(_Form, [], Issues) ->
    Issues.


-spec is_url(bitstring()) -> boolean().
is_url(PossibleUrl) ->
    match =:= re:run(PossibleUrl,
		     <<"\\b((?:https?://|www\\d{0,3}[.]|[a-z0-9.\\-]"
		       "+[.][a-z]{2,4}/)(?:[^\\s()<>]+|\\(([^\\s()<>]"
		       "+|(\\([^\s()<>]+\\)))*\\))+(?:\\(([^\\s()<>]"
		       "+|(\\([^\\s()<>]+\\)))*\\)|[^\\s`!()\\[\\]{};"
		       ":'\".,<\>?«»“”‘’]))">>, [{capture, none}]).

%%-----------------------------------------------------------
%% Internal Methods
%%-----------------------------------------------------------

run_validators(FieldValue, Validators) ->
    run_validators(FieldValue, Validators, []).

run_validators(Value, [Validator|RemValidators], Issues) ->
    UpdatedIssues = check(Value, Validator, Issues),
    run_validators(Value, RemValidators, UpdatedIssues);
run_validators(_Value, [], Issues) ->
    Issues.

check(Value, Validator, Issues) when is_function(Validator) ->
    case Validator(Value) of
	valid ->
	    Issues;
	Issue ->
	    [Issue|Issues]
    end;
check(Value, is_url, Issues) ->
    case is_url(Value) of
	true ->
	    Issues;
	false ->
	    [<<"is not a valid URL">>|Issues]
    end;
check(Value, is_target_id, Issues) ->
    case erli_storage:read(target, Value) of
	{error, _Error} ->
	    [<<"target record with id '",
	       Value/bitstring, "' does not exist">>|Issues];
	_T ->
	    Issues
    end;
check(_Value, _, Issues) ->
    Issues.
