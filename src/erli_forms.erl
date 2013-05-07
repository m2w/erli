%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc A modular form validation library.
%%% Forms are validated as proplists, with validator functions
%%% of the type {key, [validators]}.
%%% Comes with a number of common validators built-in.
%%% @end
%%%==========================================================

-module(erli_forms).

%% API
-export([http_body_to_form/1,
	 validate/2]).

-export_type([validator_fun/0, validator/0]).

-type value() :: term().
-type key() :: value().
-type form() :: [{atom(), term()}].
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
			  [Key, Value] -> {binary_to_atom(Key, latin1), Value};
			  [Key] -> {binary_to_atom(Key, latin1), true}
		      end
	      end, KVPs);
http_body_to_form(Body) when is_list(Body) ->
    KVPs = re:split(Body, <<"&">>, [trim]),
    lists:map(fun(KVP) ->
		      case re:split(KVP, <<"=">>, [trim]) of
			  [Key, Value] -> {Key, Value};
			  [Key] -> {binary_to_atom(Key, latin1), true}
		      end
	      end, KVPs).

%% @TODO: implement the actual form validation
-spec validate(form(), [{key(), [validator() | validator_fun()]}]) ->
		      valid | [validation_error()].
validate(Form, Validators) ->
    case lists:map(fun(Validator) ->
			   check(Form, Validator)
		   end, Validators) of
	[] -> valid;
	ValidationErrors -> ValidationErrors
    end,
    valid.

%%-----------------------------------------------------------
%% Internal Methods
%%-----------------------------------------------------------

-spec check(form(), {key(), [validator() | validator_fun()]}) ->
		   valid | [validation_error()].
check(Form, {Key, [Validator|RemainingValidators]}) ->
    case proplists:lookup(Key, Form) of
	none -> is_required
    end.
