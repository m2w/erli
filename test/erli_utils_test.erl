%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2012-2013 Moritz Windelen.

%% @doc Test suite for erli utility methods.

-module(erli_utils_test).

-include_lib("eunit/include/eunit.hrl").

get_env_test_() ->
    {setup,
     fun() ->
	     application:set_env(erli, key, <<"value">>),
	     application:set_env(erli, second_key, theValue)
     end,
     [?_assertEqual(<<"value">>, erli_utils:get_env(key)),
      ?_assertEqual(undefined, erli_utils:get_env(wrong_key)),
      ?_assertEqual(theValue, erli_utils:get_env(second_key)),
      ?_assertEqual(theDefault, erli_utils:get_env(wrong_key, theDefault)),
      ?_assertEqual(theValue, erli_utils:get_env(second_key, aDefaultValue))]}.

is_valid_url_test_() ->
    [?_assertNot(erli_utils:is_valid_url(<<"2001:0db8:0000:08d3:0000:8a2e:0070?7344=asdf">>)),
     ?_assertNot(erli_utils:is_valid_url(<<"google.com">>)),
     ?_assertNot(erli_utils:is_valid_url(<<"123.1.1.123">>)),
     ?_assertNot(erli_utils:is_valid_url("google.com")),
     ?_assert(erli_utils:is_valid_url("http://google.com")),
     ?_assert(erli_utils:is_valid_url(<<"http://www.google.de/search?client=safari&rls=en&"
					"q=erlang&ie=UTF-8&oe=UTF-8&redir_esc=&ei="
					"JB4ST4_0OIHTsgbN0uQu&gbv=1&sei=JB4ST8LoPM7FtAbRnMRB">>)),
     ?_assert(erli_utils:is_valid_url(<<"http://google.com">>)),
     ?_assert(erli_utils:is_valid_url(<<"mailto://bob@bob.com">>)),
     ?_assert(erli_utils:is_valid_url(<<"http://192.0.0.1:1234">>)),
     ?_assert(erli_utils:is_valid_url(<<"smtp://2001:0db8:0000:08d3:0000:8a2e:0070:7344">>)),
     ?_assert(erli_utils:is_valid_url(<<"https://2001:0db8:0000:08d3:0000:8a2e:0070:7344?asdf=asdf">>)),
     ?_assert(erli_utils:is_valid_url(<<"http://google.com:1234?test=harro">>)),
     ?_assert(erli_utils:is_valid_url(<<"ssh://me@google.com">>))].

check_for_accept_type_test_() ->
    [?_assertEqual(html, erli_utils:check_for_accept_type("text/html")),
     ?_assertEqual(html, erli_utils:check_for_accept_type(<<"text/html">>)),
     ?_assertEqual(json, erli_utils:check_for_accept_type("application/json")),
     ?_assertEqual(html, erli_utils:check_for_accept_type("*/*")),
     ?_assertEqual(html, erli_utils:check_for_accept_type("text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")),
     ?_assertEqual(html, erli_utils:check_for_accept_type("application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")),
     ?_assertEqual(json, erli_utils:check_for_accept_type("application/json,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")),
     ?_assertEqual(json, erli_utils:check_for_accept_type("application/json,application/xml;q=0.9"))].
