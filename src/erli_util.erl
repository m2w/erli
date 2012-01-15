%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011-2012 Moritz Windelen.
%% @doc erli utility methods.

-module(erli_util).
-author('Moritz Windelen <moritz@tibidat.com>').

-export([is_valid_url/1]).

%%------------------------------------------------------------------------------
%% @spec is_valid_url(Url::binary()) -> true | false
%% @doc Returns whether the URL complies with a validation regex that ensures
%%      only valid URL/URIs are accepted. Currently only ASCII URLs count as
%%      being `valid'.
%% @end
%%------------------------------------------------------------------------------
is_valid_url(Url) ->
    {ok, Re} = re:compile("^(?:[a-zA-Z0-9]+:\/\/)" % compulsory schema
			  "(?:(?:(?:[a-zA-Z0-9]+\.)+(?:[a-zA-Z]+))|" % standard
						                     % URLs
			  "(?:(?:[0-9]+\.){3}(?:[0-9]+))|" % IPs
			  "(?:(?:[a-f0-9]+\:)+(?:[a-f0-9]+)))(?:(?:\s*$)|" % IPv6
			  "(?:(?:[:\/?]).+$))", % any query/path suffixes
			  [dotall]),
    case re:run(Url, Re, [{capture, none}]) of
	match ->
	    true;
	nomatch ->
	    false
    end.

%%%=============================================================================
%%% Tests
%%%=============================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_valid_url_test_() ->
    [?_assertNot(is_valid_url(<<"2001:0db8:0000:08d3:0000:8a2e:0070?7344=asdf">>)),
     ?_assertNot(is_valid_url(<<"google.com">>)),
     ?_assertNot(is_valid_url(<<"123.1.1.123">>)),
     ?_assertNot(is_valid_url("google.com")),
     ?_assert(is_valid_url("http://google.com")),
     ?_assert(is_valid_url(<<"http://www.google.de/search?client=safari&rls=en&"
			     "q=erlang&ie=UTF-8&oe=UTF-8&redir_esc=&ei="
			     "JB4ST4_0OIHTsgbN0uQu&gbv=1&sei=JB4ST8LoPM7FtAbRnMRB">>)),
     ?_assert(is_valid_url(<<"http://google.com">>)),
     ?_assert(is_valid_url(<<"mailto://bob@bob.com">>)),
     ?_assert(is_valid_url(<<"http://192.0.0.1:1234">>)),
     ?_assert(is_valid_url(<<"smtp://2001:0db8:0000:08d3:0000:8a2e:0070:7344">>)),
     ?_assert(is_valid_url(<<"https://2001:0db8:0000:08d3:0000:8a2e:0070:7344?asdf=asdf">>)),
     ?_assert(is_valid_url(<<"http://google.com:1234?test=harro">>)),
     ?_assert(is_valid_url(<<"ssh://me@google.com">>))].

-endif.
