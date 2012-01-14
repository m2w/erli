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

