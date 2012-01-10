%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2011 Moritz Windelen.
%% @doc erli utility methods.

%% TODO: make this a gen_server

-module(erli_util).
-author('Moritz Windelen <moritz@tibidat.com>').

-export([is_valid_url/1]).

-include_lib("webmachine/include/webmachine.hrl").
-include("erli.hrl").

is_valid_url(Url) ->
    {ok, Re} = re:compile("^(?:[a-zA-Z0-9]+:\/\/)(?:(?:(?:[a-zA-Z0-9]+\.)+(?:[a-zA-Z]+))|(?:(?:[0-9]+\.){3}(?:[0-9]+))|(?:(?:[a-f0-9]+\:)+(?:[a-f0-9]+)))(?:(?:\s*$)|(?:(?:[:\/?]).+$))", [dotall]),
    case re:run(Url, Re, [{capture, none}]) of
	match ->
	    true;
	nomatch ->
	    false
    end.

