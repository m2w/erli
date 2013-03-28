%% @author Moritz Windelen <moritz@tibidat.com>
%% @copyright 2012-2013 Moritz Windelen.
%% @doc erli utility methods.

-module(erli_utils).
-author('Moritz Windelen <moritz@tibidat.com>').

%% API
-export([format_req/1, generate_req/1,
	 get_env/1, get_env/2, is_valid_url/1,
	 is_erli_running/0]).

-include("webmachine/include/webmachine_logger.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(URL_REGEX,
	["^(?:[a-zA-Z0-9]+://)(?:(?:(?:[a-zA-Z0-9]+.)+("
	 "?:[a-zA-Z]+))|(?:(?:[0-9]+.){3}(?:[0-9]+))|(?"
	 ":(?:[a-f0-9]+:)+(?:[a-f0-9]+))(?:(?: "
	 "*$)|(?:(?:[:/?]).+$)))"]).
-define(HTML_REGEX, <<".*text/html.*">>).
-define(JSON_REGEX, <<".*application/json.*">>).
-define(ANY_REGEX, <<".*\\*/\\*.*">>).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Naive check whether erli is running on this system.
-spec is_erli_running() -> ok | {error, not_running}.
is_erli_running() ->
    {ok, Nodes} = net_adm:names(),
    case check_for_erli(Nodes) of
	found ->
	    ok;
	not_found ->
	    {error, not_running}
    end.

%% @doc Provides a simple way to generate valid webmachine requests for
%% testing purposes.
-spec generate_req(Path :: string()) -> #wm_reqdata{}.
generate_req(Path) ->
    Req0 = wrq:create(get, "1.1", Path, gb_trees:empty()),
    {ok, CWD} = file:get_cwd(),
    DispatchPath = case lists:reverse(CWD) of
		       "tinue." ++ _Rest ->
			   filename:join("../priv", "dispatch.conf");
		       _Other ->
			   filename:join("./priv", "dispatch.conf")
		   end,
    {ok, Dispatcher} = file:consult(DispatchPath),
    {_Mod, _ModOpts, HostTokens, Port, PathTokens, Bindings,
     AppRoot, StringPath} =
	webmachine_dispatcher:dispatch(Path, Dispatcher, Req0),
    wrq:load_dispatch_data(Bindings, HostTokens, Port,
			   PathTokens, AppRoot, StringPath, Req0).

%% @doc If an application env var has been specified for the `Key', it is
%% returned, else the function returns `undefined'.
-spec get_env(Key :: atom()) -> undefined | term().
get_env(Key) ->
    case application:get_env(erli, Key) of
	undefined ->
	    undefined;
	{ok, Val} ->
	    Val
    end.

%% @doc If an application env var has been specified for the `Key', it is
%% returned, else the function returns the default value.
-spec get_env(Key :: atom(), Default :: term()) -> term().
get_env(Key, Default) ->
    case application:get_env(erli, Key) of
	undefined ->
	    Default;
	{ok, Val} ->
	    Val
    end.

%% @doc Returns whether the URL complies with a validation regex that ensures
%% only valid URL/URIs are accepted. Currently only ASCII URLs count as
%% being `valid'.
-spec is_valid_url(Url :: binary()) -> boolean().
is_valid_url(Url) ->
    case re:run(Url, ?URL_REGEX,
		[dotall, caseless, {capture, none}]) of
	match ->
	    true;
	nomatch ->
	    false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Iterate over a list of Nodes to see if erli is running.
-spec check_for_erli([{string(), integer()}]) -> found | not_found.
check_for_erli([{"erli_node", _Port} | _Nodes]) ->
    found;
check_for_erli([{_Name, _Port} | Nodes]) ->
    check_for_erli(Nodes);
check_for_erli([]) ->
    not_found.

%%--------------------------------------------------------------------
%% Everything below this comment is taken directly and unmodified from
%% `webmachine_logger.erl`, which is part of the webmachine project:
%%
%% -author Justin Sheehy <justin@basho.com>
%% -author Andy Gross <andy@basho.com>
%% -copyright 2007-2008 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.
%%
%%--------------------------------------------------------------------
format_req(#wm_log_data{method = Method,
			headers = Headers, peer = Peer, path = Path,
			version = Version, response_code = ResponseCode,
			response_length = ResponseLength}) ->
    User = "-",
    Time = fmtnow(),
    Status = integer_to_list(ResponseCode),
    Length = integer_to_list(ResponseLength),
    Referer = case mochiweb_headers:get_value("Referer",
					      Headers)
	      of
		  undefined -> "";
		  R -> R
	      end,
    UserAgent = case
		    mochiweb_headers:get_value("User-Agent", Headers)
		of
		    undefined -> "";
		    U -> U
		end,
    fmt_alog(Time, Peer, User, fmt_method(Method), Path,
	     Version, Status, Length, Referer, UserAgent).

fmt_method(M) when is_atom(M) -> atom_to_list(M).

fmtnow() ->
    {{Year, Month, Date}, {Hour, Min, Sec}} =
	calendar:local_time(),
    io_lib:format("[~2..0w/~s/~4..0w:~2..0w:~2..0w:~2..0w "
		  "~s]",
		  [Date, month(Month), Year, Hour, Min, Sec, zone()]).

zone() ->
    Time = erlang:universaltime(),
    LocalTime = calendar:universal_time_to_local_time(Time),
    DiffSecs =
	calendar:datetime_to_gregorian_seconds(LocalTime) -
	calendar:datetime_to_gregorian_seconds(Time),
    zone(DiffSecs / 3600 * 100).

fmt_alog(Time, Ip, User, Method, Path, {VM, Vm}, Status,
	 Length, Referrer, UserAgent) ->
    [fmt_ip(Ip), " - ", User, [$\s], Time, [$\s, $"],
     Method, " ", Path, " HTTP/", integer_to_list(VM), ".",
     integer_to_list(Vm), [$", $\s], Status, [$\s], Length,
     [$\s, $"], Referrer, [$", $\s, $"], UserAgent,
     [$", $\n]].

fmt_ip(IP) when is_tuple(IP) -> inet_parse:ntoa(IP);
fmt_ip(undefined) -> "0.0.0.0";
fmt_ip(HostName) -> HostName.

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%% Ugly reformatting code to get times like +0000 and -1300
zone(Val) when Val < 0 ->
    io_lib:format("-~4..0w", [trunc(abs(Val))]);
zone(Val) when Val >= 0 ->
    io_lib:format("+~4..0w", [trunc(abs(Val))]).
