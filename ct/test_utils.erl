%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc Utility functions to simplify testing.
%%% @end
%%%==========================================================

-module(test_utils).

%% API
-export([clear_all_mnesia_data/0,
	 generate_targets/1]).

-include("models.hrl").

%%-----------------------------------------------------------
%% API Methods
%%-----------------------------------------------------------

%% @TODO: make gen_server so that the env vars can stay in state

clear_all_mnesia_data() ->
    mnesia:clear_table(counters),
    mnesia:clear_table(targets),
    mnesia:clear_table(paths),
    mnesia:clear_table(visits).

generate_targets(0) ->
    ok;
generate_targets(Count) ->
    {ok, FL} = application:get_env(erli, flag_limit),
    Domain = generate_domain(),
    TLD = generate_tld(),
    Url = <<"http://", Domain/bitstring, ".", TLD/bitstring>>,
    FC = random:uniform(25),
    Target = #target{url=Url, flag_count=FC, is_banned=FC>FL},
    case erli_storage:create(Target) of
	{error, _} ->
	    generate_targets(Count);
	_Rec ->
	    generate_targets(Count - 1)
    end.


generate_domain() ->
    generate_bin(random:uniform(30)).
generate_tld() ->
    generate_bin(random:uniform(3)).

generate_bin(Len) ->
    re:replace(base64:encode(crypto:rand_bytes(Len)),
	       "[\/+=]", "",
	       [global, {return, binary}]).
