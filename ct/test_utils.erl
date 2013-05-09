%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc Utility functions to simplify testing.
%%% @end
%%%==========================================================

-module(test_utils).

%% API
-export([clear_all_mnesia_data/0,
	 generate_targets/1,
	 generate_paths/1]).

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

generate_paths(N) ->
    Targets = generate_targets(random:uniform(N)),
    generate_paths(N, Targets, []).

generate_paths(0, Targets, Acc) ->
    Acc;
generate_paths(N, Targets, Acc) ->
    X = random:uniform(length(Targets)),
    Id = (lists:nth(X, Targets))#target.id,
    case erli_storage:create(#path{target_id=Id}) of
	{error, Error} ->
	    error_logger:info_msg("error = ~s", [Error]),
	    generate_paths(N, Targets, Acc);
	P ->
	    generate_paths(N - 1, Targets, [P|Acc])
    end.

generate_targets(N) ->
    generate_targets(N, []).

generate_targets(0, Acc) ->
    Acc;
generate_targets(N, Acc) ->
    Target = make_target(),
    case erli_storage:create(Target) of
	{error, _} ->
	    generate_targets(N, Acc);
	T ->
	    generate_targets(N - 1, [T|Acc])
    end.

make_target() ->
    {ok, FL} = application:get_env(erli, flag_limit),
    Domain = generate_domain(),
    TLD = generate_tld(),
    Url = <<"http://", Domain/bitstring, ".", TLD/bitstring>>,
    FC = random:uniform(25),
    #target{url=Url, flag_count=FC, is_banned=FC>FL}.


generate_domain() ->
    generate_bin(random:uniform(30)).
generate_tld() ->
    generate_bin(random:uniform(3)).

generate_bin(Len) ->
    re:replace(base64:encode(crypto:rand_bytes(Len)),
	       "[\/+=]", "",
	       [global, {return, binary}]).
