%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc Test suite for functional testing of the 'key'
%%% resource.
%%% @end
%%%==========================================================

-module(key_resource_SUITE).

-compile(export_all).

-include("models.hrl").
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetramp, {seconds, 30}}].

init_per_suite(Config) ->
    test_utils:init_suite(Config, keys).

end_per_suite(_Config) ->
    erli:stop().

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    test_utils:clear_all_mnesia_data().

groups() ->
    [].

all() ->
    [create_key,
     view_key,
     expire_key,
     view_expired_key].

create_key(Config) ->
    {skip, undef}.

view_key(Config) ->
    {skip, undef}.

expire_key(Config) ->
    {skip, undef}.

view_expired_key(Config) ->
    {skip, undef}.
