%%%=============================================================================
%%% SETTINGS MACROS
%%% @doc Macros that may be modified to change default behaviour of erli.
%%% @end
%%%=============================================================================
% @doc The number of reports before Target URLs are banned.
-define(FLAG_LIMIT, 5).
% @doc The maximal number of attempts to generate a unique URL path.
-define(MAX_CONFLICTS, 100).

%%%=============================================================================
%%% ERLI RECORDS
%%%=============================================================================

-record(target, {target :: binary(), rep_num=0 :: integer()}).
-record(timeslots, {night=0 :: integer(), morning=0 :: integer(),
		    afternoon=0 :: integer(), evening=0 :: integer()}).
-record(path, {path :: binary(), target_url :: binary(), total_clicks=0 :: integer(),
	       visitors_by_country=sets:new(), visitors_by_time=#timeslots{},
	       deleted=false :: boolean()}).
-record(visitor, {country_code :: string(), hour :: integer()}).
