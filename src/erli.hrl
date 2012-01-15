%%%=============================================================================
%%% SETTINGS MACROS
%%% @doc Macros that may be modified to change default behaviour of erli.
%%% @end
%%%=============================================================================
% @doc The number of reports before Target URLs are banned.
-define(FLAG_LIMIT, 5).
% @doc The maximal number of attempts to generate a unique URL path.
-define(MAX_CONFLICTS, 100).
% @doc The number of requests allowed during a set timespan.
-define(REQ_LIMIT, 100000).

%%%=============================================================================
%%% ERLI RECORDS
%%% @end
%%%=============================================================================
%%------------------------------------------------------------------------------
%% @type target() = #target{target=binary(),
%%                          paths=list(),
%%                          reported=interger(),
%%                          rep_num=integer()}
%%------------------------------------------------------------------------------
-record(target, {target, paths=[], reported=0, rep_num=0}).
%%------------------------------------------------------------------------------
%% @type timeslots() = #timeslots{night=integer(),
%%                                morning=integer(),
%%                                afternoon=integer(),
%%                                evening=integer()}
%%------------------------------------------------------------------------------
-record(timeslots, {night=0, morning=0, afternoon=0, evening=0}).
%%------------------------------------------------------------------------------
%% @type path() = #path{path=string(),
%%                      total_clicks=integer(),
%%                      unique_clicks=integer(),
%%                      country_lst=list(),
%%                      timeslot_visits=timeslots()}
%%------------------------------------------------------------------------------
-record(path, {path, total_clicks=0, unique_clicks=0, country_lst=[],
	       timeslot_visits=#timeslots{}}).
%%------------------------------------------------------------------------------
%% @type visitor_ip = #visitor_ip{visitor_ip=string(), path=list()}
%%------------------------------------------------------------------------------
-record(visitor_ip, {visitor_ip, paths=[]}).




