%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc Data models used throughout erli.
%%% Contains the record and type declarations for all models.
%%% @end
%%%==========================================================

%%-----------------------------------------------------------
%% Internal Types
%%-----------------------------------------------------------

-type id() :: bitstring().
-type incremental_id() :: non_neg_integer().
-type unix_timestamp() :: integer().
-type url() :: bitstring().

%%-----------------------------------------------------------
%% Records
%%-----------------------------------------------------------

%% @doc Contains all information on a target URL.
%% This includes links to all related data.
-record(target, {id :: id(),
		 record_number :: incremental_id(),
		 url :: url(),
		 last_modified :: unix_timestamp(),
		 is_banned=false :: boolean(),
		 flag_count=0 :: non_neg_integer(),
		 screenshot_id=undefined :: undefined | id()}).

%% @doc Contains all information on a shortened URL.
%% This includes links to all related data.
-record(path, {id :: id(),
	       record_number :: incremental_id(),
	       target_id :: id(),
	       is_banned=false :: boolean()}).

%% @doc Represents a single visit to a shortened URL.
%% Includes links to all related data.
-record(visit, {id :: incremental_id(),
		path :: id(),
		peer,
		time :: unix_timestamp()}).

%%-----------------------------------------------------------
%% External Types
%%-----------------------------------------------------------

-type model_name() :: target | path | visit.
-type model() :: #target{} | #path{} | #visit{}.
-type initial_state() :: collection | entity.
-type query_range() :: {pos_integer(), pos_integer()}.
