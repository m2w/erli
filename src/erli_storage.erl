%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc Simple API to interact with the storage backend.
%%% The storage module performs minimal validation before
%%% write operations and contains a number of utility methods
%%% to facilitate interaction with the stored data beyond CRUD.
%%% @end
%%%==========================================================

-module(erli_storage).

%% API
-export([create/1,
	 read/2,
	 read_multiple/2,
	 request_removal/1,
	 count/1]).
%% Utility API
-export([setup_tables/1]).

-include("models.hrl").

%%-----------------------------------------------------------
%% Types
%%-----------------------------------------------------------

%%-----------------------------------------------------------
%% API Methods
%%-----------------------------------------------------------

-spec create(model()) -> model() | {error, atom() | {atom(), {model(), model()}}}.
create(Obj) when is_record(Obj, target) ->
    RecordNumber = mnesia:dirty_update_counter(counters, target, 1),
    LastModified = erli_utils:unix_timestamp(),
    Object = Obj#target{record_number=RecordNumber, last_modified=LastModified},
    case mnesia:dirty_index_read(target, Object#target.url, url) of
	[] ->
	    case generate_id(target) of
		{error, Error} ->
		    {error, Error};
		Id ->
		    UpdatedObject = Object#target{id=Id},
		    ok = mnesia:dirty_write(target, UpdatedObject),
		    UpdatedObject
	    end;
	[ConflictingRecord] ->
	    {error, {conflict, {Object#target{id= <<"none">>}, ConflictingRecord}}}
    end;
create(Obj) when is_record(Obj, path) ->
    RecordNumber = mnesia:dirty_update_counter(counters, path, 1),
    Object = Obj#path{record_number=RecordNumber},
    case Object#path.id of
	undefined ->
	    case generate_id(path) of
		{error, Error} ->
		    {error, Error};
		Id ->
		    UpdatedObject = Object#path{id=Id},
		    ok = mnesia:dirty_write(path, UpdatedObject),
		    UpdatedObject
	    end;
	Id ->
	    case read(path, Id) of
		{error, not_found} ->
		    ok = mnesia:dirty_write(path, Object),
		    Object;
		{error, Error} ->
		    {error, Error};
		_Record ->
		    {error, conflict}
	    end
    end;
create(Object) when is_record(Object, visit) ->
    Id = mnesia:dirty_update_counter(counters, visit, 1),
    UpdatedObject = Object#visit{id=Id},
    ok = mnesia:dirty_write(visit, UpdatedObject),
    UpdatedObject.

-spec read(model_name(), id()) -> model() | {error, atom()}.
read(target, Id) ->
    wrap_read(mnesia:dirty_read(target, Id));
read(path, Id) ->
    wrap_read(mnesia:dirty_read(path, Id));
read(visit, Id) ->
    wrap_read(mnesia:dirty_read(visit, Id)).

-spec read_multiple(model_name(), query_range()) -> list().
read_multiple(targets, {Start, End}) ->
    mnesia:dirty_select(target, [{#target{record_number='$1', _='_'},
				  [{'>=', '$1', Start},
				   {'=<', '$1', End}],
				  ['$_']}]);
read_multiple(paths, {Start, End}) ->
    mnesia:dirty_select(path, [{#path{record_number='$1', _='_'},
				[{'>=', '$1', Start},
				 {'=<', '$1', End}],
				['$_']}]);
read_multiple(visits, {Start, End}) ->
    mnesia:dirty_select(visit, [{#visit{id='$1', _='_'},
				 [{'>=', '$1', Start},
				  {'=<', '$1', End}],
				 ['$_']}]).

-spec request_removal(#target{}) ->
			     {request_accepted | target_banned, #target{}}.
request_removal(Target) when is_record(Target, target) ->
    CurrentLimit = erli_utils:get_env(flag_limit),
    case Target#target.flag_count of
	FC when FC < CurrentLimit ->
	    LastModified = erli_utils:unix_timestamp(),
	    UpdatedTarget = Target#target{flag_count=FC+1,
					  last_modified=LastModified},
	    mnesia:dirty_write(target, UpdatedTarget),
	    {request_accepted, UpdatedTarget};
	FC when FC =:= CurrentLimit ->
	    LastModified = erli_utils:unix_timestamp(),
	    UpdatedTarget = Target#target{last_modified=LastModified,
					  is_banned=true},
	    ban(UpdatedTarget),
	    {target_banned, UpdatedTarget}
    end.

-spec count(model_name()) -> non_neg_integer().
count(Model) ->
    mnesia:table_info(Model, size).


%%-----------------------------------------------------------
%% Utility API
%%-----------------------------------------------------------

-spec setup_tables([node()]) -> ok.
setup_tables(Nodes) ->
    ok = mnesia:start(), % ensure mnesia is started
    fix_schema(Nodes),
    create_tables(Nodes).

%%-----------------------------------------------------------
%% Internal Methods
%%-----------------------------------------------------------

-spec create_tables([node()]) -> ok.
create_tables(Nodes) ->
    maybe_create_table(counters, [{disc_copies, Nodes},
				  {attributes, [type, id]}]),
    maybe_create_table(target, [{disc_copies, Nodes},
				 {index, [url, record_number]},
				 {attributes, record_info(fields, target)}]),
    maybe_create_table(path, [{disc_copies, Nodes},
			       {index, [target_id, record_number]},
			       {attributes, record_info(fields, path)}]),
    maybe_create_table(visit, [{disc_copies, Nodes},
				{index, [path]},
				{attributes, record_info(fields, visit)}]).

-spec maybe_create_table(atom(), TableSpec :: list()) -> ok.
maybe_create_table(TabName, TabSpec) ->
    case mnesia:create_table(TabName, TabSpec) of
	{atomic, ok} ->
	    ok;
	{aborted, {already_exists, _}} ->
	    error_logger:warning_msg(
	      "Table ~s already exists, ASSUMING its definition is up-to-date!",
	      [TabName]),
	    ok
    end.

-spec fix_schema([node()]) -> ok.
fix_schema(Nodes) ->
    ExistingRamCopies = mnesia:table_info(schema, ram_copies),
    NodesWithRamCopies =
	[X || X <- Nodes, lists:member(X, ExistingRamCopies)],
    [{atomic, ok} = mnesia:change_table_copy_type(schema, Node, disc_copies)
     || Node <- NodesWithRamCopies],
    DiskCopies = mnesia:table_info(schema, disc_copies),
    NodesWithoutSchema =
	[X || X <- Nodes, not lists:member(X, DiskCopies)],
    case NodesWithoutSchema of
	[] ->
	    ok;
	N ->
	    stopped = mnesia:stop(), % take mnesia offline for schema creation
	    ok = mnesia:create_schema(N),
	    ok = mnesia:start() % restart mnesia
    end.


-spec wrap_read(list() | {aborted, atom()}) -> model() | {error, atom()}.
wrap_read([]) ->
    {error, not_found};
wrap_read([Record]) ->
    Record;
wrap_read({aborted, Error}) ->
    {error, Error}.

-spec ban(#target{}) -> {atomic, integer()} | {aborted, atom()}.
ban(Target) when is_record(Target, target) ->
    mnesia:transaction(
      fun() ->
	      AffectedPaths =
		  mnesia:select(path, [{#path{target_id='$1', _='_'},
					[{'=', '$1', Target#target.id}],
					['$_']}]),
	      lists:map(fun(Path) ->
				UpdatedPath = Path#path{is_banned=true},
				mnesia:write(UpdatedPath)
			end, AffectedPaths),
	      mnesia:write(Target),
	      length(AffectedPaths)
      end).

-spec extract({list(), Continuation::term()} | '$end_of_table') -> list().
extract(Res) ->
    extract(Res, []).

-spec extract({list(), Continuation::term()} | '$end_of_table', list()) -> list().
extract('$end_of_table', Acc) ->
    Acc;
extract({Objects, Cont}, Acc) ->
    extract(mnesia:select(Cont), Acc ++ Objects).

-spec generate_id(target | path) -> id() | {error, unable_to_generate_id}.
generate_id(Table) ->
    generate_id(Table, 0).

-spec generate_id(target | path, integer()) -> id() | {error, unable_to_generate_id}.
generate_id(Table, Attempts) when Attempts < 20 ->
    Id = re:replace(
	   base64:encode(
	     crypto:rand_bytes(3)),
	   "[\/+=]", "",
	   [global, {return, binary}]),
    case mnesia:dirty_read(Table, Id) of
	[] ->
	    Id;
	_Record ->
	    generate_id(Table, Attempts+1)
    end;
generate_id(_Table, _Attempts) ->
    {error, unable_to_generate_id}.
