-module(simple_cache).

-export([insert/2, delete/1, lookup/1, drop/0]).

%% @doc
%% insert/2 will place a new {Key, Value} pair into the simple_cache
%% application, it will ignore whatever is already there by replacing
%% the item currently in the cache.
%% @spec insert(term(), value()) -> ok | {error, Reason}
insert(Key, Value) ->
	case sc_store:lookup(Key) of
		{ok, Pid} ->
			sc_event:replace(Key, Value),
			sc_element:replace(Pid, Value);
		{error, _} ->
			sc_event:create(Key, Value),
			{ok, Pid} = sc_element:create(Value),
			sc_store:insert(Key, Pid)
	end.

%% @doc
%% lookup/1 will search for a given key in the simple_cache and return
%% it if the item is there.
%% @spec lookup(term) -> term() | {error, not_found}
lookup(Key) ->
	sc_event:lookup(Key),
	try
		{ok, Pid} = sc_store:lookup(Key),
		sc_element:fetch(Pid)
	catch
		_Class:_Exception ->
			 {error, not_found}
	end.

%% @doc
%% delete/1 will delete a {Key, Value} pair in the simple_cache by
%% indexing with the Key.
%% @spec delete(term()) -> ok
delete(Key) ->
	sc_event:delete(Key),
	case sc_store:lookup(Key) of
		{ok, Pid} ->
			sc_store:delete(Pid);
		{error, not_found} ->
			ok
	end.

%% @doc
%% drop/0 will drop the entire simple_cache store from memory and
%% start fresh.
%% @spec drop() -> ok
drop() ->
    Children = sc_element_sup:list_children(),
    [gen_server:cast(Child, delete) || {_, Child, worker, _List} <- Children],
    ok.
