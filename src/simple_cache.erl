-module(simple_cache).

-export([
		 insert/2,
		 delete/1,
		 lookup/1
		]).

insert(Key, Value) ->
	case sc_store:lookup(Key) of
		{ok, Pid} ->
			sc_element:replace(Pid, Value);
		{error, _} ->
			{ok, Pid} = sc_element:create(Value),
			sc_store:insert(Key, Pid)
	end.

lookup(Key) ->
	try
		{ok, Pid} = sc_store:lookup(Key),
	    sc_element:fetch(Pid)
	catch
		_Class:_Exception ->
			 {error, not_found}
	end.

delete(Key) ->
	case sc_store:lookup(Key) of
		{ok, Pid} ->
			sc_store:delete(Pid);
		{error, not_found} ->
			ok
	end.
	
