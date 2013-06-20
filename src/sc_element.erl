%%%-------------------------------------------------------------------
%%% File    : sc_element.erl
%%% Author  :  <xeno@localhost>
%%% Description : 
%%%
%%% Created :  1 Jun 2013 by  <xeno@localhost>
%%%-------------------------------------------------------------------
-module(sc_element).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([
	 start_link/2,
	 create/2,
	 create/1,
	 fetch/1,
	 replace/2,
	 delete/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {value, lease_time, start_time}).

%%====================================================================
%% External functions
%%====================================================================
%% @doc
%% start_link/2 will start a new gen_server off which is holding the
%% specific value we're passed.
%% @spec start_link(Value::term(), LeaseTime::integer()) -> ok
start_link(Value, LeaseTime) ->
    gen_server:start_link(?MODULE, [Value, LeaseTime], []).

%% @doc
%% create/1 will create a new element in the cache.
%% @spec create(Value::term()) -> ok
create(Value) ->
    create(Value, ?DEFAULT_LEASE_TIME).

%% @doc
%% create/2 will create a new element in the cache with the required
%% timeout.
%% @spec create(Value::term(), Timeout::integer()) -> ok
create(Value, Timeout) ->
    sc_element_sup:start_child(Value, Timeout).

%% @doc
%% fetch/1 will return the value associated with the Pid.
%% @spec fetch(Pid::pid()) -> {ok, term()} | {error, Reason::term()}
fetch(Pid) ->
    gen_server:call(Pid, fetch).

%% @doc
%% delete/1 will remove an element from the cache.
%% @spec delete(Pid::pid()) -> ok
delete(Pid) ->
    gen_server:cast(Pid, delete).

%% @doc
%% replace/2 will entirely replace an element in the cache with a new
%% value.
%% @spec replace(Pid::pid(), Value::term()) -> ok | {error, Reason}
replace(Pid, Value) ->
    gen_server:cast(Pid, {replace, Value}).

%% @doc
%% init/1 initializes the new element handler to the associated value
%% with the specified timeout.

%% @spec init([Value::term(), LeaseTime::integer()]) -> pid()
init([Value, LeaseTime]) ->
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    {ok, #state{value = Value,
		lease_time = LeaseTime,
		start_time = StartTime},
     time_left(StartTime, LeaseTime)}.

handle_call(fetch, _From, State) ->
    #state{value = Value,
	   lease_time = LeaseTime,
	   start_time = StartTime} = State,
    TimeLeft = time_left(StartTime, LeaseTime),
    {reply, {ok, Value}, State, TimeLeft}.

handle_cast({replace, Value}, State) ->
    #state{lease_time = LeaseTime,
	   start_time = StartTime} = State,
    TimeLeft = time_left(StartTime, LeaseTime),
    {noreply, State#state{value = Value}, TimeLeft};

handle_cast(delete, State) ->
    {stop, normal, State}.

handle_info(timeout, State) ->
    {stop, normal, State}.


terminate(_Reason, _State) ->
    sc_store:delete(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

time_left(_StartTime, infinity) ->
    infinity;
time_left(StartTime, LeaseTime) ->
    Now = calendar:local_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
    TimeElapsed = CurrentTime - StartTime,
    case LeaseTime - TimeElapsed of
	Time when Time =< 0 ->
	    0;
	Time ->
	    Time * 1000
    end.
