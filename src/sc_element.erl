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
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Value, LeaseTime) ->
	gen_server:start_link(?MODULE, [Value, LeaseTime], []).

create(Value) ->
	create(Value, ?DEFAULT_LEASE_TIME).

create(Value, Timeout) ->
	sc_sup:start_child(Value, Timeout).

fetch(Pid) ->
	gen_server:call(Pid, fetch).

delete(Pid) ->
	gen_server:cast(Pid, delete).

replace(Pid, Value) ->
	gen_server:cast(Pid, {replace, Value}).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Value, LeaseTime]) ->
	Now = calendar:local_time(),
	StartTime = calendar:datetime_to_gregorian_seconds(Now),
	{ok, #state{value = Value,
			   lease_time = LeaseTime,
			   start_time = StartTime},
			   time_left(StartTime, LeaseTime)}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(fetch, _From, State) ->
	#state{value = Value,
		   lease_time = LeaseTime,
		   start_time = StartTime} = State,
	TimeLeft = time_left(StartTime, LeaseTime),
	{reply, {ok, Value}, State, TimeLeft}.
%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({replace, Value}, State) ->
	#state{lease_time = LeaseTime,
		   start_time = StartTime} = State,
	TimeLeft = time_left(StartTime, LeaseTime),
	{noreply, State#state{value = Value}, TimeLeft};
handle_cast(delete, State) ->
	{stop, normal, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
	{stop, normal, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	sc_store:delete(self()),
	ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

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
