%%%-------------------------------------------------------------------
%%% File    : sc_event.erl
%%% Author  :  <xeno@localhost>
%%% Description :
%%%
%%% Created :  2 Jun 2013 by  <xeno@localhost>
%%%-------------------------------------------------------------------
-module(sc_event).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0,
	 add_handler/2,
	 delete_handler/2,
	 lookup/1,
	 create/2,
	 replace/2,
	 delete/1]).


-define(SERVER, ?MODULE).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% Function: add_handler/0
%% Description: Adds an event handler
%%--------------------------------------------------------------------
add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

%%--------------------------------------------------------------------
%% Function: delete_handler/2
%% Description: Deletes an event handler
%%--------------------------------------------------------------------
delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

lookup(Key) ->
    gen_event:notify(?SERVER, {lookup, Key}).

create(Key, Value) ->
    gen_event:notify(?SERVER, {create, {Key, Value}}).

replace(Key, Value) ->
    gen_event:notify(?SERVER, {replace, {Key, Value}}).

delete(Key) ->
    gen_event:notify(?SERVER, {delete, Key}).
