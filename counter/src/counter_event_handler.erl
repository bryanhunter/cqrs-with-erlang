-module(counter_event_handler).

-behavior(gen_event).

-export([add_handler/0, delete_handler/0]).

-export([init/1, handle_event/2, handle_call/2, 
	handle_info/2, code_change/3, terminate/2]).

add_handler() ->
    bus:add_handler(?MODULE, []).

delete_handler() ->
    bus:delete_handler(?MODULE, []).

init([]) ->
	{ok, []}.

handle_event({counter_created, Name, DateCreated}, State) ->
	error_logger:info_msg("Handle {counter_created {~p, ~p}}~n", 
		[Name, DateCreated]),
	{ok, State};
handle_event({counter_bumped, Name, CounterValue, DateBumped}, State) ->
	error_logger:info_msg("Handle {counter_bumped {~p, ~p, ~p}}~n", 
		[Name, CounterValue, DateBumped]),
	{ok, State};
handle_event(_, State) ->
	{ok, State}.

handle_call(_, State) ->
	{ok, State}.

handle_info(_, State) -> 
	{ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
