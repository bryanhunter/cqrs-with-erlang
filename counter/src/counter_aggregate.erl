-module(counter_aggregate).

-export([load_from_event_stream/1]).

-record(state, {name, counter_value=0, date_created, date_bumped, 
	changes=[]}).

load_from_event_stream(Events) ->
	Pid = spawn(fun () -> loop([]) end),
	apply_event(Events, Pid).

apply_event([], Pid) ->
	Pid;
apply_event([Event|Rest], Pid) ->
	Pid ! Event,
	apply_event(Rest, Pid).

loop(State) ->
	error_logger:info_msg("counter_aggregate state {~p}}~n", [State]),
	receive 
		{counter_created, Name, DateCreated} ->
			bus:publish_event({counter_created, Name, DateCreated}),
			loop(State#state{name=Name, date_created=DateCreated});
		{counter_bumped, Name, CounterValue,  DateBumped} ->
			bus:publish_event({counter_bumped, Name, CounterValue, DateBumped}),
			loop(State#state{name=Name, counter_value=CounterValue, 
				date_bumped=DateBumped});
		_ ->
			loop(State)
	end.	


