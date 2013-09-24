-module(counter_aggregate).

%% Counter our very simple aggregate. 

-export([new/0,create_counter/2, bump_counter/1]).
-export([process_unsaved_changes/2, load_from_history/2]).

-record(state, {id, counter_value=0, date_created, date_bumped, changes=[]}).
-define(PROCESS_TIME_OUT, 45000).

%% API

new() ->
	spawn(fun() -> init() end).

create_counter(Pid, Id) -> 
	Pid ! {attempt_command, {create_counter, Id}}.

bump_counter(Pid) ->
	Pid ! {attempt_command, bump_counter}.

process_unsaved_changes(Pid, Saver) ->
	Pid ! {process_unsaved_changes, Saver}.

load_from_history(Pid, Events) ->
 	Pid ! {load_from_history, Events}.

%% Internals

init() -> 
	State = #state{},
	loop(State).

loop(State) ->
	%% error_logger:info_msg("Process ~p state:[~p]~n", [self(), State]),
	receive 
		{apply_event, Event} ->
			NewState = apply_event(Event, State),
			loop(NewState);	
		{attempt_command, Command} ->
			NewState = attempt_command(Command, State),
			loop(NewState);
		{process_unsaved_changes, Saver} ->
			Id = State#state.id,
			Saver(Id, lists:reverse(State#state.changes)),
			NewState = State#state{changes=[]},
			loop(NewState);
		{load_from_history, Events} ->
 			NewState = apply_many_events(Events, #state{}),
 			loop(NewState);
 		Unknown -> 
 			error_logger:warning_msg("Received unknown message (~p)~n", [Unknown]),
 			loop(State)
		after ?PROCESS_TIME_OUT ->
			shutting_down
	end.

attempt_command({create_counter, Id}, State) ->
	%% TODO: check if it's already been created 
	apply_new_event({counter_created, Id, erlang:localtime()}, State);
attempt_command(bump_counter, State) ->
	NewCounterValue = State#state.counter_value + 1,
	Id = State#state.id,
	apply_new_event({counter_bumped, Id, NewCounterValue, erlang:localtime()}, State);
attempt_command(Command, State) ->
	error_logger:warn_msg("attempt_command for unexpected command (~p)~n", [Command]),
	State.

apply_new_event(Event, State) ->
	NewState = apply_event(Event, State),
	CombinedChanges = [Event] ++ NewState#state.changes,
	NewState#state{changes=CombinedChanges}.

apply_event({counter_created, Id, DateCreated}, State) ->
	%% TODO: refactor the gproc:reg call out to the repository
	gproc:reg({n, l, {counter_aggregate, Id}}),
	State#state{id=Id, date_created=DateCreated};
apply_event({counter_bumped, _Id, CounterValue, DateBumped}, State) ->
	State#state{counter_value=CounterValue, date_bumped=DateBumped};
apply_event(_Event, State)->
	State. % For some events, we don't have state to mutate

apply_many_events([], State) ->
	State;
apply_many_events([Event|Rest], State) ->
	NewState = apply_event(Event, State),
	apply_many_events(Rest, NewState).
	