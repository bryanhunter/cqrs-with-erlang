-module(counter_aggregate).

-export([new/0,create_counter/2, bump_counter/1]).
-export([get_unsaved_changes/1, mark_changes_saved/1, load_from_history/2]).

-record(state, {id, counter_value=0, date_created, date_bumped, changes=[]}).
-define(PROCESS_TIME_OUT, 600000).

%% API
new() ->
	State = #state{},
	spawn(fun() -> loop(State) end).

create_counter(Pid, Id) -> 
	Pid ! {attempt_command, {create_counter, Id}}.

bump_counter(Pid) ->
	Pid ! {attempt_command, bump_counter}.

get_unsaved_changes(Pid) -> 
	Pid ! {get_unsaved_changes, self()},
	receive
		{unsaved_changes, Changes} -> Changes
	after 1000 -> []
	end.

mark_changes_saved(Pid) ->
 	Pid ! mark_changes_saved.

load_from_history(Pid, Events) ->
 	Pid ! {load_from_history, Events}.

%% Internals

loop(State) ->
	error_logger:info_msg("Process ~p state:[~p]~n", [self(), State]),

	receive 
		{apply_event, Event} ->
			NewState = apply_event(Event, State),
			loop(NewState);	
		{attempt_command, Command} ->
			NewState = attempt_command(Command, State),
			loop(NewState);
		{get_unsaved_changes, From} ->
			From ! {unsaved_changes, lists:reverse(State#state.changes)},
			loop(State);
		mark_changes_saved ->
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
	% Maybe check if it already been created.
	apply_new_event({counter_created, Id, erlang:localtime()}, State);
attempt_command(bump_counter, State) ->
	NewCounterValue = State#state.counter_value + 1,
	apply_new_event({counter_bumped, NewCounterValue, erlang:localtime()}, State);
attempt_command(Command, State) ->
	error_logger:warn_msg("attempt_command for unexpected command (~p)~n", [Command]),
	State.

apply_new_event(Event, State) ->
	NewState = apply_event(Event, State),
	CombinedChanges = [Event] ++ NewState#state.changes,
	NewState#state{changes=CombinedChanges}.

apply_many_events([], State) ->
	State;
apply_many_events([Event|Rest], State) ->
	NewState = apply_event(Event, State),
	apply_many_events(Rest, NewState).

apply_event({counter_created, Id, DateCreated}, State) ->
	State#state{id=Id, date_created=DateCreated};
apply_event({counter_bumped, CounterValue, DateBumped}, State) ->
	State#state{counter_value=CounterValue, date_bumped=DateBumped};
apply_event(_Event, State)->
	State. % For some events, we don't have state to mutate

