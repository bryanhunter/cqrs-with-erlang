-module(bank_account).

-export([new/0, create/2, deposit_money/2, withdraw_money/2]).
-export([process_unsaved_changes/2, load_from_history/2]).

-record(state, {id, date_created, balance=0, changes=[]}).
-define(PROCESS_TIME_OUT, 45000).

-include("bank_data_structures.hrl").

%% API

new() ->
	spawn(fun() -> init() end).

create(Pid, Id) -> 
	Pid ! {attempt_command, {create, Id}}.

deposit_money(Pid, Amount) ->
	Pid ! {attempt_command, {deposit_money, Amount}}.

withdraw_money(Pid, Amount) ->
	Pid ! {attempt_command, {withdraw_money, Amount}}.

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

attempt_command({create, Id}, State) ->
	%% TODO: check if it's already been created 
	Event = #bank_account_created{id=Id, date_created=erlang:localtime()},
	apply_new_event( Event, State);

attempt_command({deposit_money, Amount}, State) ->
	NewBalance = State#state.balance + Amount,
	Id = State#state.id,
	Event = #bank_account_money_deposited{id=Id, amount=Amount, 
				new_balance=NewBalance,transaction_date=erlang:localtime()},
	apply_new_event(Event, State);

attempt_command({withdraw_money, Amount}, State) ->
	NewBalance = State#state.balance - Amount,
	Id = State#state.id,

	Event = case(NewBalance < 0) of
		false -> 
			#bank_account_money_withdrawn{id=Id, amount=Amount,
						new_balance=NewBalance,transaction_date=erlang:localtime()};
		true -> 
			#bank_account_payment_declined{id=Id,amount=Amount,
						transaction_date=erlang:localtime()}
		end,
	apply_new_event(Event, State);

attempt_command(Command, State) ->
	error_logger:warn_msg("attempt_command for unexpected command (~p)~n", [Command]),
	State.

apply_new_event(Event, State) ->
	NewState = apply_event(Event, State),
	CombinedChanges = [Event] ++ NewState#state.changes,
	NewState#state{changes=CombinedChanges}.

apply_event(#bank_account_created{id=Id,date_created=DateCreated}, State) ->
	case gproc:where({n,l, {bank_account, Id}}) of
		%% TODO: refactor the gproc:reg call out to the repository
		undefined -> 
			gproc:reg({n, l, {bank_account, Id}}),
			gproc:await({n,l, {bank_account, Id}});
		_ -> ok
	end,
	State#state{id=Id, date_created=DateCreated};
apply_event(#bank_account_money_deposited{amount=Amount}, #state{balance=Balance}=State) ->
	State#state{balance = Balance + Amount};

apply_event(#bank_account_money_withdrawn{amount=Amount}, #state{balance=Balance}=State) ->
	State#state{balance = Balance - Amount};

apply_event(_Event, State)->
	State. % For some events, we don't have state to mutate

apply_many_events([], State) ->
	State;
apply_many_events([Event|Rest], State) ->
	NewState = apply_event(Event, State),
	apply_many_events(Rest, NewState).
