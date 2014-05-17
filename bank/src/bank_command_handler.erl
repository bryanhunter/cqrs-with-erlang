-module(bank_command_handler).

-behavior(gen_event).

-export([add_handler/0, delete_handler/0]).
-export([init/1, handle_event/2, handle_call/2, 
	handle_info/2, code_change/3, terminate/2]).

-include("bank_data_structures.hrl").

add_handler() ->
    bank_bus:add_handler(?MODULE, []).

delete_handler() ->
    bank_bus:delete_handler(?MODULE, []).

init([]) ->
	{ok, []}.

handle_event(#create_bank_account{id=Id}, State) ->
	case bank_account_repository:get_by_id(Id) of
		not_found ->
			Pid = bank_account:new(),
			bank_account:create(Pid, Id),
			bank_account_repository:save(Pid),
			{ok, State};
		_ -> 	
			{ok, State}
	end;
handle_event(#deposit_money_into_bank_account{id=Id,amount=Amount}, State) ->
	case bank_account_repository:get_by_id(Id) of
		not_found ->
			{ok, State};
		{ok,Pid} ->
			bank_account:deposit_money(Pid, Amount),
			bank_account_repository:save(Pid),
			{ok, State}
	end;
handle_event(#withdraw_money_from_bank_account{id=Id,amount=Amount}, State) ->
		case bank_account_repository:get_by_id(Id) of
			not_found ->
				{ok, State};
			{ok,Pid} ->
				bank_account:withdraw_money(Pid, Amount),
				bank_account_repository:save(Pid),
				{ok, State}
		end;

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
