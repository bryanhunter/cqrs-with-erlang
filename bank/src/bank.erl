-module(bank).

%% The public client API for the bank application 
-export([open/0,close/0,
		create/1,deposit/2, withdraw/2, 
		check_balance/1,
		create_many/2, deposit_many/2, withdraw_many/2]).

-include("bank_data_structures.hrl").

open() ->
	application:start(bank).

close() ->
	application:stop(bank).

create(Account) -> 
	bank_bus:send_command(#create_bank_account{id=Account}).

deposit(Account, Amount) ->
	bank_bus:send_command(
		#deposit_money_into_bank_account{id=Account,amount=Amount}).

withdraw(Account, Amount) -> 
	bank_bus:send_command(
		#withdraw_money_from_bank_account{id=Account,amount=Amount}).

check_balance(Account) ->
	Details = bank_read_store:get_bank_account_details(),
	Dict = dict:from_list(Details),
	case dict:find(Account, Dict) of
		{ok, Value} -> Value;
		_ -> no_such_account
	end.

create_many(First, Last) ->
	[create(X) || X <- lists:seq(First, Last)].

deposit_many(First, Last) ->
	[many_deposit_helper(Account) 
		|| Account <- lists:seq(First, Last)].

many_deposit_helper(Account) ->
	[deposit(Account, 10) || _X <- lists:seq(1, 10)].
	
withdraw_many(First, Last) ->
	[many_withdraw_helper(Account) 
		|| Account <- lists:seq(First, Last)].

many_withdraw_helper(Account) ->
	[withdraw(Account, 5) || _X <- lists:seq(1, 10)].

