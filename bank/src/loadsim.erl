-module(loadsim).

-export([create_many/2, deposit_many/2, withdraw_many/2]).

create_many(First, Last) ->
	[bank:create(X) || X <- lists:seq(First, Last)],
	ok.

deposit_many(First, Last) ->
	[many_deposit_helper(Account)
		|| Account <- lists:seq(First, Last)],
	ok.

many_deposit_helper(Account) ->
	[bank:deposit(Account, 10) || _X <- lists:seq(1, 10)].

withdraw_many(First, Last) ->
	[many_withdraw_helper(Account)
		|| Account <- lists:seq(First, Last)],
	ok.

many_withdraw_helper(Account) ->
	[bank:withdraw(Account, 5) || _X <- lists:seq(1, 10)].
