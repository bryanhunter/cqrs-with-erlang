%% Projections

-record(bank_account_summary, 
		{
			count_of_accounts=0
		}).


%% Commands
-record(create_bank_account,
		{
			id
		}).

-record(deposit_money_into_bank_account,
		{
			id,
			amount=0
		}).

-record(withdraw_money_from_bank_account,
		{
			id,
			amount=0
		}).

%% Events
-record(bank_account_created,
		{
			id,
			date_created
		}).

-record(bank_account_money_deposited,
		{
			id,
			amount=0,
			new_balance=0,
			transaction_date
		}).

-record(bank_account_money_withdrawn,
		{
			id,
			amount=0,
			new_balance=0,
			transaction_date
		}).

-record(bank_account_payment_declined,
		{
			id,
			amount=0,
			transaction_date
		}).
