-module(bank_read_store).

-include("bank_data_structures.hrl").

-export([init/0, 
	get_bank_account_summary/0, set_bank_account_summary/1,
	get_bank_account_details/0, set_bank_account_details/1]).

%% Using ets for the read store is fine for this demo, but the data 
%% will be discarded when the creating process dies, and there is no
%% automatic garbage collection for ets tables.

init() ->
    ets:new(read_store_summary_views, [public, named_table]),
    set_bank_account_summary(#bank_account_summary{}),
    set_bank_account_details([]), 
    ok.

get_bank_account_summary() -> 
	[{bank_account_summary, Summary}] = 
		ets:lookup(read_store_summary_views, bank_account_summary),
	Summary.

set_bank_account_summary(NewData) -> 
	ets:insert(read_store_summary_views, {bank_account_summary, NewData}).

get_bank_account_details() ->
	[{bank_account_details, Details}] = 
		ets:lookup(read_store_summary_views, bank_account_details),
	Details.

set_bank_account_details(NewData) -> 
	ets:insert(read_store_summary_views, {bank_account_details, NewData}).