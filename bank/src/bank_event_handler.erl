-module(bank_event_handler).

-behavior(gen_event).

-include("bank_data_structures.hrl").

-export([add_handler/0, delete_handler/0]).
-export([init/1, handle_event/2, handle_call/2, 
	handle_info/2, code_change/3, terminate/2]).

add_handler() ->
    bank_bus:add_handler(?MODULE, []).

delete_handler() ->
    bank_bus:delete_handler(?MODULE, []).

init([]) ->
	{ok, []}.

handle_event(#bank_account_created{id=Id}=Event, State) ->
	bank_account_summary_projection:project_new_bank_account(Id),
	bank_account_detail_projection:process_event(Event),
	{ok, State};
handle_event(#bank_account_money_deposited{}=Event, State) ->
	bank_account_detail_projection:process_event(Event),
	{ok, State};
handle_event(#bank_account_money_withdrawn{}=Event, State) ->
	bank_account_detail_projection:process_event(Event),
	{ok, State};
handle_event(#bank_account_payment_declined{}=Event, State) ->
	bank_account_detail_projection:process_event(Event),
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

%% private

