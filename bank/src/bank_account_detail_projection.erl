-module(bank_account_detail_projection).

-behaviour(gen_server).

-include("bank_data_structures.hrl").
-define(SERVER, ?MODULE).

%% API Function Exports
-export([start_link/0, process_event/1]).

%% gen_server Function Exports
-export([init/1, 
        handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

%% API Function Definitions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

process_event(Event) ->
    %% error_logger:info_msg("Projection process_event: ~p.~n", [Event]),
	gen_server:cast(?SERVER, Event).

%% gen_server Function Definitions
init([]) ->
	List = bank_read_store:get_bank_account_details(),
    Details = dict:from_list(List),
    {ok, Details}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(#bank_account_created{id=Id}, Details) ->
    NewDetails = dict:store(Id, 0, Details),
    update_read_store(NewDetails),
    {noreply,NewDetails};
handle_cast(#bank_account_money_withdrawn{id=Id, new_balance=Balance}, Details) ->
	NewDetails = dict:store(Id, Balance, Details),
    update_read_store(NewDetails),
    {noreply,NewDetails};
handle_cast(#bank_account_payment_declined{id=Id}, Details) ->
    error_logger:info_msg("Payment declined for Account: ~p. Shame, shame!~n", [Id]),
    {noreply,Details};
handle_cast(#bank_account_money_deposited{id=Id, new_balance=Balance}, Details) ->
    NewDetails = dict:store(Id, Balance, Details),
    update_read_store(NewDetails),
    {noreply,NewDetails};
handle_cast(_Msg, State) ->
    {noreply, State}.

update_read_store(Details) ->
    bank_read_store:set_bank_account_details(dict:to_list(Details)).
    
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
