-module(bank_account_summary_projection).

-behaviour(gen_server).

-include("bank_data_structures.hrl").
-define(SERVER, ?MODULE).

%% API Function Exports
-export([start_link/0, project_new_bank_account/1]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API Function Definitions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

project_new_bank_account(Id) ->
	gen_server:cast(?SERVER, {project_new_bank_account, Id}).

%% gen_server Function Definitions
init([]) ->
	State = bank_read_store:get_bank_account_summary(),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({project_new_bank_account, _Id}, State) ->
	NewCount = State#bank_account_summary.count_of_accounts + 1,
	NewSummary = State#bank_account_summary{count_of_accounts=NewCount},
	bank_read_store:set_bank_account_summary(NewSummary),
	{noreply, NewSummary};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
