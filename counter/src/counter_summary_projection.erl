-module(counter_summary_projection).

-behaviour(gen_server).

-include("counter_data_structures.hrl").
-define(SERVER, ?MODULE).

%% API Function Exports
-export([start_link/0, project_new_counter/1, project_counter_bump/1]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API Function Definitions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

project_new_counter(CounterId) ->
	gen_server:cast(?SERVER, {project_new_counter, CounterId}).

project_counter_bump(CounterValue) ->
	gen_server:cast(?SERVER, {project_counter_bump, CounterValue}).

%% gen_server Function Definitions
init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({project_new_counter, CounterId}, State) ->
	project_new_counter_to_counter_summary(CounterId),
	{noreply, State};
handle_cast({project_counter_bump, CounterValue}, State) ->
	project_counter_bump_to_counter_summary(CounterValue),
	{noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Function Definitions

project_new_counter_to_counter_summary(CounterId) ->
	Summary = read_store:get_counter_summary(),
	NewCount = Summary#counter_summary.count_of_counters + 1,
	SummaryMessage = lists:concat(["Last counter added was ", CounterId]),
	NewSummary = Summary#counter_summary
		{
			count_of_counters=NewCount,
			summary_message = SummaryMessage
		},
	read_store:set_counter_summary(NewSummary),
	ok.

project_counter_bump_to_counter_summary(CounterValue) ->
	Summary = read_store:get_counter_summary(),
	NewSum = Summary#counter_summary.sum_of_counter_values + 1,
	Lowest = min(CounterValue, Summary#counter_summary.lowest_value),
	Highest = max(CounterValue, Summary#counter_summary.highest_value),

	NewSummary = Summary#counter_summary
		{
			sum_of_counter_values=NewSum,
			lowest_value=Lowest,
			highest_value=Highest
		},
	read_store:set_counter_summary(NewSummary),
	ok.

