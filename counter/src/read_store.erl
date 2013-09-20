-module(read_store).
-include("counter_data_structures.hrl").

-export([init/0, get_counter_summary/0, set_counter_summary/1]).

%% Using ets for the read store is fine for this demo, but the data 
%% will be discarded when the creating process dies, and their is no
%% automatic garbage collection for ets tables.

init() ->
    ets:new(read_store_summary_views, [public, named_table]),
    set_counter_summary(#counter_summary{}),
    ok.

get_counter_summary() -> 
	[{counter_summary, Summary}] = 
		ets:lookup(read_store_summary_views, counter_summary),
	Summary.

set_counter_summary(NewData) -> 
	ets:insert(read_store_summary_views, {counter_summary, NewData}).