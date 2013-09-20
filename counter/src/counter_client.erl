-module(counter_client).

-export([create_counter/1, bump_counter/1]).
-export([query_for_counter/1, query_for_counter_summary/0]).
-export([blast/2, create_and_blast/2]).

-define(SERVER, bus).

create_counter(Id) ->
	%% error_logger:info_msg("counter_client:create_counter(~p)~n", [Id]),
	bus:send_command({create_counter, Id}).
	
bump_counter(Id) ->
	%% error_logger:info_msg("counter_client:bump_counter(~p)~n", [Id]),
	bus:send_command({bump_counter, Id}).

blast(Counters, BumpTo) ->
	[spawn( ?MODULE, create_and_blast, [Id, BumpTo] ) ||
		Id <- lists:seq(1, Counters)],
	ok.

create_and_blast(CounterId, HowMany) ->
	create_counter(CounterId),
	[bump_counter(CounterId) || 
		_X <- lists:seq(1,HowMany)].

query_for_counter(Id) -> 
	read_store:get_counter_detail(Id).

query_for_counter_summary() -> 
	read_store:get_counter_summary().
