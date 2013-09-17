-module(counter_client).

-export([create_counter/1, bump_counter/1]).
-export([query_for_counter/1, query_for_counter_summary/0]).

-define(SERVER, bus).

create_counter(Id) ->
	error_logger:info_msg("counter_client:create_counter(~p)~n", [Id]),
	bus:send_command({create_counter, Id}).
	
bump_counter(Id) ->
	error_logger:info_msg("counter_client:bump_counter(~p)~n", [Id]),
	bus:send_command({bump_counter, Id}).

query_for_counter(_Id) -> 
	ok.

query_for_counter_summary() -> 
	ok.
