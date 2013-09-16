-module(counter_client).

-export([create_counter/1, bump_counter/1]).
-export([query_for_counter/1, query_for_counter_summary/0]).

-define(SERVER, bus).

create_counter(Name) ->
	error_logger:info_msg("counter_client:create_counter(~p)~n", [Name]),
	gen_event:notify(?SERVER, {create_counter, Name}).

bump_counter(Name) ->
	error_logger:info_msg("counter_client:bump_counter(~p)~n", [Name]),
	gen_event:notify(?SERVER, {bump_counter, Name}).

query_for_counter(_Name) -> 
	ok.

query_for_counter_summary() -> 
	ok.
