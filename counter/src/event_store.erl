-module(event_store).

%% Using ets for an event store is fine for this demo, but the data 
%% will be discarded when the creating process dies, and their is no
%% automatic garbage collection for ets tables.

-export([init/0,get_events/1,append_events/2, delete/1]).

-define(TABLE_ID, ?MODULE).

init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

append_events(Key, Events) ->
	StoredEvents = get_raw_events(Key),
	NewEvents = lists:reverse(Events),
    CombinedEvents = NewEvents ++ StoredEvents,
    ets:insert(?TABLE_ID, {Key, CombinedEvents}),
    lists:foreach(fun (Event) -> bus:publish_event(Event) end, NewEvents).

get_events(Key) ->
	lists:reverse(get_raw_events(Key)).

delete(Key) ->
    ets:delete(?TABLE_ID, Key).

get_raw_events(Key) ->
    case ets:lookup(?TABLE_ID, Key) of
        [{Key, Events}] -> Events;
        [] -> []
    end.
