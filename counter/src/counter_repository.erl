-module(counter_repository).

-export([get_by_id/1,save/1]).

get_by_id(Id) ->
	case gproc:where({n,l, {counter_aggregate, Id}}) of
		undefined -> load_from_event_store(Id);
		Pid -> {ok, Pid}
	end.

save(Pid) ->
	Saver = fun(Id, Events) -> event_store:append_events(Id, Events) end,
	counter_aggregate:process_unsaved_changes(Pid, Saver).

load_from_event_store(Id) ->
	case event_store:get_events(Id) of 
		[] -> 
			not_found;
		Events -> 
			Pid = counter_aggregate:new(),
			counter_aggregate:load_from_history(Pid, Events),
			{ok, Pid}
	end.
