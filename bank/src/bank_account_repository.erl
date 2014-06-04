-module(bank_account_repository).

-export([get_by_id/1,save/1, add_to_cache/1, remove_from_cache/1]).

add_to_cache(Id) ->
	keypid:save(Id, self()).

remove_from_cache(Id) ->
	keypid:delete(Id).

get_by_id(Id) ->
	case keypid:get(Id) of
		not_found -> load_from_event_store(Id);
		Pid -> {ok, Pid}
	end.

save(Pid) ->
	Saver = fun(Id, Events) ->
		bank_event_store:append_events(Id, Events) end,
	bank_account:process_unsaved_changes(Pid, Saver).

load_from_event_store(Id) ->
	case bank_event_store:get_events(Id) of
		[] ->
			not_found;
		Events ->
			Pid = bank_account:new(),
			bank_account:load_from_history(Pid, Events),
			{ok, Pid}
	end.
