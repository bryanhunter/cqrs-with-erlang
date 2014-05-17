-module(bank_account_repository).

-export([get_by_id/1,save/1]).

get_by_id(Id) ->
	case gproc:where({n,l, {bank_account, Id}}) of
		undefined -> load_from_event_store(Id);
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
