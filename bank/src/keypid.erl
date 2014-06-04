-module(keypid).

-export([init/0, save/2, get/1, delete/1]).
-define(TABLE_ID, ?MODULE).

init() ->
	ets:new(?TABLE_ID, [public, named_table]),
	ok.

delete(Key) ->
	ets:delete(?TABLE_ID, Key).

save(Key, Pid) ->
	save_helper(Key, Pid, is_pid(Pid)).

save_helper(Key, Pid, true) ->
	ets:insert(?TABLE_ID, {Key, Pid});
save_helper(_, _, _) ->
	false.

get(Key) ->
	case ets:lookup(?TABLE_ID, Key) of
		[{Key, Pid}] ->
			case is_pid(Pid) andalso is_process_alive(Pid) of
				true ->
					Pid;
				false ->
					not_found
			end;
		[] -> not_found
	end.
