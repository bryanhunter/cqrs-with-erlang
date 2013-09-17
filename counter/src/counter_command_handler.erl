-module(counter_command_handler).

-behavior(gen_event).

-export([add_handler/0, delete_handler/0]).

-export([init/1, handle_event/2, handle_call/2, 
	handle_info/2, code_change/3, terminate/2]).

add_handler() ->
    bus:add_handler(?MODULE, []).

delete_handler() ->
    bus:delete_handler(?MODULE, []).

init([]) ->
	{ok, []}.

handle_event({create_counter, Id}, State) ->
	error_logger:info_msg("Handle {create_counter {~p}}~n", [Id]),
	{ok, Pid} = counter_aggregate:create_counter(Id),
	counter_repository:save(Pid),
	{ok, State};
handle_event({bump_counter, Id}, State) ->
	error_logger:info_msg("Handle {bump_counter {~p}}~n", [Id]),
	%% {ok, Pid} = counter_repository:get_counter_aggregate(Id),
	{ok, State};
handle_event(_, State) ->
	{ok, State}.

handle_call(_, State) ->
	{ok, State}.

handle_info(_, State) -> 
	{ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

