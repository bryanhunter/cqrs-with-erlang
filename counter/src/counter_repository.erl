-module(counter_repository).

-behaviour(gen_server).

-export([start_link/0,get/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get(Id) -> 
	gen_server:call(?SERVER, {get, Id}).


%% Calbacks
init(State) ->
	 {ok, State}.

handle_call({get, Id}, _From, State) ->
	 Events = event_store:get_events(Id),
	 Pid = counter_aggregate:load_from_event_stream(Events),
	 {reply, {ok, Pid}, State};
handle_call(get_count, _From, State) ->
    {reply, ok, State}.

handle_cast(delete, State) ->
    {stop, normal, State}.

handle_info(timeout, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.