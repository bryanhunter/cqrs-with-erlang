-module(counter_repository).

% public interface IRepository<T> where T : AggregateRoot, new()
% {
%     void Save(AggregateRoot aggregate, int expectedVersion);
%     T GetById(Guid id);
% }

-behaviour(gen_server).

-export([start_link/0,get_by_id/1,save/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_by_id(Id) -> 
	gen_server:call(?SERVER, {get, Id}).

save(Pid) ->
	gen_server:call(?SERVER, {save, Pid}).	

%% Calbacks
init(State) ->
	 {ok, State}.

handle_call({get, Id}, _From, State) ->
	 Events = event_store:get_events(Id),
	 Pid = counter_aggregate:load_from_event_stream(Events),
	 {reply, {ok, Pid}, State};
handle_call({save, Id}, _From, State) ->
	 Events = counter_aggregate:get_uncommited_events(Id),
	 event_store:append_events(Events),
	 {reply, ok, State};
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