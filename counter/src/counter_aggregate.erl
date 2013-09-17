-module(counter_aggregate).

-behaviour(gen_server).

-export([start_link/1, create_counter/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {id, counter_value=0, date_created, date_bumped, 
	changes=[]}).

-define(SERVER, ?MODULE).

%% API
start_link([Id]) ->
	gen_server:start_link({via, gproc, {n,g,{?MODULE,Id}}}, ?MODULE, []).

create_counter(Id) ->
    counter_aggregate_sup:start_child(Id).

%% gen_server callbacks
init([Id]) ->
	{ok, #state{id=Id}}.

handle_call({counter_created, Id, DateCreated},  _From, State) -> 
	error_logger:info_msg("handle_call state {~p}}~n", [State]),
	bus:publish_event({counter_created, Id, DateCreated}),
	{reply, ok, State#state{id=Id, date_created=DateCreated}};
handle_call({counter_bumped, Id, CounterValue,  DateBumped},  _From, State) -> 
	error_logger:info_msg("handle_call state {~p}}~n", [State]),
	bus:publish_event({counter_bumped, Id, CounterValue, DateBumped}),
	{reply, ok, State#state{id=Id, counter_value=CounterValue, 
				date_bumped=DateBumped}}.

handle_cast(delete, State) ->
    {stop, normal, State}.

handle_info(timeout, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal


%% -----------------------------------------------------------------------------

% public abstract class AggregateRoot
% {
%     private readonly List<Event> _changes = new List<Event>();
   
%     public abstract Guid Id { get; }
%     public int Version { get; internal set; }

%     public IEnumerable<Event> GetUncommittedChanges()
%     {
%         return _changes;
%     }

%     public void MarkChangesAsCommitted()
%     {
%         _changes.Clear();
%     }

%     public void LoadsFromHistory(IEnumerable<Event> history)
%     {
%         foreach (var e in history) ApplyChange(e, false);
%     }

%     protected void ApplyChange(Event @event)
%     {
%         ApplyChange(@event, true);
%     }

%     private void ApplyChange(Event @event, bool isNew)
%     {
%         this.AsDynamic().Apply(@event);
%         if(isNew) _changes.Add(@event);
%     }
% }
