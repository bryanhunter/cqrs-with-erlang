-module(counter_aggregate_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), 
	{I, {I, start_link, []}, temporary, brutal_kill, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Id) ->
    supervisor:start_child(?SERVER, [Id]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Aggregate = ?CHILD(counter_aggregate, worker),
    Children = [Aggregate],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.