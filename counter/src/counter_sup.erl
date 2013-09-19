-module(counter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Bus = ?CHILD(bus, worker),
	Repository = ?CHILD(counter_repository, worker),
	% AggregateSup = {counter_aggregate_sup, 
	% 	{counter_aggregate_sup, start_link, []}, 
	% 	permanent, 2000, supervisor, [counter_aggregate]},

	Children = [Bus, Repository],
	RestartStrategy = {one_for_one, 10, 60},
    {ok, {RestartStrategy, Children}}.

