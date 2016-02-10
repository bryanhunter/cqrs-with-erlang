-module(counter_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API functions
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks
init([]) ->
	Bus = ?CHILD(bus, worker),
	Projection = ?CHILD(counter_summary_projection, worker),
	Children = [Bus,Projection],
	RestartStrategy = {one_for_one, 10, 60},
    {ok, {RestartStrategy, Children}}.

