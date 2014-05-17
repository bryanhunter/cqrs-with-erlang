
-module(bank_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
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
	Children = [
				?CHILD(bank_bus, worker),
				?CHILD(bank_account_summary_projection, worker),
				?CHILD(bank_account_detail_projection, worker)
			],
	RestartStrategy = {one_for_one, 10, 60},
    {ok, {RestartStrategy, Children}}.
