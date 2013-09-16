-module(counter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	event_store:init(),

    case counter_sup:start_link() of
        {ok, _Pid} ->
            counter_command_handler:add_handler(),
            counter_event_handler:add_handler();
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.