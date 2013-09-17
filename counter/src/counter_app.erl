-module(counter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	ensure_started(sasl),
	ensure_started(gproc),

	event_store:init(),

    case counter_sup:start_link() of
        {ok, Pid} ->
            counter_command_handler:add_handler(),
            counter_event_handler:add_handler(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.