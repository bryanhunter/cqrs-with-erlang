Counter sample
==============



Understanding gen_event wireup
------------------------------
_app:start()
	_sup:start_link()
		supervisor:start_link({local, _sup}, _sup, []).
			_sup:init([])
				start_link(event_manager)
					_event_manager:start_link()
						gen_event:start_link({local, _event_manager).
							event manager process created 
							event manager process added to the supervisor tree
							event manager registered locally _event_manager
							{ok, PidOfEventManager}
	guard : {ok, Pid} ->
		_event_handler:add_handler()
			_event_manager:event_handler(_event_handler, [])
			 	gen_event:add_handler(_event_manager, _event_handler, Handler, Args).
			 		_event_handler:init()


_app
- is an application

_sup
- is a supervisor

_event_manager
- doesn't have a behavior
- is a process 

_event_handler
- is gen_event

