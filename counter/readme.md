Counter sample
==============

This sample demonstrates one path of implementing the concepts of CQRS in Erlang. 
This sample was built with the primary purpose of being presented in about 10 minutes 
(code walk + demo) on stage on a 1024 x 768 projector. The goal was *NOT* to create 
a reference implementation. CQRS is an architectural pattern not a prescription. In short, 
this code shouldn't be used as the foundation for anything important. :)


Seeing it work
--------------

````
cd ./counter
rebar get-deps
rebar compile
run

application:start(counter).
counter_client:create_counter(wolfman).
counter_client:bump_counter(wolfman).
counter_client:bump_counter(wolfman).
counter_client:bump_counter(wolfman).
observer:start().
counter_client:create_counter(dracula).
counter_client:bump_counter(dracula).
counter_client:bump_counter(dracula).
counter_client:bump_counter(dracula).
counter_client:bump_counter(dracula).
counter_client:query_for_counter_summary().
counter_client:blast(1000,100).

````


Understanding gen_event wireup
------------------------------
```
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
			 		

```

