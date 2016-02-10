CQRS with Erlang
================

Presented at The Stange Loop 2013 in St. Louis, Missouri on October 20 from 9:50-10:30.

Abstract
========
Erlang an industry-proven functional programming language that simplifies writing reliable, concurrent, distributed systems.

CQRS is an architectural pattern that separates commands (which mutate state) from queries (which return values). With CQRS the “read” data store and the “write” data store can be on different severs, can use different storage engines, and can be scaled independently. CQRS is often linked with the Event Sourcing pattern which models state as a series of events (past tense verbs) rather than a single “latest” value. What works for an accountant’s ledger and for Git can work for our “write” store too. Given a series of events we can deal with concurrency and collisions more intelligently than “last guy wins”. We can also define varied service level agreements for commands and queries.

CQRS promotes distribution, concurrency and eventual consistency which is dandy until we attempt to code an implementation with conventional tools like C# or Java. Lucky for us Erlang is unconventional in all the right ways. Many of the ideas of CQRS dovetail perfectly with the sweet-spots of Erlang.

In this session we will dive into CQRS and explore a sample implementation written in Erlang. We will spotlight a few areas where CQRS shines. We will also show where CQRS in another language would be a comparative horror show.