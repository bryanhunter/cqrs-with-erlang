#Bank sample

This sample demonstrates one path of implementing the concepts of CQRS in Erlang. This sample was built with the primary purpose of being presented in about 10 minutes (code walk + demo) on stage on a 1024 x 768 projector. The goal was NOT to create a reference implementation. CQRS is an architectural pattern not a prescription. In short, this code shouldn't be used as the foundation for anything important. :)


## Seeing it work

```
cd ./bank
rebar compile
./run.cmd

Eshell V6.0  (abort with ^G)
1> bank:open().
ok
2> bank:create(wolfman).
ok
3> bank:deposit(wolfman, 100).
ok
4> bank:check_balance(wolfman).
100
5> bank:deposit(wolfman, 100).
ok
6> bank:deposit(wolfman, 100).
ok
7> bank:check_balance(wolfman).
300
8> bank:withdraw(wolfman, 400).
ok
9>
=INFO REPORT==== 4-Jun-2014::11:02:39 ===
Payment declined for Account: wolfman. Shame, shame!

9> bank:check_balance(wolfman).
300
10> bank:withdraw(wolfman, 250).
ok
11> bank:check_balance(wolfman).
50
12> bank:close().
ok

```
