ep
==

Erlang Protocols

Build
-----

```
make
```

will compile and generate the ep escript, run it to see usage

Test
----

```
make test
```

Using the Script
----------------

Map Type Structs
................

```
./ep compile-proto map erl test/ep_compiler_SUITE_data/ consy
```

Outputs:

```erlang
-module(consy).

-export([first/1, rest/1]).

first(L = [H | _]) when is_list(L) ->
    ep_test:consy@first(L);
first(V = #{'__struct__' := Type}) ->
    Type:consy@first(V).

rest(Arg@0 = [_ | T]) -> ep_test:consy@rest(Arg@0);
rest(V = #{'__struct__' := Type}) -> Type:consy@rest(V).
```

Map Type Structs
................

```
./ep compile-proto tuple erl test/ep_compiler_SUITE_data/ consy
```

```erlang
-module(consy).

-export([first/1, rest/1]).

first(L = [H | _]) when is_list(L) ->
    ep_test:consy@first(L);
first(V = {{Type, _Data, _}}) -> Type:consy@first(V).

rest(Arg@0 = [_ | T]) -> ep_test:consy@rest(Arg@0);
rest(V = {{Type, _Data, _}}) -> Type:consy@rest(V).
```


Notes
-----

If I embed the ast of the impl function in the proto module I don't have
access to private functions and I have to rewrite local calls to remote ones
for the ones that are public.

