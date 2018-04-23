# ep

Erlang Protocols

## Build

```
make
```

Will compile and generate the ep escript, run it to see usage

## Test

```
make test
```

## Using the Script to Test the Parse Transform

The following will run the ep parse transform (ep_pt) on 
`test/ep_pt_SUITE_data/ep_test_1.erl` and output protocol metadata to MY_OUTPUT


```
./ep pt erl MY_OUTPUT test/ep_pt_SUITE_data/ep_test_1.erl
```

Output:

```erlang
-file("test/ep_pt_SUITE_data/ep_test_1.erl", 1).

-module(ep_test_1).

-export([printable@to_string/1, consy@first/1,
         consy@rest/1]).

-ep({printable, #{to_string => {my_to_string, 1}}}).

-ep({consy,
     #{first => {first, 1}, rest => {consy_rest, 1}}}).

my_to_string(V) -> io_lib:format("~p", [V]).

first(L = [H | _]) when is_list(L) -> H.

consy_rest([_ | T]) -> T.


printable@to_string(Arg@1) -> my_to_string(Arg@1).

consy@first(Arg@1) -> first(Arg@1).

consy@rest(Arg@1) -> consy_rest(Arg@1).
```

```
./ep pt erl MY_OUTPUT test/ep_pt_SUITE_data/ep_test_2.erl
```

Output:

```erlang
-file("test/ep_pt_SUITE_data/ep_test_2.erl", 1).

-module(ep_test_2).

-export([]).

-def_ep({printable, #{to_string => {my_to_string, 1}}}).

-def_ep({consy, #{first => 1, rest => 1}}).

my_to_string(V) when is_integer(V) ->
    integer_to_binary(V);
my_to_string(V) when is_float(V) -> float_to_binary(V);
my_to_string(V) when is_atom(V) ->
    atom_to_binary(V, utf8).
```

Content of MY\_OUTPUT:

```
MY_OUTPUT
└── ep
    ├── consy
    │   ├── ep_test_1.ep
    │   └── ep_test_2.epd
    └── printable
        ├── ep_test_1.ep
        └── ep_test_2.epd

3 directories, 4 files
```

## Using the Script to Consolidate Protocol Implementations

### Map Type Structs

#### Printable (with default implementations)

```
./ep compile-proto map erl MY_OUTPUT/ep/ printable
```

Outputs:

```erlang
-module(printable).

-export([to_string/1]).

to_string(V) when is_integer(V) -> integer_to_binary(V);
to_string(V) when is_float(V) -> float_to_binary(V);
to_string(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_string(Arg@0 = V) ->
    ep_test_1:printable@to_string(Arg@0);
to_string(V = #{'__struct__' := Type}) ->
    Type:printable@to_string(V).
```

#### Consy (without default implementations)

```
./ep compile-proto map erl MY_OUTPUT/ep/ consy
```

Outputs:

```erlang
-module(consy).

-export([first/1, rest/1]).

first(L = [H | _]) when is_list(L) ->
    ep_test_1:consy@first(L);
first(V = #{'__struct__' := Type}) ->
    Type:consy@first(V).

rest(Arg@0 = [_ | T]) -> ep_test_1:consy@rest(Arg@0);
rest(V = #{'__struct__' := Type}) -> Type:consy@rest(V).
```

### Tuple Type Structs

#### Printable (with default implementations)

```
./ep compile-proto tuple erl MY_OUTPUT/ep/ printable
```

Outputs:

```erlang
-module(printable).

-export([to_string/1]).

to_string(V) when is_integer(V) -> integer_to_binary(V);
to_string(V) when is_float(V) -> float_to_binary(V);
to_string(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_string(Arg@0 = V) ->
    ep_test_1:printable@to_string(Arg@0);
to_string(V = {{Type, _Data, _}}) ->
    Type:printable@to_string(V).
```

#### Consy (without default implementations)

```
./ep compile-proto tuple erl MY_OUTPUT/ep/ consy
```

```erlang
-module(consy).

-export([first/1, rest/1]).

first(L = [H | _]) when is_list(L) ->
    ep_test_1:consy@first(L);
first(V = {{Type, _Data, _}}) -> Type:consy@first(V).

rest(Arg@0 = [_ | T]) -> ep_test_1:consy@rest(Arg@0);
rest(V = {{Type, _Data, _}}) -> Type:consy@rest(V).
```


## Notes

If I embed the ast of the impl function in the proto module I don't have
access to private functions and I have to rewrite local calls to remote ones
for the ones that are public.

## TODO

* [ ] Remove {eof, Line} from module AST in parse transform when appending proto funs
* [ ] Validate that proto implementation implements right functions and arities

* [ ] Remove .ep and .epd files in output if that module no longer declares or implements protos
