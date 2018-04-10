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

## Using the Script to Consolidate Protocol Implementations

### Map Type Structs

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

### Tuple Type Structs

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

first([H | _]) -> H.

consy_rest([_ | T]) -> T.


printable@to_string(Arg@1) -> my_to_string(Arg@1).

consy@first(Arg@1) -> first(Arg@1).

consy@rest(Arg@1) -> consy_rest(Arg@1).
```

Content of MY\_OUTPUT:

```
tree  MY_OUTPUT
MY_OUTPUT
└── ep
    ├── consy
    │   └── ep_test_1.ep
    └── printable
        └── ep_test_1.ep

3 directories, 2 files
```


## Notes

If I embed the ast of the impl function in the proto module I don't have
access to private functions and I have to rewrite local calls to remote ones
for the ones that are public.

## TODO

* [ ] Remove {eof, Line} from module AST in parse transform when appending proto funs
* [ ] Validate that proto implementation implements right functions and arities
