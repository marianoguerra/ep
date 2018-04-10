-module(ep_compiler_SUITE).
-compile(export_all).

all() -> [ep_test_1_map, ep_test_1_tuple].

data_dir(Config) -> proplists:get_value(data_dir, Config).
priv_dir(Config) -> test_server:lookup_config(priv_dir, Config).

ep_test_1_map(Config) ->
    ep_test_1(Config, map, #{'__struct__' => my_mod}).

ep_test_1_tuple(Config) ->
    ep_test_1(Config, tuple, {{my_mod, my_data, my_meta}}).

ep_test_1(Config, TVType, Struct) ->
    DataDir = data_dir(Config),
    ModAst = ep_compiler:compile(DataDir, "consy", #{tv_type => TVType}),
    %ok = ModAst,
    ErlCode = ep_compiler:ast_to_erl(ModAst),
    ct:pal("~s~n", [ErlCode]),
    {ok, ModName, ModBin, _Warnings} = ep_compiler:compile_forms(ModAst), 
    ep_compiler:purge_module(ModName),
    {ok, Mod} = ep_compiler:load_module(ModName, ModBin),
    call_undef(Mod, first, [[1, 2]], ep_test, consy@first, 1),
    call_undef(Mod, rest, [[1, 2, 3]], ep_test, consy@rest, [2, 3]),
    call_undef(Mod, first, [Struct], my_moda, consy@first, [Struct]).

call_undef(Mod, Fun, Args, ModCall, FunCall, Expected) ->
    try 
        % it should call the implementation, but the module is not loaded..
        Expected = apply(Mod, Fun, Args)
    catch
        error:undef ->
            % .. so we check it fails trying to call the right implementation
            StackTrace = erlang:get_stacktrace(),
            [{ModCall, FunCall, Args, []} | _] = StackTrace
    end.
