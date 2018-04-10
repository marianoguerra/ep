-module(ep_compiler_SUITE).
-compile(export_all).

all() -> [ep_test_1].

data_dir(Config) -> proplists:get_value(data_dir, Config).
priv_dir(Config) -> test_server:lookup_config(priv_dir, Config).

ep_test_1(Config) ->
    DataDir = data_dir(Config),
    ModAst = ep_compiler:compile(DataDir, "consy"),
    %ok = ModAst,
    ErlCode = ep_compiler:ast_to_erl(ModAst),
    ct:pal("~s~n", [ErlCode]),
    {ok, ModName, ModBin, _Warnings} = ep_compiler:compile_forms(ModAst), 
    ep_compiler:purge_module(ModName),
    {ok, Mod} = ep_compiler:load_module(ModName, ModBin),
    try 
        % it should call the implementation, but the module is not loaded..
        1 = Mod:first([1, 2])
    catch
        error:undef ->
            % .. so we check it fails trying to call the right implementation
            StackTrace = erlang:get_stacktrace(),
            [{ep_test, consy@first, [[1, 2]], []} | _] = StackTrace
    end,

    try 
        % it should call the implementation, but the module is not loaded..
        [2, 3] = Mod:rest([1, 2, 3])
    catch
        error:undef ->
            StackTrace1 = erlang:get_stacktrace(),
            % .. so we check it fails trying to call the right implementation
            [{ep_test, consy@rest, [[1, 2, 3]], []} | _] = StackTrace1
    end.

