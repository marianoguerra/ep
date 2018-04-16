-module(ep_escript).
-export([main/1]).

main(["compile-proto", TVType, Target, BaseDir, ProtoName])
  when (TVType =:= "map" orelse TVType =:= "tuple") andalso
       (Target =:= "erl" orelse Target =:= "ast") ->
    TVTypeAtom = list_to_atom(TVType),

    ModAst = ep_compiler:compile(BaseDir, ProtoName, #{tv_type => TVTypeAtom}),
	ast_to_target(ModAst, Target),
    erlang:halt(0);

main(["pt", Target, OutputDir, ModulePath])
  when (Target =:= "erl" orelse Target =:= "ast") ->

	{ok, Forms} = epp:parse_file(ModulePath, []),
	ModAst = ep_pt:parse_transform(Forms, [{ep_opts, #{output_path => OutputDir}}]),
	ast_to_target(ModAst, Target),
    erlang:halt(0);

main(Args) ->
    io:format("Unknown Arguments: ~p~n", [Args]),
    usage(),
    erlang:halt(0).

ast_to_target(ModAst, Target) ->
    case Target of
        "erl" ->
            ErlCode = ep_compiler:ast_to_erl(ModAst),
            io:format("~s~n", [ErlCode]);
        "ast" ->
            io:format("~p~n", [ModAst])
    end.

usage() ->
    io:format("Usage:~n"),
    io:format("  ep compile-proto map|tuple erl|ast <base-dir> <proto-name>~n"),
    io:format("    % compile proto definitions at <base-dir>/<proto-name>~n"),
    io:format("    % generate pattern for map|tuple struct~n"),
    io:format("    % compile to erl|ast~n"),
    io:format("~n"),
    io:format("  ep pt erl|ast <output-dir> <path/to/module.erl>~n"),
    io:format("    % do the same the parse transform would do"),
    io:format("    % parse module, modify it to expose proto functions"),
    io:format("    % write proto metadata in <output-dir> and print"),
    io:format("    % updated module in erl or ast format").
