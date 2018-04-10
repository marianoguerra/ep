-module(ep).
-export([main/1]).

%% escript Entry point
main(["compile-proto", TVType, Target, BaseDir, ProtoName])
  when (TVType =:= "map" orelse TVType =:= "tuple") andalso
       (Target =:= "erl" orelse Target =:= "ast") ->
    TVTypeAtom = list_to_atom(TVType),

    ModAst = ep_compiler:compile(BaseDir, ProtoName, #{tv_type => TVTypeAtom}),
    case Target of
        "erl" ->
            ErlCode = ep_compiler:ast_to_erl(ModAst),
            io:format("~s~n", [ErlCode]);
        "ast" ->
            io:format("~s~n", [ModAst])
    end,
    erlang:halt(0);

main(Args) ->
    io:format("Unknown Arguments: ~p~n", [Args]),
    usage(),
    erlang:halt(0).

usage() ->
    io:format("Usage:~n"),
    io:format("  ep compile-proto map|tuple erl|ast <base-dir> <proto-name>~n"),
    io:format("    % compile proto definitions at <base-dir>/<proto-name>~n"),
    io:format("    % generate pattern for map|tuple struct~n"),
    io:format("    % compile to erl|ast~n").
