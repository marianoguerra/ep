-module(ep_compiler).
-export([compile/3, compile_asts/4, ast_to_erl/1, ast_to_beam_file/2]).
-export([gen_proto_fun_name/2, gen_var_args/2]).
-export([compile_forms/1, load_module/2, soft_purge_module/1, purge_module/1]).

compile(BasePath, ProtoName, Opts) ->
    PathDeclBlob = filename:join([BasePath, ProtoName, "*.epd"]),
    DeclPaths = filelib:wildcard(PathDeclBlob),
    DeclInfo = case DeclPaths of
                   [DeclPath] ->
                       {ok, [DeclInfo0]} = file:consult(DeclPath),
                       DeclInfo0;
                   [DeclPath | _] ->
                       warn("Multiple protocol definitions for ~p: ~p~n",
                            [ProtoName, DeclPaths], Opts),
                       {ok, [DeclInfo0]} = file:consult(DeclPath),
                       DeclInfo0;
                   [] ->
                       warn("No protocol definition for ~p ~s ~n", [ProtoName, PathDeclBlob], Opts),
                       #{}
               end,

    PathBlob = filename:join([BasePath, ProtoName, "*.ep"]),
    ProtoPaths = filelib:wildcard(PathBlob),
    FileContents = [file:consult(Path) || Path <- ProtoPaths],
    % handle errors
    Asts = [Ast || {ok, [Ast]} <- FileContents],
    AstMap = compile_asts(Asts, ProtoName, DeclInfo, Opts),
    ast_map_to_mod_ast(AstMap, ProtoName).

warn(FmtStr, Args, #{warn_fun := WarnFun}) ->
    WarnFun(FmtStr, Args);
warn(FmtStr, Args, _Opts) ->
    io:format(FmtStr, Args).

compile_forms(Forms) ->
    compile:forms(Forms, [return, binary]).

soft_purge_module(ModName) ->
    code:purge(ModName).

purge_module(ModName) ->
    code:purge(ModName).

load_module(ModName, ModBin) ->
    {module, Mod} = erlang:load_module(ModName, ModBin),
    {ok, Mod}.

ast_to_erl(ModAst) ->
  erl_prettypr:format(erl_syntax:form_list(ModAst)).

ast_to_beam_file(ModAst, OutDir) ->
    case compile:forms(ModAst, [return]) of
        {ok, ModuleName, Code} ->
            beam_to_file(ModuleName, Code, OutDir),
            {ok, ModuleName, Code, []};
        {ok, ModuleName, Code, _Warnings}=Res ->
            beam_to_file(ModuleName, Code, OutDir),
            Res;
        {error, _Errors, _Warnings}=Error -> Error;
        error -> {error, [{error, compile_forms_error}], []}
    end.

beam_to_file(ModuleName, Code, OutDir) ->
    BeamName = atom_to_list(ModuleName) ++ ".beam",
    BeamPath = filename:join(OutDir, BeamName),
    case file:write_file(BeamPath, Code) of
        ok -> {ok, ModuleName, Code, []};
        Error -> Error
    end.

ast_map_to_mod_ast(AstMap, ProtoName) ->
    [{attribute, 1, module, list_to_atom(ProtoName)},
     {attribute, 2, export, maps:keys(AstMap)}|
     maps:values(AstMap)].

compile_asts(Asts, ProtoName, DeclInfo, Opts) ->
    compile_asts(Asts, #{}, ProtoName, DeclInfo, Opts).

compile_asts([], Accum, ProtoName, DeclInfo, Opts) ->
    add_default_and_generic_router_clause_to_funs(Accum, ProtoName, DeclInfo, Opts);
compile_asts([#{funs := Funs, module := Module} | T], Accum, ProtoName, DeclInfo, Opts) ->
    FoldFn = fun ({PFunName, {function, _Line, _ImplFunName, Arity, [Clause]}},
                  AccumIn) ->
                     % we use the arity of the impl since it should be
                     % checked agains definition before dumping
                     add_fun_clause(PFunName, Module, ProtoName, Arity, Clause, AccumIn)
             end,
    NewAccum = lists:foldl(FoldFn, Accum, maps:to_list(Funs)),
    compile_asts(T, NewAccum, ProtoName, DeclInfo, Opts).

add_default_and_generic_router_clause_to_funs(Accum, ProtoName, DeclInfo, Opts) ->
    TVType = maps:get(tv_type, Opts, map),
    maps:map(fun ({PFunName, Arity}, Ast) ->
                     DefaultClauses = get_default_clauses(PFunName, Arity, DeclInfo),
                     {function, Line, PFunName, Arity, Clauses} = Ast,
					 NewClause = generic_router_clause(Line, Arity, ProtoName, PFunName, TVType),
                     NewClauses = DefaultClauses ++ Clauses ++ [NewClause],
                     {function, Line, PFunName, Arity, NewClauses}

             end, Accum).

get_default_clauses(PFunName, Arity, #{funs := Funs}) ->
    case maps:find({PFunName, Arity}, Funs) of
        error -> [];
        {ok, {function, _Line, _FunName, Arity, Clauses}} -> Clauses
    end;
get_default_clauses(_PFunName, _Arity, _Opts) ->
    [].

add_fun_clause(PFunName, Module, ProtoName, Arity,
               {clause, _CL, CHeadArgs, CGuards, _CBody}, Accum) ->
    Line = 3,
    FunKey = {PFunName, Arity},
    % we need to put the clause head arguments in the generated clause but
    % we need to pass it to the implementation, so we need to match with
    % a variable that won't clash each arg that is not already matching,
    % we will use underscore
    {HeadArgs, VarArgs} = match_clause_head_args(CHeadArgs, {[], [], 0}),
    FunName = gen_proto_fun_name(ProtoName, PFunName),
    % we route to the generated proto impl name since we know it will be
    % exported and for uniformity
    Clause = ast_match_args_call_remote(Line, HeadArgs, CGuards,
                                        {atom, Line, Module},
                                        {atom, Line, FunName}, VarArgs),
    NewAst = case maps:get(FunKey, Accum, not_found) of
                 not_found ->
                     {function, Line, PFunName, Arity, [Clause]};
                 {function, CLine, PFunName, Arity, Clauses} ->
                     {function, CLine, PFunName, Arity, [Clause | Clauses]}
             end,
    Accum#{FunKey => NewAst}.

match_clause_head_args([], {HeadArgs, VarArgs, _Idx}) ->
    {lists:reverse(HeadArgs), lists:reverse(VarArgs)};
match_clause_head_args([Ast={match, _Line, VarAst={var, _Line1, _VName}, _RightAst}|T],
                       {HeadArgs, VarArgs, Idx}) ->
    NewAccum = {[Ast|HeadArgs], [VarAst|VarArgs], Idx + 1},
    match_clause_head_args(T, NewAccum);
match_clause_head_args([Ast|T], {HeadArgs, VarArgs, Idx}) ->
    Line = element(2, Ast),
    VarAst  = gen_var(Line, Idx),
    NewAccum = {[{match, Line, VarAst, Ast}|HeadArgs], [VarAst|VarArgs], Idx + 1},
    match_clause_head_args(T, NewAccum).

gen_proto_fun_name(ProtoName, PFunName) when is_atom(ProtoName) ->
    gen_proto_fun_name(atom_to_list(ProtoName), PFunName);
gen_proto_fun_name(ProtoNameStr, PFunName) ->
	PFunNameStr = atom_to_list(PFunName),
	list_to_atom(ProtoNameStr ++ "@" ++ PFunNameStr).

generic_router_clause(Line, Arity, ProtoName, PFunName, TVType) ->
    FunName = gen_proto_fun_name(ProtoName, PFunName),
	FirstArg = gen_tv_match(Line, TVType),
	RestArgs = gen_var_args(Line, Arity - 1),

	HeadArgs = [FirstArg | RestArgs],
	CallArgs = [{var,Line,'V'}|RestArgs],
    ModAst = {var,Line,'Type'},
    FunAst = {atom,Line,FunName},
    ast_match_args_call_remote(Line, HeadArgs, [], ModAst, FunAst, CallArgs).

ast_match_args_call_remote(Line, HeadArgs, Guards, ModAst, FunAst, CallArgs) ->
	{clause, Line, HeadArgs, Guards,
	 [{call,Line, {remote,Line,ModAst,FunAst}, CallArgs}]}.

gen_var(Line, I) ->
    {var, Line, list_to_atom("Arg@" ++ integer_to_list(I))}.

gen_var_args(Line, Count) ->
	[gen_var(Line, I) || I <- lists:seq(1, Count)].

gen_tv_match(Line, map) ->
    {match,Line,
     {var,Line,'V'},
     {map,Line,
      [{map_field_exact,Line,
        {atom,Line,'__struct__'},
        {var,Line,'Type'}}]}};
gen_tv_match(Line, tuple) ->
    {match,Line,
     {var,Line,'V'},
     {tuple,Line,
      [{tuple,Line,[{var,Line,'Type'},{var,Line,'_Data'},{var,Line,'_'}]}]}}.

