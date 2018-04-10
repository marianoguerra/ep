-module(ep_compiler).
-export([compile/2, compile_asts/2, ast_to_erl/1]).
-export([gen_proto_fun_name/2, gen_var_args/2]).
-export([compile_forms/1, load_module/2, soft_purge_module/1, purge_module/1]).

compile(BasePath, ProtoName) ->
    PathBlob = filename:join([BasePath, ProtoName, "*.ep"]),
    ProtoPaths = filelib:wildcard(PathBlob),
    FileContents = [file:consult(Path) || Path <- ProtoPaths],
    % handle errors
    Asts = [Ast || {ok, [Ast]} <- FileContents],
    AstMap = compile_asts(Asts, ProtoName),
    ast_map_to_mod_ast(AstMap, ProtoName).

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

ast_map_to_mod_ast(AstMap, ProtoName) ->
    [{attribute, 1, module, list_to_atom(ProtoName)},
     {attribute, 2, export, maps:keys(AstMap)}|
     maps:values(AstMap)].

compile_asts(Asts, ProtoName) ->
    compile_asts(Asts, #{}, ProtoName).

compile_asts([], Accum, ProtoName) ->
    add_generic_router_clause_to_funs(Accum, ProtoName);
compile_asts([#{funs := Funs, module := Module} | T], Accum, ProtoName) ->
    FoldFn = fun ({PFunName, {function, _Line, _ImplFunName, Arity, [Clause]}},
                  AccumIn) ->
                     % we use the arity of the impl since it should be
                     % checked agains definition before dumping
                     add_fun_clause(PFunName, Module, ProtoName, Arity, Clause, AccumIn)
             end,
    NewAccum = lists:foldl(FoldFn, Accum, maps:to_list(Funs)),
    compile_asts(T, NewAccum, ProtoName).

add_generic_router_clause_to_funs(Accum, ProtoName) ->
    maps:map(fun ({PFunName, Arity}, Ast) ->
                     {function, Line, PFunName, Arity, Clauses} = Ast,
					 NewClause = generic_router_clause(Line, Arity, ProtoName, PFunName),
                     NewClauses = Clauses ++ [NewClause],
                     {function, Line, PFunName, Arity, NewClauses}

             end, Accum).

add_fun_clause(PFunName, Module, ProtoName, Arity, _OrigClause, Accum) ->
    Line = 3,
    FunKey = {PFunName, Arity},
    Args = gen_var_args(Line, Arity),
    FunName = gen_proto_fun_name(ProtoName, PFunName),
    % we route to the generated proto impl name since we know it will be
    % exported and for uniformity
    Clause = ast_match_args_call_remote(Line, Args, {atom, Line, Module},
                                        {atom, Line, FunName}, Args),
    NewAst = case maps:get(FunKey, Accum, not_found) of
                 not_found ->
                     {function, Line, PFunName, Arity, [Clause]};
                 {function, CLine, PFunName, Arity, Clauses} ->
                     {function, CLine, PFunName, Arity, [Clause | Clauses]}
             end,
    Accum#{FunKey => NewAst}.

gen_proto_fun_name(ProtoName, PFunName) when is_atom(ProtoName) ->
    gen_proto_fun_name(atom_to_list(ProtoName), PFunName);
gen_proto_fun_name(ProtoNameStr, PFunName) ->
	PFunNameStr = atom_to_list(PFunName),
	list_to_atom(ProtoNameStr ++ "@" ++ PFunNameStr).

generic_router_clause(Line, Arity, ProtoName, PFunName) ->
    FunName = gen_proto_fun_name(ProtoName, PFunName),

	FirstArg = {match,Line,
				{var,Line,'V'},
				{map,Line,
				 [{map_field_exact,Line,
				   {atom,Line,'__struct__'},
				   {var,Line,'Type'}}]}},

	RestArgs = gen_var_args(Line, Arity - 1),

	HeadArgs = [FirstArg | RestArgs],
	CallArgs = [{var,Line,'V'}|RestArgs],
    ModAst = {var,Line,'Type'},
    FunAst = {atom,Line,FunName},
    ast_match_args_call_remote(Line, HeadArgs, ModAst, FunAst, CallArgs).

ast_match_args_call_remote(Line, HeadArgs, ModAst, FunAst, CallArgs) ->
	{clause, Line, HeadArgs, [],
	 [{call,Line, {remote,Line,ModAst,FunAst}, CallArgs}]}.

gen_var_args(Line, Count) ->
	[{var, Line, list_to_atom("Arg" ++ integer_to_list(I))} ||
	 I <- lists:seq(1, Count)].
