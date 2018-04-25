-module(ep_pt).
-export([parse_transform/2, format_error/1]).

parse_transform(Forms0, Options) ->
    State = #{
      module => nil,
      protos => #{},
      decls => #{},
      funs => #{},
      warns => [],
      errors => [],
      exports => #{}
     },
    {Forms, NewState} = ast_walk:forms(Forms0, fun walker/2, State),
    maybe_serialize_protos(NewState, Options),
    maybe_serialize_decls(NewState, Options),
    remove_outdated_protos(NewState, Options),
    remove_outdated_decls(NewState, Options),
    [N1, N2 | AstRest] = Forms,
    ExportsAttr = {attribute, 3, export, gen_exports(NewState)},
    [N1, N2, ExportsAttr | remove_eof(AstRest) ++ gen_proto_functions(NewState)].

format_error(Error) -> atom_to_list(Error).

walker(State0=#{protos := Protos},
       Ast={attribute, Line, ep, {Name, Funs}}) ->

    ProtoInfo = #{line => Line, funs => Funs},
    NewProtos = Protos#{Name => ProtoInfo},

    State = case maps:get(Name, Protos, nil) of
                nil ->
                    State0#{protos := NewProtos};
                CurProtoInfo ->
                    add_warning(State0#{protos := NewProtos},
                              {duplicated_definition,
                               #{name => Name,
                                 current => CurProtoInfo,
                                 new => ProtoInfo}})
            end,
    {Ast, State};

walker(State0=#{decls := Decls},
       Ast={attribute, Line, def_ep, {Name, Funs}}) ->

    DeclInfo = #{line => Line, funs => Funs},
    NewDecls = Decls#{Name => DeclInfo},

    State = case maps:get(Name, Decls, nil) of
                nil ->
                    State0#{decls := NewDecls};
                CurDeclInfo ->
                    add_warning(State0#{decls := NewDecls},
                              {duplicated_definition,
                               #{name => Name,
                                 current => CurDeclInfo,
                                 new => DeclInfo}})
            end,
    {Ast, State};

walker(State0=#{funs := Funs}, {pre, Ast={function, _Line, Name, Arity, _Clauses}}) ->
    State = State0#{funs := Funs#{{Name, Arity} => Ast}},
    {Ast, State};

walker(State, Ast={attribute, _Line, module, Module}) ->
    {Ast, State#{module := Module}};

walker(State=#{exports := CurExports}, Ast={attribute, _Line, export, Exports}) ->
    ExportsMap = maps:from_list([{FA, true} || FA <- Exports]),
    NewExports = maps:merge(CurExports, ExportsMap),
    {Ast, State#{exports := NewExports}};

walker(State, Other) ->
    {Other, State}.

maybe_serialize_protos(State=#{protos := Protos, module := Module}, Options) ->
    OutputBasePath = get_output_base_path(Options),
    lists:foldl(fun ({Name, Info}, StateIn) ->
                        {ProtoFuns, StateOut} = funs_for_proto(Name, Info, StateIn),
                        serialize_proto(OutputBasePath, Module, Name, Info, ProtoFuns),
                        StateOut
                end,
                State,
                maps:to_list(Protos)).

maybe_serialize_decls(State=#{decls := Decls , module := Module}, Options) ->
    EpOpts = proplists:get_value(ep_opts, Options, #{}),
    OutputBasePath = maps:get(output_path, EpOpts, "."),
    lists:foldl(fun ({Name, Info}, StateIn) ->
                        {Funs, StateOut} = funs_for_decl(Name, Info, StateIn),
                        serialize_decl(OutputBasePath, Module, Name, Info, Funs),
                        StateOut
                end,
                State,
                maps:to_list(Decls)).

funs_for_proto(Name, #{funs := PFuns}, State=#{funs := Funs}) ->
    lists:foldl(fun ({PFName, FunId={_FName, _FArity}}, {PFunsIn, StateIn}) ->
                        case maps:get(FunId, Funs, not_found) of
                            not_found ->
                                StateOut = add_warning(StateIn,
                                                       {fun_impl_not_found,
                                                        #{fid => FunId,
                                                          proto => Name}}),
                                {PFunsIn, StateOut};
                            Ast ->
                                {PFunsIn#{PFName => Ast}, StateIn}
                        end
                end,
                {#{}, State},
                maps:to_list(PFuns)).

funs_for_decl(Name, #{funs := PFuns}, State=#{funs := Funs}) ->
    lists:foldl(fun
                    ({PFName, FunId={_FName, FArity}}, {PFunsIn, StateIn}) ->
                        case maps:get(FunId, Funs, not_found) of
                            not_found ->
                                StateOut = add_warning(StateIn,
                                                       {fun_impl_not_found,
                                                        #{fid => FunId,
                                                          decl => Name}}),
                                {PFunsIn, StateOut};
                            Ast ->
                                {PFunsIn#{{PFName, FArity}=> Ast}, StateIn}
                        end;
                    ({_PFName, FArity}, AccumOut) when is_integer(FArity)->
                        % proto fun declaration with arity only
                        AccumOut
                end,
                {#{}, State},
                maps:to_list(PFuns)).


serialize_proto(BasePath, Module, Name, Info, Funs) ->
    FileName = atom_to_list(Module) ++ ".ep",
    Path = filename:join([BasePath, "ep", atom_to_list(Name), FileName]),
    dump(Path, #{module => Module, name => Name, info => Info, funs => Funs}).

serialize_decl(BasePath, Module, Name, Info, Funs) ->
    FileName = atom_to_list(Module) ++ ".epd",
    Path = filename:join([BasePath, "ep", atom_to_list(Name), FileName]),
    dump(Path, #{module => Module, name => Name, info => Info, funs => Funs}).

dump(Path, Data) ->
    Str = io_lib:format("~w.", [Data]),
    filelib:ensure_dir(Path),
    file:write_file(Path, Str).

add_warning(State=#{warns := Warns}, Warn) ->
    State#{warns := [Warn | Warns]}.

gen_exports(#{protos := Protos}) ->
    FoldFn = fun ({ProtoName, #{funs := Funs}}, AccumIn) ->
                     NewExports = [{ep_compiler:gen_proto_fun_name(ProtoName, PFunName), Arity} ||
                                   {PFunName, {_FName, Arity}} <- maps:to_list(Funs)],
                     NewExports ++ AccumIn
             end,
    lists:foldl(FoldFn, [], maps:to_list(Protos)).

gen_proto_functions(#{protos := Protos, module := Module}) ->
    Line = 9001,
    FoldFn = fun ({ProtoName, #{funs := Funs}}, AccumIn) ->
                     FunsList = maps:to_list(Funs),
                     NewExports = [gen_proto_fun_ast(Line, Arity, Module, ep_compiler:gen_proto_fun_name(ProtoName, PFunName), FName)
                                   || {PFunName, {FName, Arity}} <- FunsList],
                     NewExports ++ AccumIn
             end,
    lists:foldl(FoldFn, [], maps:to_list(Protos)).

gen_proto_fun_ast(Line, Arity, _Module, FunName, FunCallName) ->
    Args = ep_compiler:gen_var_args(Line, Arity),
    Clause = {clause, Line, Args, [],
              [{call,Line, {atom, Line, FunCallName}, Args}]},
    {function, Line, FunName, Arity, [Clause]}.

get_output_base_path(Options) ->
    EpOpts = proplists:get_value(ep_opts, Options, #{}),
    maps:get(output_path, EpOpts, ".").

get_outdated(FileName, Options, NameMap) ->
    BasePath = get_output_base_path(Options),
    PathBlob = filename:join([BasePath, "ep", "*", FileName]),
    Existing = filelib:wildcard(PathBlob),
    Current = maps:from_list([{list_to_binary(filename:join([BasePath, "ep", Name, FileName])), true} || Name <- maps:keys(NameMap)]),
    lists:filter(fun (Item) ->
						 not maps:get(list_to_binary(Item), Current, false)
                 end, Existing).

remove_outdated_protos(#{module := Module, protos := Protos}, Options) ->
    FileName = atom_to_list(Module) ++ ".ep",
    ToRemove = get_outdated(FileName, Options, Protos),
    [file:delete(Path) || Path <- ToRemove].

remove_outdated_decls(#{module := Module, decls := Decls}, Options) ->
    FileName = atom_to_list(Module) ++ ".epd",
    ToRemove = get_outdated(FileName, Options, Decls),
    [file:delete(Path) || Path <- ToRemove].

remove_eof(Ast) ->
	lists:filter(fun ({eof, _}) -> false; (_) -> true end, Ast).
