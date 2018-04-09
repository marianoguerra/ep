-module(ep_pt).
-export([parse_transform/2, format_error/1]).

parse_transform(Forms0, Options) ->
    State = #{module => nil, protos => #{}, funs => #{}, warns => [], errors => []},
    {Forms, NewState} = ast_walk:forms(Forms0, fun walker/2, State),
    maybe_serialize_protos(NewState, Options),
    Forms.

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

walker(State0=#{funs := Funs}, {pre, Ast={function, _Line, Name, Arity, _Clauses}}) ->
    State = State0#{funs := Funs#{{Name, Arity} => Ast}},
    {Ast, State};

walker(State, Ast={attribute, _Line, module, Module}) ->
    {Ast, State#{module := Module}};

walker(State, Other) ->
    {Other, State}.

maybe_serialize_protos(State=#{protos := Protos, module := Module}, Options) ->
    EpOpts = proplists:get_value(ep_opts, Options, #{}),
    OutputBasePath = maps:get(output_path, EpOpts, "."),
    lists:foldl(fun ({Name, Info}, StateIn) ->
                        {ProtoFuns, StateOut} = funs_for_proto(Name, Info, StateIn),
                        serialize_proto(OutputBasePath, Module, Name, Info, ProtoFuns),
                        StateOut
                end,
                State,
                maps:to_list(Protos)).

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


serialize_proto(BasePath, Module, Name, Info, Funs) ->
    FileName = atom_to_list(Module) ++ "." ++ atom_to_list(Name) ++ ".ep",
    Path = filename:join([BasePath, "ep", FileName]),
    dump(Path, #{module => Module, name => Name, info => Info, funs => Funs}).

dump(Path, Data) ->
    Str = io_lib:format("~w.", [Data]),
    filelib:ensure_dir(Path),
    file:write_file(Path, Str).

add_warning(State=#{warns := Warns}, Warn) ->
    State#{warns := [Warn | Warns]}.
