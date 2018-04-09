-module(ep_pt).
-export([parse_transform/2, format_error/1]).

parse_transform(Forms0, Options) ->
    State = #{module => nil, protos => #{}, funs => #{}, warns => [], errors => []},
    {Forms, NewState} = ast_walk:forms(Forms0, fun walker/2, State),
    maybe_serialize_protos(NewState, Options),
    Forms.

format_error(Error) -> atom_to_list(Error).

walker(State0=#{protos := Protos, warns := Warns},
       Ast={attribute, Line, ep, {Name, Funs}}) ->

    ProtoInfo = #{line => Line, funs => Funs},
    NewProtos = Protos#{Name => ProtoInfo},

    State = case maps:get(Name, Protos, nil) of
                nil ->
                    State0#{protos := NewProtos};
                CurProtoInfo ->
                    Warn = {duplicated_definition,
                            #{name => Name,
                              current => CurProtoInfo,
                              new => ProtoInfo}},

                    NewWarns = [Warn | Warns],
                    State0#{protos := NewProtos, warns := NewWarns}
            end,
    {Ast, State};

walker(State0=#{funs := Funs}, {pre, Ast={function, _Line, Name, Arity, _Clauses}}) ->
    State = State0#{funs := Funs#{{Name, Arity} => Ast}},
    {Ast, State};

walker(State, Ast={attribute, _Line, module, Module}) ->
    {Ast, State#{module := Module}};

walker(State, Other) ->
    {Other, State}.

maybe_serialize_protos(#{protos := Protos, module := Module}, Options) ->
    EpOpts = proplists:get_value(ep_opts, Options, #{}),
    OutputBasePath = maps:get(output_path, EpOpts, "."),
    [serialize_proto(OutputBasePath, Module, Name, Info) || {Name, Info} <- maps:to_list(Protos)].

serialize_proto(BasePath, Module, Name, Info) ->
    FileName = atom_to_list(Module) ++ "." ++ atom_to_list(Name) ++ ".ep",
    Path = filename:join([BasePath, "ep", FileName]),
    dump(Path, #{module => Module, name => Name, info => Info}).

dump(Path, Data) ->
    Str = io_lib:format("~w.", [Data]),
    ct:pal("writing ~p -> ~s", [Path, Str]),
    filelib:ensure_dir(Path),
    file:write_file(Path, Str).
