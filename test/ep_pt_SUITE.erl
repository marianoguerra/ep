-module(ep_pt_SUITE).
-compile(export_all).

all() -> [ep_test_proto_impls, ep_test_proto_decls].

data_dir(Config) -> proplists:get_value(data_dir, Config).
priv_dir(Config) -> test_server:lookup_config(priv_dir, Config).

ep_test_proto_impls(Config) ->
    DataDir = data_dir(Config),
    OutputDir = priv_dir(Config),
    ModPath = filename:join(DataDir, "ep_test_1.erl"), 
    {ok, Forms} = epp:parse_file(ModPath, []),
    NewForms = ep_pt:parse_transform(Forms, [{ep_opts, #{output_path => OutputDir}}]),
    ct:pal("~p~n", [NewForms]),

    {ok, ModName, ModBin, _Warnings} = ep_compiler:compile_forms(NewForms), 
    ep_compiler:purge_module(ModName),
    {ok, Mod} = ep_compiler:load_module(ModName, ModBin),
    1 = Mod:consy@first([1, 2]),
    [2, 3] = Mod:consy@rest([1, 2, 3]),
    Data = {1,2,3},
    DataStr = io_lib:format("~p", [Data]),
    DataStr = Mod:printable@to_string(Data),

    ErlCode = ep_compiler:ast_to_erl(NewForms),
    ct:pal("~s~n", [ErlCode]),
    PrintablePath = filename:join([OutputDir, "ep", "printable", "ep_test_1.ep"]),
    ConsyPath = filename:join([OutputDir, "ep", "consy", "ep_test_1.ep"]),
    {ok, [PrintableInfo]} = file:consult(PrintablePath),
    {ok, [ConsyInfo]} = file:consult(ConsyPath),
    ExPrintableInfo = #{info =>
                        #{funs=>#{to_string=>{my_to_string,1}},line=>3},
                        funs => #{to_string =>
                                  {function,6,my_to_string,1,
                                   [{clause,6,
                                     [{var,6,'V'}],
                                     [],
                                     [{call,6,
                                       {remote,6,{atom,6,io_lib},{atom,6,format}},
                                       [{string,6,"~p"},
                                        {cons,6,{var,6,'V'},{nil,6}}]}]}]}},
                        module=>ep_test_1,name=>printable},
    ExConsyInfo = #{info=>
                    #{funs=>#{first=>{first,1},rest=>{consy_rest,1}},line=>4},
                    funs =>
                    #{first =>
					  {function,9,first,1,
					   [{clause,9,
						 [{match,9,
						   {var,9,'L'},
						   {cons,9,{var,9,'H'},{var,9,'_'}}}],
						 [[{call,9,{atom,9,is_list},[{var,9,'L'}]}]],
						 [{var,9,'H'}]}]},
                      rest =>
                      {function,10,consy_rest,1,
                       [{clause,10,
                         [{cons,10,{var,10,'_'},{var,10,'T'}}],
                         [],
                         [{var,10,'T'}]}]}},
                    module=>ep_test_1,name=>consy},
    ExPrintableInfo = PrintableInfo,
    ExConsyInfo = ConsyInfo.

ep_test_proto_decls(Config) ->
    DataDir = data_dir(Config),
    OutputDir = priv_dir(Config),
    ModPath = filename:join(DataDir, "ep_test_2.erl"), 
    {ok, Forms} = epp:parse_file(ModPath, []),
    NewForms = ep_pt:parse_transform(Forms, [{ep_opts, #{output_path => OutputDir}}]),
    PrintablePath = filename:join([OutputDir, "ep", "printable", "ep_test_2.epd"]),
    ConsyPath = filename:join([OutputDir, "ep", "consy", "ep_test_2.epd"]),
    {ok, [PrintableInfo]} = file:consult(PrintablePath),
    {ok, [ConsyInfo]} = file:consult(ConsyPath),

    #{name := printable, module := ep_test_2, info := Info, funs := Funs} = PrintableInfo,
    #{funs := #{to_string := {my_to_string, 1}}} = Info,
    Funs = #{to_string =>
             {function,6,my_to_string,1,
              [{clause,6,
                [{var,6,'V'}],
                [[{call,6,{atom,6,is_integer},[{var,6,'V'}]}]],
                [{call,6,{atom,6,integer_to_binary},[{var,6,'V'}]}]},
               {clause,7,
                [{var,7,'V'}],
                [[{call,7,{atom,7,is_float},[{var,7,'V'}]}]],
                [{call,7,{atom,7,float_to_binary},[{var,7,'V'}]}]},
               {clause,8,
                [{var,8,'V'}],
                [[{call,8,{atom,8,is_atom},[{var,8,'V'}]}]],
                [{call,8,
                  {atom,8,atom_to_binary},
                  [{var,8,'V'},{atom,8,utf8}]}]}]}},

    #{name := consy, module := ep_test_2, info := CInfo, funs := CFuns} = ConsyInfo,
    #{funs := #{first := 1, rest := 1}} = CInfo,
    0 = maps:size(CFuns).
