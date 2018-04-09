-module(ep_pt_SUITE).
-compile(export_all).

all() -> [ep_test_1].

data_dir(Config) -> proplists:get_value(data_dir, Config).
priv_dir(Config) -> test_server:lookup_config(priv_dir, Config).

ep_test_1(Config) ->
    DataDir = data_dir(Config),
    OutputDir = priv_dir(Config),
    ModPath = filename:join(DataDir, "ep_test_1.erl"), 
    {ok, Forms} = epp:parse_file(ModPath, []),
    ep_pt:parse_transform(Forms, [{ep_opts, #{output_path => OutputDir}}]),
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
                         [{cons,9,{var,9,'H'},{var,9,'_'}}],
                         [],
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

