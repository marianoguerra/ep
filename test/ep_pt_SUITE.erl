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
    PrintablePath = filename:join([OutputDir, "ep", "ep_test_1.printable.ep"]),
    ConsyPath = filename:join([OutputDir, "ep", "ep_test_1.consy.ep"]),
    {ok, [PrintableInfo]} = file:consult(PrintablePath),
    {ok, [ConsyInfo]} = file:consult(ConsyPath),
    ExPrintableInfo = #{info =>
                        #{funs=>#{to_string=>{my_to_string,1}},line=>3},
                        module=>ep_test_1,name=>printable},
    ExConsyInfo = #{info=>
                    #{funs=>#{first=>{first,1},rest=>{consy_rest,1}},line=>4}
                    ,module=>ep_test_1,name=>consy},
    ExPrintableInfo = PrintableInfo,
    ExConsyInfo = ConsyInfo.

