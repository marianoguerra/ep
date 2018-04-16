-module(ep_rebar3_plugin).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, ep).
-define(DEPS, [compile]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 ep"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ErlOpts = rebar_state:get(State, erl_opts, []),
    EpOpts = proplists:get_value(ep_opts, ErlOpts, #{}), 

    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    [begin
         %Opts = rebar_app_info:opts(AppInfo),
         %InDir = rebar_app_info:out_dir(AppInfo),
         %BaseDir = filename:join(InDir, "ep_protos"),
         OutDir = rebar_app_info:ebin_dir(AppInfo),
         AppName = rebar_app_info:name(AppInfo),
         BaseDir = filename:join(maps:get(output_path, EpOpts, "./"), "ep"),
         rebar_api:info("Compiling Erlang Protocols for app '~s'", [AppName]),
         R = compile_protos(BaseDir, OutDir, EpOpts),
         rebar_api:debug("  Result ~p", [R]),
         R
     end || AppInfo <- Apps],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% private api
compile_protos(BaseDir, OutDir, EpOpts) ->
    [compile_proto(BaseDir, ProtoName, OutDir, EpOpts) ||
     ProtoName <- list_dirs(BaseDir)].

compile_proto(BaseDir, ProtoName, OutDir, EpOpts) ->
    rebar_api:info("  Compiling Erlang Protocol '~s'", [ProtoName]),
    ModAst = ep_compiler:compile(BaseDir, ProtoName, EpOpts),
    ep_compiler:ast_to_beam_file(ModAst, OutDir).

list_dirs(BaseDir) ->
    case file:list_dir(BaseDir) of
        {ok, Filenames} ->
            [Filename ||
             Filename <- Filenames,
             filelib:is_dir(filename:join(BaseDir, Filename))];
        {error, enoent} ->
            []
    end.
