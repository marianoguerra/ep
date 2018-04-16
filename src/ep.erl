-module(ep).
% for escript
-export([main/1]).

% for parse transform
-export([parse_transform/2, format_error/1]).

% rebar3 plugin
-export([init/1]).

%% escript API
main(Args) -> ep_escript:main(Args).

%% parse transform API
parse_transform(Forms, Options) -> ep_pt:parse_transform(Forms, Options).
format_error(Error) -> ep_pt:format_error(Error).

%% rebar3 plugin API
init(State) -> ep_rebar3_plugin:init(State).
