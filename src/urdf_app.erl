
% This file is part of erlang-json-ld released under the MIT license.
% See the LICENSE file for more information.

-module(urdf_app).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    urdf_sup:start_link().

stop(_State) ->
    ok.
