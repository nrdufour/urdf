
% This file is part of urdf released under the MIT license.
% See the LICENSE file for more information.

-module(jsonld_normalize).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

%
% This module will normalize a JSON-LD document stored as EEP018 list
% into the same format.
%

-export([do_it/1]).

-include("jsonld.hrl").

-record(ctx, {
    objects = [],
    stuff
}).

do_it(Doc) when is_list(Doc) ->
    Objects = prepare_list(Doc),

    NormalizeObject = fun(Object, Ctx) ->
        normalize_object(Object, Ctx)
    end,

    FinalCtx = lists:foldl(
        NormalizeObject,
        #ctx{},
        Objects
    ),

    FinalCtx#ctx.objects.

% --- Internal API ---

prepare_list(Doc) ->
    case ?IS_OBJECT(Doc) of
        true  ->
            [Doc];
        false ->
            Doc
    end.


normalize_object(Object, Ctx) ->
    ok.
