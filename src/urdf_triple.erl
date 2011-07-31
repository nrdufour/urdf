
% This file is part of urdf released under the MIT license.
% See the LICENSE file for more information.

-module(urdf_triple).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([factorize/1]).

-include("triples.hrl").

factorize(Triples) ->
    Fun = fun(Triple, D) ->
        Subject = Triple#triple.subject,

        EntityList = case dict:is_key(Subject, D) of
            true -> dict:fetch(Subject, D);
            false -> []
        end,

        UpdatedEntityList = EntityList ++ [Triple],

        dict:store(Subject, UpdatedEntityList, D)
    end,
    lists:foldl(Fun, dict:new(), Triples).
