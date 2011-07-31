
% This file is part of urdf released under the MIT license.
% See the LICENSE file for more information.

-module(urdf_t2j).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([convert/1]).

-include("triples.hrl").

convert(Triples) ->

    Entities = urdf_triple:factorize(Triples),

    ConvertValue = fun(Triple, Acc) ->
        Key = Triple#triple.property,
        Value = Triple#triple.object,
        Type = Triple#triple.type,

        WrappedValue = case Type of
            resource -> iolist_to_binary([<<"{\n    \"@iri\": \"">>, Value, <<"\"\n  }">>]);
            _ -> iolist_to_binary([<<"\"">>, Value, <<"\"">>])
        end,

        iolist_to_binary([Acc, <<",\n  \"">>, Key, <<"\": ">>, WrappedValue])
    end,

    ConvertEntity = fun(ItemId, Values, Acc) ->
        Subject = iolist_to_binary([<<"{\n  \"@subject\": {\n    \"@iri\": \"">>, ItemId, <<"\"\n  }">>]),

        DisplayedValues = lists:foldl(ConvertValue, <<"">>, Values),

        Comma = case Acc of
            [] -> <<"">>;
            _  -> <<", ">>
        end,

        iolist_to_binary([Acc, Comma, Subject, DisplayedValues, <<"\n}">>])
    end,

    iolist_to_binary([<<"[">>, dict:fold(ConvertEntity, [], Entities), <<"]\n\n">>]).
