
% This file is part of urdf released under the MIT license.
% See the LICENSE file for more information.

-module(urdf_t2n).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([convert/1]).

-include("triples.hrl").

convert(Triples) ->
    Entities = factorize(Triples),

    ConvertValue = fun({Key, Value, Type}, {ItemId, Acc}) ->

        WrappedValue = case Type of
            resource ->
                case Value of
                    << "_:", _Rest/binary >> -> Value;
                    _ -> list_to_binary([<<"<">>, Value, <<">">>])
                end;
            _ -> list_to_binary([<<"\"">>, Value, <<"\"">>])
        end,

        NewValue = list_to_binary([ItemId, <<" <">>, Key, <<"> ">>, WrappedValue, <<".\n">>]),
        {ItemId, << Acc/binary, NewValue/binary >>}
    end,

    ConvertEntity = fun(ItemId, Values, Acc) ->
        WrappedItemId = case ItemId of
            << "_:", _Rest/binary >> -> ItemId;
            _ -> list_to_binary([<<"<">>, ItemId, <<">">>])
        end,

        {_, DisplayedValues} = lists:foldl(ConvertValue, {WrappedItemId, <<"">>}, Values),
        Acc ++ [list_to_binary([DisplayedValues,  <<"\n">>])]
    end,

    dict:fold(ConvertEntity, [], Entities).

factorize(Triples) ->
    Fun = fun(Triple, D) ->
        Subject = Triple#triple.subject,

        EntityList = case dict:is_key(Subject, D) of
            true -> dict:fetch(Subject, D);
            false -> []
        end,

        UpdatedEntityList = EntityList ++ [{ Triple#triple.property, Triple#triple.object, Triple#triple.type }],

        dict:store(Subject, UpdatedEntityList, D)
    end,
    lists:foldl(Fun, dict:new(), Triples).
