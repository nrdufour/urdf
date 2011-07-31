
% This file is part of urdf released under the MIT license.
% See the LICENSE file for more information.

-module(urdf_t2n).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([convert/1]).

-include("triples.hrl").

convert(Triples) ->
    Entities = urdf_triple:factorize(Triples),

    ConvertValue = fun(Triple, {ItemId, Acc}) ->

        Key = Triple#triple.property,
        Value = Triple#triple.object,
        Type = Triple#triple.type,

        WrappedValue = case Type of
            resource ->
                case Value of
                    << "_:", _Rest/binary >> -> Value;
                    _ -> iolist_to_binary([<<"<">>, Value, <<">">>])
                end;
            _ -> iolist_to_binary([<<"\"">>, Value, <<"\"">>])
        end,

        NewValue = iolist_to_binary([ItemId, <<" <">>, Key, <<"> ">>, WrappedValue, <<".\n">>]),
        {ItemId, << Acc/binary, NewValue/binary >>}
    end,

    ConvertEntity = fun(ItemId, Values, Acc) ->
        WrappedItemId = case ItemId of
            << "_:", _Rest/binary >> -> ItemId;
            _ -> iolist_to_binary([<<"<">>, ItemId, <<">">>])
        end,

        {_, DisplayedValues} = lists:foldl(ConvertValue, {WrappedItemId, <<"">>}, Values),
        Acc ++ [iolist_to_binary([DisplayedValues,  <<"\n">>])]
    end,

    iolist_to_binary(dict:fold(ConvertEntity, [], Entities)).
