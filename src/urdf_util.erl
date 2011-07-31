
% This file is part of urdf released under the MIT license.
% See the LICENSE file for more information.

-module(urdf_util).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([uuid/0, new_bnode/0, is_proplist/1]).

uuid() ->
    Now = {_, _, Micro} = now(),
    Nowish = calendar:now_to_universal_time(Now),
    Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
    Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
    list_to_binary(Prefix ++ to_hex(crypto:rand_bytes(9))).

-define(BNODE_PREFIX, <<"_:">>).

new_bnode() ->
    UUID = uuid(),
    << ?BNODE_PREFIX/binary, UUID/binary >>.

is_proplist(Object) when is_list(Object) ->
    Fun = fun(X, Acc) ->
        IsProp = case X of
            {_, _} -> true;
            _      -> false
        end,
        Acc and IsProp
    end,
    lists:foldl(Fun, true, Object);
is_proplist(_) ->
    false.

%
% Internal API
%

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.
