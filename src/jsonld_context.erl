
% This file is part of urdf released under the MIT license.
% See the LICENSE file for more information.

-module(jsonld_context).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-include("jsonld.hrl").

-export([create_default/0, merge/2, has_namespace/2, get_namespace/2, get_base/1, get_vocab/1]).

-record(context, {
        namespaces,
        base = undefined,
        vocab = undefined,
        coerce
    }
).

create_default() ->
    DefaultContext = create_default_context(),
    DefaultCoerce = create_default_coerce(),
    #context{ namespaces = DefaultContext, coerce = DefaultCoerce }.

merge(Context, NewNamespaces) ->
    Fun = fun({Key, Value}, Ctx) ->
        case Key of
            ?VOCAB_KEY  -> Ctx#context{ vocab = Value };
            ?BASE_KEY   -> Ctx#context{ base  = Value };
            ?COERCE_KEY ->
                % TODO
                Ctx;
            _ ->
                UpdatedNamespaces = dict:store(Key, Value, Ctx#context.namespaces),
                Ctx#context{ namespaces = UpdatedNamespaces }
        end
    end,
    lists:foldl(Fun, Context, NewNamespaces).

has_namespace(Context, Prefix) ->
    dict:is_key(Prefix, Context#context.namespaces).

get_namespace(Context, Prefix) ->
    dict:fetch(Prefix, Context#context.namespaces).

get_base(Context) ->
    Context#context.base.

get_vocab(Context) ->
    Context#context.vocab.

%
% Internal API
%

create_default_context() ->
    InitialDict = dict:new(),
    lists:foldl(
        fun({Key, Value}, Dict) ->
            dict:store(Key, Value, Dict)
        end,
        InitialDict,
        ?DEFAULT_CONTEXT).

create_default_coerce() ->
    dict:new().
