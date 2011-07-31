
% This file is part of urdf released under the MIT license.
% See the LICENSE file for more information.

-module(urdf_j2t).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([convert/1]).

-include("triples.hrl").
-include("jsonld.hrl").

-record(state, {
    context,
    subject,
    triples = []
}).

convert(Doc) ->
    % decode the binary json into eep0018
    JsonItem = jsx:json_to_term(Doc),

    % create the jsonld processor initial state
    DefaultContext = jsonld_context:create_default(),
    InitialState = #state{context = DefaultContext},

    % extract the triples
    FinalState = triples(JsonItem, InitialState),

    % return them
    FinalState#state.triples.

% ----------------------------------------------------------------------------
% Internal API
% ----------------------------------------------------------------------------

% Patterns
-define(IRI_PATTERN, "^<?(?<iri>(?<prefix>\\w+)\\:(?<iri_starter>/?)(/?)(?<name>[^>\\s]+))>?$").
-define(BNODE_PATTERN, "^_\\:\\w+$").
-define(CURIE_PATTERN, "^(?<prefix>\\w+)\\:(?<reference>\\w+)$").
-define(ABSOLUTE_PATTERN, "^(?<iri>(\\w+)\\:(/?)(/?)([^>\\s]+))$").
-define(ABSOLUTE_IRI_PATTERN, "^(?<iri>(\\w+)\:(/?)(/?)([^>\\s]+))$").
-define(RELATIVE_IRI_PATTERN, "^(?<iri>[^\\:>\\s]+)$").
-define(LANG_PATTERN, "^(?<literal>.+)@(?<lang>[a-zA-Z][a-zA-Z0-9\\-]+)$").
-define(TYPED_LITERAL_PATTERN, "^(?<literal>.+)\\^\\^(?<datatype>.+)$").
-define(DATETIME_PATTERN, "^(?<year>\\d\\d\\d\\d)([-])?(?<month>\\d\\d)([-])?(?<day>\\d\\d)((T|\\s+)(?<hour>\\d\\d)(([:])?(?<minute>\\d\\d)(([:])?(?<second>\\d\\d)(([.])?(?<fraction>\\d+))?)?)?)?((?<tzzulu>Z)|(?<tzoffset>[-+])(?<tzhour>\\d\\d)([:])?(?<tzminute>\\d\\d))?$").

% default processing function: using an object or an array
triples(Item, InitialState) ->
    case ?IS_OBJECT(Item) of
        % it's an object
        true ->
            % Local Context: merge if exists
            StateWithLocalContext = process_local_context(Item, InitialState),
            % Subject
            StateWithSubject = process_subject(Item, StateWithLocalContext),
            % Everything else
            process_other(Item, StateWithSubject);
        % it's an array
        false ->
            % process each element according to their type
            % and merge the result
            AllTriples = lists:foldl(
                fun(Element, Acc) ->
                    CurrentState = triples(Element, InitialState),
                    CurrentTriples = CurrentState#state.triples,
                    Acc ++ CurrentTriples
                end,
                InitialState#state.triples,
                Item
            ),
            InitialState#state{triples = AllTriples}
    end.

process_local_context(JsonObject, InitialState) ->
    % Local Context: merge if exists
    LocalContextProp = ?HAS_VALUE(JsonObject, ?LOCAL_CONTEXT_KEY),
    case LocalContextProp of
        false -> InitialState;
        {_, Value} ->
            NewContext = jsonld_context:merge(InitialState#state.context, Value),
            InitialState#state{context = NewContext}
    end.

% -- Subject --

process_subject(JsonObject, StateWithLocalContext) ->
    % Subject
    LocalSubjectProp = ?HAS_VALUE(JsonObject, ?SUBJECT_KEY),
    case LocalSubjectProp of
        {_, SubjectValue} -> process_subject_value(SubjectValue, StateWithLocalContext);
        false ->
            BNode = urdf_util:new_bnode(),
            StateWithLocalContext#state{ subject = BNode }
    end.

% The subject value can be an object, an array or a simple literal (to confirm here...)

process_subject_value(SubjectValue, StateWithLocalContext) when is_list(SubjectValue) ->
    case ?IS_OBJECT(SubjectValue) of
        % this is an object
        true  ->
            TriplesFromObject = triples(SubjectValue, StateWithLocalContext),
            CurrentSubject = ?HAS_VALUE(SubjectValue, ?SUBJECT_KEY),
            StateWithLocalContext#state{
             subject = CurrentSubject,
              triples = lists:append(TriplesFromObject, StateWithLocalContext#state.triples)
            };
        % this is an array
        false ->
            TriplesFromList = triples(SubjectValue, StateWithLocalContext),
            BNode = urdf_util:new_bnode(),
            StateWithLocalContext#state{
                subject = BNode,
                triples = lists:append(TriplesFromList, StateWithLocalContext#state.triples)
            }
    end;

% the subject value is neither an object or an array
process_subject_value(SubjectValue, StateWithLocalContext) ->
    CurrentSubject = process_resource(SubjectValue, StateWithLocalContext#state.context),
    StateWithLocalContext#state{ subject = CurrentSubject }.

% ---

process_other(JsonObject, StateWithSubject) ->
    lists:foldl(
        fun({Key, Value}, State) ->
            case Key of
                ?LOCAL_CONTEXT_KEY -> State;
                ?SUBJECT_KEY -> State;
                _ ->
                    Property = case Key of
                        ?TYPE_KEY -> <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#type">>;
                        _ -> process_property(Key, State#state.context)
                    end,
                    case is_list(Value) of
                        true  ->
                            case ?IS_OBJECT(Value) of
                                true  ->
                                    NewState = triples(Value, State),
                                    LinkedTripleObject = case ?HAS_VALUE(Value, ?SUBJECT_KEY) of
                                        {_, SubjectValue} -> SubjectValue;
                                        false ->
                                            urdf_util:new_bnode()
                                    end,
                                    LinkedTriple = triple(State#state.subject, Property, LinkedTripleObject, State#state.context),
                                    NewState#state{triples = lists:append(NewState#state.triples, [LinkedTriple])};
                                false ->
                                    % TODO Need to process array item here
                                    State
                            end;
                        false ->
                            Triple = triple(State#state.subject, Property, Value, State#state.context),
                            State#state{triples = lists:append(State#state.triples, [Triple])}
                    end
            end
        end,
        StateWithSubject,
        JsonObject).

% ---

is_resource(_Subject, _Property, Object, Context) ->
    jsonld_context:has_namespace(Context, Object)
    or not(nomatch == re:run(Object, ?BNODE_PATTERN))
    or not(nomatch == re:run(Object, ?CURIE_PATTERN))
    or not(nomatch == re:run(Object, ?ABSOLUTE_IRI_PATTERN)).
%    or not(nomatch == re:run(Object, ?RELATIVE_IRI_PATTERN)).

triple(Subject, Property, Object, Context) ->
    case is_resource(Subject, Property, Object, Context) of
        true  -> process_resource_valued_triple(Subject, Property, Object, Context);
        false -> process_literal_valued_triple(Subject, Property, Object, Context)
    end.

process_resource_valued_triple(Subject, Property, Object, Context) ->
    #triple{type = resource, subject = Subject, property = Property, object = process_resource(Object, Context)}.

process_literal_valued_triple(Subject, Property, Object, _Context) ->
    #triple{type = literal, subject = Subject, property = Property, object = Object}.

process_resource(Object, Context) ->
    AbsoluteIri = re:run(Object, ?ABSOLUTE_IRI_PATTERN, [{capture, ['iri'], binary}]),
    RelativeIri = re:run(Object, ?RELATIVE_IRI_PATTERN, [{capture, ['iri'], binary}]),
    Curie = re:run(Object, ?CURIE_PATTERN, [{capture, ['prefix', 'reference'], binary}]),
    BNode = re:run(Object, ?BNODE_PATTERN),
    case jsonld_context:has_namespace(Context, Object) of
        true -> jsonld_context:get_namespace(Context, Object);
        false ->
            case {BNode, Curie, AbsoluteIri, RelativeIri} of
                % BNode
                {{match, _}, _, _, _} -> Object;
                % Curie
                {_, {match, [Prefix, Reference]}, _, _} ->
                    case jsonld_context:has_namespace(Context, Prefix) of
                        true ->
                            PrefixNamespace = jsonld_context:get_namespace(Context, Prefix),
                            <<PrefixNamespace/binary, Reference/binary>>;
                        false ->
                          case jsonld_context:has_namespace(Context, Reference) of
                              true ->
                                  jsonld_context:get_namespace(Context, Reference);
                              false ->
                                  throw({wrong_curie_resource, Object})
                          end
                    end;
                % AbsoluteIri
                {_, _, {match, [IRI]}, _} ->
                    PossibleBase = jsonld_context:get_base(Context),
                    Base = case PossibleBase of
                        undefined -> <<"">>;
                        _         -> PossibleBase
                    end,
                    <<Base/binary, IRI/binary>>;
                % RelativeIri
                %{_, _, _, {match, [IRI]}} ->
                %    PossibleBase = jsonld_context:get_base(Context),
                %    case PossibleBase of
                %        undefined ->
                %            throw({wrong_relative_iri_resource, Object});
                %        _ ->
                %            % TODO need something for url parsing with #base rather than just concatenate it!
                %            <<PossibleBase/binary, IRI/binary>>
                %    end;
                % Everything else
                _ ->
                    Object
                    %throw({wrong_resource, Object})
            end
    end.

process_property(Key, Context) ->
    case re:run(Key, ?IRI_PATTERN, [{capture, ['iri', 'prefix', 'iri_starter', 'name'], binary}]) of
        {match, [_IRI, _Prefix, <<"/">>, _Name]} -> Key;
        {match, [_IRI, <<"_">>, _IRI_Starter, _Name]} -> Key;
        {match, [IRI, Prefix, _IRI_Starter, Name]} ->
            case jsonld_context:has_namespace(Context, Prefix) of
                true ->
                    URI = jsonld_context:get_namespace(Context, Prefix),
                    <<URI/binary, Name/binary>>;
                false -> IRI
            end;
        _ ->
            case jsonld_context:has_namespace(Context, Key) of
                true -> jsonld_context:get_namespace(Context, Key);
                false ->
                    Vocab = jsonld_context:get_vocab(Context),
                    case Vocab of
                        undefined -> throw({bad_property, Key});
                        _ -> <<Vocab/binary, Key/binary>>
                    end
            end
    end.
