
% This file is part of urdf released under the MIT license.
% See the LICENSE file for more information.

-module(urdf_j2t).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

%% TODO added to compile for now
-compile(export_all).

-include("triples.hrl").
-include("jsonld.hrl").

-record(state, {
    context,
    subject,
    triples = []
}).

convert(Doc) ->
    % decode the binary json into eep0018
    JsonItem = jsx:decode(Doc),

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

-define(BNODE_PATTERN, "^_\\:\\w+$").
-define(CURIE_PATTERN, "^(?<prefix>\\w+)\\:(?<reference>\\w+)$").
-define(ABSOLUTE_PATTERN, "^(?<iri>(\\w+)\\:(/?)(/?)([^>\\s]+))$").
-define(ABSOLUTE_IRI_PATTERN, "^(?<iri>(\\w+)\:(/?)(/?)([^>\\s]+))$").
-define(LANG_PATTERN, "^(?<literal>.+)@(?<lang>[a-zA-Z][a-zA-Z0-9\\-]+)$").
-define(TYPED_LITERAL_PATTERN, "^(?<literal>.+)\\^\\^(?<datatype>.+)$").
-define(DATETIME_PATTERN, "^(?<year>\\d\\d\\d\\d)([-])?(?<month>\\d\\d)([-])?(?<day>\\d\\d)((T|\\s+)(?<hour>\\d\\d)(([:])?(?<minute>\\d\\d)(([:])?(?<second>\\d\\d)(([.])?(?<fraction>\\d+))?)?)?)?((?<tzzulu>Z)|(?<tzoffset>[-+])(?<tzhour>\\d\\d)([:])?(?<tzminute>\\d\\d))?$").

% default processing function: using an object or an array
triples(Item, InitialState) ->
    case ?IS_OBJECT(Item) of
        % it's an object
        true ->
            % Local Context: merge if exists
            UpdatedJsonldContext = jsonld_context:process_local_context(Item, InitialState#state.context),
            StateWithLocalContext = InitialState#state{ context = UpdatedJsonldContext },
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

% Patterns to recognize an IRI (absolute or not, curie, bnode)
-define(ABSOLUTE_IRI, "^(?<iri>(\\w+)\\://([^>\\s]+))$").
-define(ABSOLUTE_CURIE, "^(?<iri>(?<prefix>\\w+)\\:(?<name>\\w+))$").
-define(RELATIVE_CURIE, "^(?<iri>(\\:)?(?<name>\\w+))$").
-define(BNODE, "^_\\:(?<id>\\w+)$").

% Process a triple object.
% In JSON-LD, it's the value for a given key.
%
% It can be:
% - an absolute IRI such as 'http://xmlns.com/foaf/0.1/name'
% - a CURIE such as 'foaf:name'
% - a relative CURIE such as ':name' or 'name'
% - a bnode such as '_:c10n1'
% - a literal such as 'hello world!'
% - a JSON object carrying:
%   - an @iri property
%   - a @literal property (with optionally @language)
%
% returns the expanded property IRI or the value
%
process_object(Object, Context) ->
    case ?IS_OBJECT(Object) of
        % object is a json object
        true  ->
            process_object_as_object(Object, Context);
        % object is an IRI or a literal
        false ->
            process_object_as_iri_or_literal(Object, Context)
    end.

process_object_as_object(_Object, _Context) ->
    to_be_implemented.

process_object_as_iri_or_literal(_Object, _Context) ->
    to_be_implemented.

is_resource(_Subject, _Property, Object, Context) ->
    jsonld_context:has_prefix(Context, Object)
    or not(nomatch == re:run(Object, ?BNODE_PATTERN))
    or not(nomatch == re:run(Object, ?CURIE_PATTERN))
    or not(nomatch == re:run(Object, ?ABSOLUTE_IRI_PATTERN)).

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
    Curie = re:run(Object, ?CURIE_PATTERN, [{capture, ['prefix', 'reference'], binary}]),
    BNode = re:run(Object, ?BNODE_PATTERN),
    case jsonld_context:has_prefix(Context, Object) of
        true -> jsonld_context:get_prefix(Context, Object);
        false ->
            case {BNode, Curie, AbsoluteIri} of
                % BNode
                {{match, _}, _, _} -> Object;
                % Curie
                {_, {match, [Prefix, Reference]}, _} ->
                    case jsonld_context:has_prefix(Context, Prefix) of
                        true ->
                            PrefixNamespace = jsonld_context:get_prefix(Context, Prefix),
                            <<PrefixNamespace/binary, Reference/binary>>;
                        false ->
                          case jsonld_context:has_prefix(Context, Reference) of
                              true ->
                                  jsonld_context:get_prefix(Context, Reference);
                              false ->
                                  throw({wrong_curie_resource, Object})
                          end
                    end;
                % AbsoluteIri
                {_, _, {match, [IRI]}} ->
                    PossibleBase = jsonld_context:get_base(Context),
                    Base = case PossibleBase of
                        undefined -> <<"">>;
                        _         -> PossibleBase
                    end,
                    <<Base/binary, IRI/binary>>;
                _ ->
                    Object
                    %throw({wrong_resource, Object})
            end
    end.

% Process a triple property.
% In JSON-LD, it's the key.
%
% It can be:
% - an absolute IRI such as 'http://xmlns.com/foaf/0.1/name'
% - a CURIE such as 'foaf:name'
% - a relative CURIE such as ':name' or 'name'
%
% returns the expanded property IRI
process_property(Key, Context) ->
    % regexp to identify which type of property we have
    AbsoluteIri = re:run(Key, ?ABSOLUTE_IRI, [{capture, ['iri'], binary}]),
    AbsoluteCurie = re:run(Key, ?ABSOLUTE_CURIE, [{capture, ['iri', 'prefix', 'name'], binary}]),
    RelativeCurie = re:run(Key, ?RELATIVE_CURIE, [{capture, ['iri', 'name'], binary}]),

    % time to see which one we have here
    case {AbsoluteIri, AbsoluteCurie, RelativeCurie} of
        % we have an absolute IRI
        {{match, [_IRI]}, _,  _} -> Key;
        % we have an absolute CURIE
        {_, {match, [_IRI, Prefix, Name]}, _} ->
            case jsonld_context:has_prefix(Context, Prefix) of
                true ->
                    URI = jsonld_context:get_prefix(Context, Prefix),
                    iolist_to_binary([URI, Name]);
                % this case is a bit odd.
                % it's like having 'foo:bar' without knowing what is 'foo'
                false -> throw([unknown_prefix, Key, Prefix])
            end;
        % we have a relative CURIE
        {_, _, {match, [_IRI, Name]}} ->
            % let's grab the @vocab value
            % or the default context URI
            % TODO need to add that default context
            Vocab = jsonld_context:get_vocab(Context),
            case Vocab of
                % no vocab is defined!
                % let's see if there is a default context
                undefined ->
                    case jsonld_context:has_prefix(Context, Name) of
                        false ->
                            % TODO define a default context in jsonld_context.erl
                            % In case somebody would do:
                            % @context: "http://...."

                            % throwing an error for now
                            throw([bad_property, Key]);
                        true ->
                            jsonld_context:get_prefix(Context, Name)
                    end;
                % we have a vocab: prepend the name with it
                _ -> iolist_to_binary([Vocab, Name])
            end;
        % anything else: error
        _ ->
            throw([bad_property, Key])
    end.
