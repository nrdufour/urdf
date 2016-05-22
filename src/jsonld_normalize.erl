
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

-record(state, {
    nodes,
    node_counter = 0
}).

do_it(Doc) when is_list(Doc) ->
    RootObjects = prepare_list(Doc),

    NormalizeThis = fun(Object, State) ->
        normalize_object(Object, State)
    end,

    InitialState = create_initial_state(),

    FinalState = lists:foldl(
        NormalizeThis,
        InitialState,
        RootObjects
    ),

    dict:fold(
        fun(_Key, Value, Acc) ->
            Acc ++ [Value]
        end,
        [],
        FinalState#state.nodes
    ).

% --- Internal API ---

create_initial_state() ->
    #state{ nodes = dict:new(), node_counter = 0 }.

prepare_list(Doc) ->
    case ?IS_OBJECT(Doc) of
        true  ->
            [Doc];
        false ->
            Doc
    end.

normalize_object(Object, State) ->

    % First create a JSON-LD Context
    JsonldCtx = jsonld_context:create_default(),

    % then extract any context in the current object
    ObjectCtx = jsonld_context:process_local_context(Object, JsonldCtx),

    % extract the subject if it exists, otherwise use a bnode
    Subject = process_subject(Object, ObjectCtx),

    % Create the normalized object
    extract_normalized_objects(Subject, Object, ObjectCtx, State).

process_subject(JsonObject, _ObjectCtx) ->
    % Subject
    LocalSubjectProp = ?HAS_VALUE(JsonObject, ?SUBJECT_KEY),
    case LocalSubjectProp of
        {_, SubjectValue} ->
            SubjectValue;
        false ->
            urdf_util:new_bnode()
    end.

extract_normalized_objects(Subject, Object, ObjectCtx, State) ->
    ObjectWithSubject = [ { <<"@subject">>, [ { <<"@iri">>, Subject } ] } ],

    NormalizedObject = lists:foldl(
        fun({Key, Value}, ObjectBeingBuilt) ->
            case Key of
                <<"@subject">> ->
                    ObjectBeingBuilt;
                <<"@context">> ->
                    ObjectBeingBuilt;
                _ ->
                    Property = {
                        extract_property_key(Key, ObjectCtx),
                        extract_property_value(Value, ObjectCtx)
                    },
                    ObjectBeingBuilt ++ [Property]
            end
        end,
        ObjectWithSubject,
        Object
    ),

    % Append the object to the list
    NodeId = Subject,
    UpdatedList = dict:store(NodeId, NormalizedObject, State#state.nodes),

    State#state{ nodes = UpdatedList }.
    
extract_property_key(Key, _ObjectCtx) ->
    Key.

extract_property_value(Value, _ObjectCtx) ->
    Value.

