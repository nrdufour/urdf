
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

    % First create a JSON-LD Context
    JsonldCtx = jsonld_context:create_default(),

    % then extract any context in the current object
    ObjectCtx = jsonld_context:process_local_context(Object, JsonldCtx),

    % extract the subject if it exists, otherwise use a bnode
    Subject = process_subject(Object, ObjectCtx),

    % Finally get all the other properties
    Properties = process_properties(Object, ObjectCtx),

    % Create the normalized object
    NormalizedObject = create_normalized_object(Subject, Properties, ObjectCtx),

    % Append the object to the list
    UpdatedList = lists:merge(Ctx#ctx.objects, [NormalizedObject]),

    Ctx#ctx{ objects = UpdatedList }.

process_subject(JsonObject, ObjectCtx) ->
    % Subject
    LocalSubjectProp = ?HAS_VALUE(JsonObject, ?SUBJECT_KEY),
    case LocalSubjectProp of
        {_, SubjectValue} ->
            SubjectValue;
        false ->
            urdf_util:new_bnode()
    end.

process_properties(Object, ObjectCtx) ->
    [].

create_normalized_object(Subject, Properties, _ObjectCtx) ->
    ObjectWithSubject = [ { <<"@subject">>, [ { <<"@iri">>, Subject } ] } ],

    ProcessProperty = fun({ Name, Value }, Norm) ->
        Norm
    end,

    lists:foldl(
        ProcessProperty,
        ObjectWithSubject,
        Properties
    ).
