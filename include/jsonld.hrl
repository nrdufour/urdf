
% This file is part of urdf released under the MIT license.
% See the LICENSE file for more information.

% Keywords
-define(LOCAL_CONTEXT_KEY, <<"@context">>).
-define(BASE_KEY, <<"@base">>).
-define(REMOTE_CONTEXT_KEY, <<"@profile">>).
-define(VOCAB_KEY, <<"@vocab">>).
-define(COERCE_KEY, <<"@coerce">>).
-define(LITERAL_KEY, <<"@literal">>).
-define(IRI_KEY, <<"@iri">>).
-define(LANGUAGE_KEY, <<"@language">>).
-define(DATATYPE_KEY, <<"@datatype">>).
-define(SUBJECT_KEY, <<"@subject">>).
-define(TYPE_KEY, <<"@type">>).

-define(DEFAULT_CONTEXT,
[
        {<<"rdf">>, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>},
        {<<"xsd">>, <<"http://www.w3.org/2001/XMLSchema#">>},
        {<<"dc">>, <<"http://purl.org/dc/terms/">>},
        {<<"skos">>, <<"http://www.w3.org/2004/02/skos/core#">>},
        {<<"foaf">>, <<"http://xmlns.com/foaf/0.1/">>},
        {<<"sioc">>, <<"http://rdfs.org/sioc/ns#">>},
        {<<"cc">>, <<"http://creativecommons.org/ns#">>},
        {<<"geo">>, <<"http://www.w3.org/2003/01/geo/wgs84_pos#">>},
        {<<"vcard">>, <<"http://www.w3.org/2006/vcard/ns#">>},
        {<<"cal">>, <<"http://www.w3.org/2002/12/cal/ical#">>},
        {<<"doap">>, <<"http://usefulinc.com/ns/doap#">>},
        {<<"Person">>, <<"http://xmlns.com/foaf/0.1/Person">>},
        {<<"name">>, <<"http://xmlns.com/foaf/0.1/name">>},
        {<<"homepage">>, <<"http://xmlns.com/foaf/0.1/homepage">>}
]
).

-define(IS_OBJECT(Obj), urdf_util:is_proplist(Obj)).

-define(HAS_VALUE(Proplist, Key), lists:keyfind(Key, 1, Proplist)).
