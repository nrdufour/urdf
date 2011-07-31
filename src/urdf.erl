
% This file is part of urdf released under the MIT license.
% See the LICENSE file for more information.

-module(urdf).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([j2t/1, t2j/1, n2t/1, t2n/1, r2t/1, t2r/1]).

-include("triples.hrl").

j2t(JsonLD) ->
    urdf_j2t:convert(JsonLD).

t2j(Triples) ->
    urdf_t2j:convert(Triples).

n2t(N3) ->
    urdf_n2t:convert(N3).

t2n(Triples) ->
    urdf_t2n:convert(Triples).

r2t(RDF) ->
    urdf_r2t:convert(RDF).

t2r(Triples) ->
    urdf_t2r:convert(Triples).
