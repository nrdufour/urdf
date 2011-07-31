
% This file is part of erlang-json-ld released under the MIT license.
% See the LICENSE file for more information.

-record(triple, {
    type     :: resource | literal,
    subject  :: binary(),
    object   :: binary(),
    property :: binary()
}).

-type triple() :: #triple{}.
