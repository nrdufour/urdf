
% This file is part of urdf released under the MIT license.
% See the LICENSE file for more information.

-record(triple, {
    type     :: resource | literal,
    subject  :: binary(),
    object   :: binary(),
    property :: binary()
}).

-type triple() :: #triple{}.
