% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.


-include_lib("couch/include/couch_db.hrl").


-type error() :: {error, binary()}.
-type db_name() :: binary().
-type ddoc_id() :: binary().
-type language() :: binary().
-type sig() :: binary().
-type lib() :: binary().
-type map_fun() :: #{id := binary(), def := binary()}.
-type map_funs() :: [map_funs()].
-type result() :: binary().
-type api_mod() :: atom().
-type context_id() :: term().
-type context() :: #{
    language := language(),
    sig := sig(),
    context_id := context_id()
}.
-type context_opts() :: #{
    db_name := db_name(),
    ddoc_id => ddoc_id(),
    language => language(),
    sig => sig(),
    lib => lib(),
    map_funs => map_funs(),
    api_mod => api_mod()
}.


-record(ctx, {
    id :: context_id(),
    state :: idle | active,
    eval_ctx :: context(),
    opts :: context_opts()
}).