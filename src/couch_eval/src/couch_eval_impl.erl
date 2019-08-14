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

-module(couch_eval_impl).


-export([]).


-include("couch_eval.hrl").


-callback create_map_context(language(), sig(), lib(),
    map_funs()) -> {ok, context()} | error().


-callback destroy_context(context()) -> ok | error().


-callback return_context(context()) -> ok | error().


-callback map_docs(context(), docs()) -> {ok, result()} | error().
