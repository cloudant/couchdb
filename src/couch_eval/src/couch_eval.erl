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


-module(couch_eval).


-export([
    get_map_context/6,
    return_map_context/1,
    map_docs/2
]).


-include("couch_eval.hrl").


-spec get_map_context(db_name(), ddoc_id(), language(), sig(), lib(),
    [map_fun()]) -> {ok, context()} | error().
get_map_context(DbName, DDocId, Language, Sig, Lib, MapFuns) ->
    ApiMod = get_api_mod(Language),
    CtxOpts = #{
        db_name => DbName,
        ddoc_id => DDocId,
        language => Language,
        sig => Sig,
        lib => Lib,
        map_funs => MapFuns,
        api_mod => ApiMod
    },
    couch_eval_server:get_map_context(CtxOpts).


-spec return_map_context(context()) -> ok | error().
return_map_context(Ctx) ->
    couch_eval_server:return_map_context(Ctx).


-spec map_docs(context(), [doc()]) -> {ok, result()} | error().
map_docs(Ctx, Docs) ->
    #ctx{
        eval_ctx = EvalCtx,
        opts = Opts
    } = Ctx,

    #{api_mod := ApiMod} = Opts,
    ApiMod:map_docs(EvalCtx, Docs).


get_api_mod(Language) when is_binary(Language) ->
    try
        ModStr = config:get("couch_eval", "languages", Language),
        list_to_existing_atom(ModStr)
    catch error:badarg ->
        erlang:error({invalid_eval_api_mod, Language})
    end.
