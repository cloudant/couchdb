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
-module(couch_eval_server).


-behaviour(gen_server).


-export([
    start_link/0,
    get_map_context/6,
    return_map_context/1
]).


-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).


-define(CONTEXTS, couch_eval_contexts).
-define(CLIENTS, couch_eval_clients).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_map_context(DbName, DDocId, Language, Sig, Lib, MapFuns) ->
    CtxOpts = #{
        db_name => DbName,
        ddoc_id => DDocId,
        language => Language,
        sig => Sig,
        lib => Lib,
        map_funs => MapFuns
    },
    gen_server:call(?MODULE, {get_map_context, CtxOpts}).


return_map_context(Ctx) ->
    gen_server:call(?MODULE, {return_map_context, Ctx}).


init([]) ->
    process_flag(trap_exit, true),
    TableOpts = [public, named_table, ordered_set],
    ets:new(?CONTEXTS, TableOpts),
    ets:new(?CLIENTS, TableOpts),
    {ok, #{}}.


handle_call({get_map_context, CtxOpts}, From, State) ->
    get_map_context_for_client(From, CtxOpts),
    {noreply, State};

handle_call({return_map_context, Ctx}, From, State) ->
    return_context(From, Ctx),
    State1 = find_next_client_for_context(Ctx, State),
    {noreply, State1}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(shutdown, State) ->
    {stop, shutdown, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    couch_log:info("~p ~p died ~p", [?MODULE, Pid, Reason]),
    State1 = cleanup_down_client(Pid, State),
    {noreply, State1};

handle_info({'DOWN', Ref, _, _, _Reason}, State0) ->
    State1 = cleanup_down_client(Ref, State0),
    {noreply, State1};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% Internal functions

get_map_context_for_client(Client, CtxOpts) ->
    CtxId = get_ctx_id(CtxOpts),
    add_waiting_client(Client, CtxId, CtxOpts),

    case ets:lookup(?CONTEXTS, CtxId) of
        [] ->
            EvalCtx = create_context(CtxOpts, CtxId),
            give_context_to_client(Client, EvalCtx);
        [{CtxId, waiting, EvalCtx, _}] ->
            give_context_to_client(Client, EvalCtx);
        [{CtxId, active, _, _}] ->
            ok
    end.


create_context(CtxOpts, CtxId) ->
    #{
        db_name => DbName,
        ddoc_id => DDocId,
        language => Language,
        sig => Sig,
        lib => Lib,
        map_funs => MapFuns
    } = CtxOpts,

    ApiMod = get_api_mod(Language),
    Ctx = ApiMod:create_map_context(DbName, DDocId, Language, Sig, Lib, MapFuns),
    EvalCtx = {ApiMod, Ctx},
    ets:insert(?CONTEXTS, {CtxId, waiting, EvalCtx, CtxOpts}),
    EvalCtx.


get_api_mod(Language) when is_binary(Language) ->
    try
        ModStr = config:get("couch_eval", "languages", Language),
        list_to_existing_atom(ModStr)
    catch error:badarg ->
        erlang:error({invalid_eval_api_mod, ModStr})
    end.


destroy_context(EvalCtx) ->
    {ApiMod, Ctx} = EvalCtx,
    #{
        context_id := CtxId
    } = Ctx,

    ApiMod:destroy_context(Ctx),

    CtxOpts = ets:lookup_element(?CONTEXTS, CtxId, 4),
    ets:delete(?CONTEXTS, CtxId),

    Client = get_next_client(CtxId),

    if Client == none -> ok; true ->
        Ctx = create_context(CtxOpts, CtxId),
        give_context_to_client(Client, Ctx)
    end.


return_context(Client, EvalCtx) ->
    {ApiMod, Ctx} = EvalCtx,

    #{
        context_id := CtxId
    } = Ctx,

    ApiMod:return_context(Ctx),
    ets:update_element(?CONTEXTS, CtxId, {2, waiting}),
    ets:delete(?CLIENTS, Client),
    ok.


give_context_to_client(Client, EvalCtx) when is_tuple(Client) ->
    {Pid, _, _, _} = Client,
    give_context_to_client(Pid, EvalCtx);

give_context_to_client(Client, EvalCtx) ->
    {_, Ctx} = EvalCtx,

    #{
        context_id := CtxId
    } = Ctx,

    ets:update_element(?CONTEXTS, CtxId, {2, active}),
    ets:update_element(?CLIENTS, Client, {4, EvalCtx}),
    gen_server:reply(Client, {ok, EvalCtx}).


add_waiting_client(Client, CtxId, CtxOpts) ->
    ClientRef = erlang:monitor(process, Client),
    ets:insert(?CLIENTS, {Client, CtxId, CtxOpts, waiting, ClientRef}).


cleanup_down_client(Ref, State) when is_reference(Ref) ->
    Client = ets:match_object(?CLIENTS, {'_', '_', '_', '_', Ref}),
    cleanup_down_client(Client, State);

cleanup_down_client(Pid, State) when is_pid(Pid) ->
    Client = ets:lookup(?CLIENTS, Pid),
    cleanup_down_client(Client, State);

cleanup_down_client([], State) ->
    State;

cleanup_down_client([{Pid, _, _, EvalCtx, _}], State) ->
    ets:delete(?CLIENTS, Pid),

    if EvalCtx == waiting -> ok; true ->
        destroy_context(EvalCtx)
    end,
    State.


find_next_client_for_context(EvalCtx, State) ->
    {_, Ctx} = EvalCtx,

    #{
        context_id := CtxId
    } = Ctx,

    Client = get_next_client(CtxId),

    if Client == none -> State; true ->
        give_context_to_client(Client, EvalCtx)
    end,
    State.


get_next_client(CtxId) ->
    case ets:match_object(?CLIENTS, {'_', CtxId, waiting, '_', '_'}, 1) of
        '$end_of_table' ->
            none;
        {[Client], _} ->
            Client
    end.


get_ctx_id(CtxOpts) when is_map(CtxOpts) ->
    #{
        sig := Sig,
        db_name := DbName
    } = CtxOpts,
    get_ctx_id(DbName, Sig).

get_ctx_id(DbName, Sig) ->
    <<DbName/binary, Sig/binary>>.


