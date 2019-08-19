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
    get_map_context/1,
    return_context/1
]).


-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-include_lib("couch_eval.hrl").


-record(client, {
    pid,
    from,
    ctx_id,
    opts
}).


-define(CONTEXTS, couch_eval_contexts).
-define(ACTIVE, couch_eval_active).
-define(WAITERS, couch_eval_waiters).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_map_context(CtxOpts) ->
    gen_server:call(?MODULE, {get_map_context, CtxOpts}, infinity).


return_context(Ctx) ->
    gen_server:call(?MODULE, {return_context, Ctx}).


init([]) ->
    process_flag(trap_exit, true),
    TableOpts = [public, named_table, ordered_set],
    ets:new(?CONTEXTS, [{keypos, #ctx.id} | TableOpts]),
    ets:new(?ACTIVE, [{keypos, #client.pid} | TableOpts]),
    ets:new(?WAITERS, [{keypos, #client.pid} | TableOpts]),
    {ok, #{}}.


terminate(_Reason, _State) ->
    ok.


handle_call({get_map_context, CtxOpts}, From, State) ->
    get_map_context(From, CtxOpts),
    {noreply, State};

handle_call({return_context, Ctx}, From, State) ->
    return_context(From, Ctx),
    assign_next_client(Ctx),
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(shutdown, State) ->
    {stop, shutdown, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    couch_log:info("~p ~p died ~p", [?MODULE, Pid, Reason]),
    cleanup_down_client(Pid),
    {noreply, State};

handle_info({'DOWN', _Ref, _, Pid, _Reason}, State) ->
    cleanup_down_client(Pid),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% Internal functions

get_map_context(From, CtxOpts) ->
    CtxId = get_ctx_id(CtxOpts),

    Client = add_waiting(From, CtxId, CtxOpts),

    case ets:lookup(?CONTEXTS, CtxId) of
        [] ->
            Ctx = create_context(CtxOpts, CtxId),
            assign_context(Client, Ctx);
        [#ctx{state = idle} = Ctx] ->
            assign_context(Client, Ctx);
        [#ctx{state = active}] ->
            ok
    end.


create_context(CtxOpts, CtxId) ->
    #{
        language := Language,
        sig := Sig,
        lib := Lib,
        map_funs := MapFuns,
        api_mod := ApiMod
    } = CtxOpts,

    {ok, EvalCtx} = ApiMod:create_map_context(CtxId, Language,
        Sig, Lib, MapFuns),

    Ctx = #ctx{
        id = CtxId,
        state = idle,
        eval_ctx = EvalCtx,
        opts = CtxOpts
    },

    ets:insert(?CONTEXTS, Ctx),
    Ctx.


get_or_create_context(CtxId, CtxOpts) ->
    case ets:lookup(?CONTEXTS, CtxId) of
        [] ->
            create_context(CtxOpts, CtxId);
        [FoundCtx] ->
            FoundCtx
    end.


destroy_context(CtxId) ->
    case ets:lookup(?CONTEXTS, CtxId) of
        [] ->
            ok;
        [Ctx] ->
            #ctx{
                opts = CtxOpts,
                eval_ctx = EvalCtx
            } = Ctx,
            #{
                api_mod := ApiMod
            } = CtxOpts,
            ok = ApiMod:destroy_context(EvalCtx),
            ets:delete(?CONTEXTS, CtxId)
    end.


return_context(From, ClientCtx) ->
    {Pid, _} = From,
    #{id := CtxId} = ClientCtx,

    ets:update_element(?CONTEXTS, CtxId, {#ctx.state, idle}),
    ets:delete(?ACTIVE, Pid),
    ok.


assign_context(#client{} = Client, #ctx{} = Ctx) ->
    #ctx{
        id = CtxId
    } = Ctx,

    #client{
        pid = Pid,
        from = From
    } = Client,

    true = ets:delete(?WAITERS, Pid),
    true = ets:insert(?ACTIVE, Client),

    ets:update_element(?CONTEXTS, CtxId, {#ctx.state, active}),

    #{api_mod := ApiMod} = Ctx#ctx.opts,

    ClientCtx = #{
        ctx => Ctx#ctx.eval_ctx,
        id => Ctx#ctx.id,
        api_mod => ApiMod
    },

    gen_server:reply(From, {ok, ClientCtx}).


add_waiting(From, CtxId, CtxOpts) ->
    {Pid, _} = From,
    erlang:monitor(process, Pid),

    Client = #client{
        pid = Pid,
        from = From,
        ctx_id = CtxId,
        opts = CtxOpts
    },

    ets:insert(?WAITERS, Client),
    Client.


cleanup_down_client(Pid) when is_pid(Pid) ->
    case ets:lookup(?ACTIVE, Pid) of
        [] ->
            ets:delete(?WAITERS, Pid);
        [Active] ->
            #client{
                ctx_id = CtxId
            }  = Active,
            ets:delete(?ACTIVE, Pid),
            destroy_context(CtxId),
            assign_next_client(CtxId)
    end.


assign_next_client(ClientCtx) when is_map(ClientCtx) ->
    #{
        id := CtxId
    } = ClientCtx,
    assign_next_client(CtxId);

assign_next_client(CtxId) ->
    case ets:match_object(?WAITERS, #client{ctx_id = CtxId, _='_'}, 1) of
        '$end_of_table' ->
            ok;
        {[Client], _} ->
            Ctx = get_or_create_context(CtxId, Client#client.opts),
            assign_context(Client, Ctx)
    end.


get_ctx_id(CtxOpts) when is_map(CtxOpts) ->
    #{
        sig := Sig,
        db_name := DbName
    } = CtxOpts,
    get_ctx_id(DbName, Sig).


get_ctx_id(DbName, Sig) ->
    <<DbName/binary, Sig/binary>>.
