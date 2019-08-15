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


-include_lib("couch_eval.hrl").


-export([
    start_link/0,
    get_map_context/1,
    return_context/1
]).


-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).


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


handle_call({get_map_context, CtxOpts}, From, State) ->
    get_map_context(From, CtxOpts),
    {noreply, State};

handle_call({return_context, Ctx}, From, State) ->
    return_context(From, Ctx),
    find_next_client(Ctx),
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(shutdown, State) ->
    {stop, shutdown, State};

% handle_info({'EXIT', Pid, Reason}, State) ->
%     couch_log:info("~p ~p died ~p", [?MODULE, Pid, Reason]),
%     cleanup_down_client(Pid),
%     {noreply, State};

% handle_info({'DOWN', Ref, _, _, _Reason}, State) ->
%     cleanup_down_client(Ref),
%     {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


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

    {ok, EvalCtx} = ApiMod:create_map_context(CtxId, Language, Sig, Lib, MapFuns),

    Ctx = #ctx{
        id = CtxId,
        state = idle,
        eval_ctx = EvalCtx,
        opts = CtxOpts
    },

    ets:insert(?CONTEXTS, Ctx),
    Ctx.


% destroy_context(EvalCtx) ->
%     {ApiMod, Ctx} = EvalCtx,
%     #{
%         context_id := CtxId
%     } = Ctx,

%     ApiMod:destroy_context(Ctx),

%     CtxOpts = ets:lookup_element(?CONTEXTS, CtxId, 4),
%     ets:delete(?CONTEXTS, CtxId),

%     Client = get_next_client(CtxId),

%     if Client == none -> ok; true ->
%         Ctx = create_context(CtxOpts, CtxId),
%         give_context_to_client(Client, Ctx)
%     end.


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
    
    io:format("assigning ~p ~n ~p ~n", [Client, Ctx]),
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


% cleanup_down_client(Ref, State) when is_reference(Ref) ->
%     Client = ets:match_object(?CLIENTS, {'_', '_', '_', '_', Ref}),
%     cleanup_down_client(Client, State);

% cleanup_down_client(Pid, State) when is_pid(Pid) ->
%     Client = ets:lookup(?CLIENTS, Pid),
%     cleanup_down_client(Client, State);

% cleanup_down_client([], State) ->
%     State;

% cleanup_down_client([{Pid, _, _, EvalCtx, _}], State) ->
%     ets:delete(?CLIENTS, Pid),

%     if EvalCtx == waiting -> ok; true ->
%         destroy_context(EvalCtx)
%     end,
%     State.


find_next_client(ClientCtx) ->
    #{
        id := CtxId
    } = ClientCtx,

    case ets:match_object(?WAITERS, #client{ctx_id = CtxId, _='_'}, 1) of
        '$end_of_table' -> 
            ok;
        {[Client], _} ->
            [Ctx] = ets:lookup(?CONTEXTS, CtxId),
            assign_context(Client, Ctx)
    end.

    % Client = get_next_client(CtxId),

    % if Client == none -> State; true ->
    %     give_context_to_client(Client, EvalCtx)
    % end,
    % State.


% get_next_client(CtxId) ->
%     case ets:match_object(?CLIENTS, {'_', CtxId, waiting, '_', '_'}, 1) of
%         '$end_of_table' ->
%             none;
%         {[Client], _} ->
%             Client
%     end.


get_ctx_id(CtxOpts) when is_map(CtxOpts) ->
    #{
        sig := Sig,
        db_name := DbName
    } = CtxOpts,
    get_ctx_id(DbName, Sig).

get_ctx_id(DbName, Sig) ->
    <<DbName/binary, Sig/binary>>.


