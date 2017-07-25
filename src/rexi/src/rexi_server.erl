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

-module(rexi_server).
-behaviour(gen_server).
-vsn(1).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([start_link/1, init_p/5]).


-record(job, {
    client_ref::reference(),
    client_pid::pid(),
    worker_pid::pid()
}).

-record(st, {
    clients = ets:new(clients, [
            public,
            {write_concurrency, true},
            {keypos, #job.client_ref}
        ]),
    workers = ets:new(workers, [
            public,
            {write_concurrency, true},
            {keypos, #job.worker_pid}
        ])
}).

start_link(ServerId) ->
    gen_server:start_link({local, ServerId}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #st{}}.


handle_call(_Request, _From, St) ->
    {reply, ignored, St}.


handle_cast({doit, From, MFA}, St) ->
    handle_cast({doit, From, undefined, MFA}, St);

handle_cast({doit, {ClientPid, ClientRef} = From, Nonce, MFA}, State) ->
    LocalPid = spawn_link(?MODULE, init_p, [self(), State, From, MFA, Nonce]),
    add_job(State, #job{
        client_ref = ClientRef,
        client_pid = ClientPid,
        worker_pid = LocalPid
    }),
    {noreply, State};


handle_cast({kill, ClientRef}, #st{clients = Clients} = St) ->
    case ets:lookup(Clients, ClientRef) of
        [#job{worker_pid = Pid} = Job] ->
            erlang:unlink(Pid),
            exit(Pid, kill),
            {noreply, remove_job(St, Job)};
        [] ->
            {noreply, St}
    end;

handle_cast(_, St) ->
    couch_log:notice("rexi_server ignored_cast", []),
    {noreply, St}.

handle_info({'EXIT', Pid, Error}, #st{workers = Workers} = St) ->
    case ets:lookup(Workers, Pid) of
        [#job{client_pid = CPid, client_ref = CRef} = Job] ->
            notify_caller({CPid, CRef}, Error),
            {noreply, remove_job(St, Job)};
        [] ->
            {noreply, St}
    end;

handle_info(_Info, St) ->
    {noreply, St}.

terminate(_Reason, St) ->
    ets:foldl(fun(#job{worker_pid = Pid},_) ->
        exit(Pid,kill)
    end, nil, St#st.workers),
    ok.

code_change(_OldVsn, #st{}=State, _Extra) ->
    {ok, State}.

%% @doc initializes a process started by rexi_server.
-spec init_p(pid(), #st{}, {pid(), reference()}, {atom(), atom(), list()},
    string() | undefined) -> any().
init_p(Parent, State, From, {M,F,A}, Nonce) ->
    put(rexi_from, From),
    put('$initial_call', {M,F,length(A)}),
    put(nonce, Nonce),
    try
        apply(M, F, A)
    catch
        exit:normal ->
            ok;
        Class:Reason ->
            Stack = clean_stack(),
            Args = [M, F, length(A), Nonce, Class, Reason, Stack],
            couch_log:error("rexi_server ~s:~s/~b - ~p - :: ~p:~p ~100p", Args),
            notify_caller(From, {Reason, Stack})
    end,
    remove_worker(State, self()),
    unlink(Parent).

%% internal

clean_stack() ->
    lists:map(fun({M,F,A}) when is_list(A) -> {M,F,length(A)}; (X) -> X end,
        erlang:get_stacktrace()).

add_job(#st{workers = Workers, clients = Clients} = State, Job) ->
    ets:insert(Workers, Job),
    ets:insert(Clients, Job),
    State.

remove_job(#st{workers = Workers, clients = Clients} = State, Job) ->
    ets:delete_object(Workers, Job),
    ets:delete_object(Clients, Job),
    State.

remove_worker(#st{workers = Workers} = State, Pid) ->
    case ets:lookup(Workers, Pid) of
        [Job] ->
            remove_job(State, Job);
        [] ->
            State
    end.

notify_caller({Caller, Ref}, Reason) ->
    rexi_utils:send(Caller, {Ref, {rexi_EXIT, Reason}}).
