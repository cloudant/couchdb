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

-module(couch_stats_resource_tracker).

-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-export([
    inc/1, inc/2,
    maybe_inc/2,
    get_pid_ref/0
]).

-export([
    create_context/0, create_context/1, create_context/3,
    track/1,
    should_track/1
]).

-export([
    active/0
]).

-export([
    make_delta/0
]).

%% Singular increment operations
-export([
    db_opened/0,
    doc_read/0,
    row_read/0,
    change_processed/0,
    ioq_called/0,
    js_evaled/0,
    js_filtered/0,
    js_filtered_error/0,
    js_filtered_doc/0,
    mango_match_evaled/0,
    get_kv_node/0,
    get_kp_node/0
]).

%% Plural increment operations
-export([
    js_filtered_docs/1,
    io_bytes_read/1,
    io_bytes_written/1
]).

-include_lib("couch/include/couch_db.hrl").
-include("couch_stats.hrl").

%% Use these for record upgrades over the wire and in ETS tables
%% TODO: alternatively, just delete these. Currently using a map
%% for shipping deltas over the wire, avoiding much of the
%% problem here. We'll likely still need to handle upgrades to
%% map format over time, so let's decide a course of action here.
-define(RCTX_V1, rctx_v1).
-define(RCTX, ?RCTX_V1).

-define(MANGO_EVAL_MATCH, mango_eval_match).
-define(DB_OPEN_DOC, docs_read).

-define(FRPC_CHANGES_ROW, changes_processed).

%% Module pdict markers
-define(DELTA_TA, csrt_delta_ta).
-define(DELTA_TZ, csrt_delta_tz). %% T Zed instead of T0
-define(PID_REF, csrt_pid_ref). %% track local ID


-record(st, {
    eviction_delay = 10000, %% How many ms dead processes are visible
    tracking = #{} %% track active processes for eventual eviction
}).


%% TODO: switch to:
%% -record(?RCTX, {
-record(rctx, {
    updated_at = os:timestamp(),
    exited_at,
    pid_ref,
    mfa,
    nonce,
    from,
    type = unknown, %% unknown/background/system/rpc/coordinator/fabric_rpc/etc_rpc/etc
    db_open = 0,
    docs_read = 0,
    rows_read = 0,
    changes_processed = 0,
    ioq_calls = 0,
    io_bytes_read = 0,
    io_bytes_written = 0,
    js_evals = 0,
    js_filter = 0,
    js_filter_error = 0,
    js_filtered_docs = 0,
    mango_eval_match = 0,
    get_kv_node = 0,
    get_kp_node = 0,
    state = alive
}).

db_opened() -> inc(db_open).
doc_read() -> inc(docs_read).
row_read() -> inc(rows_read).
change_processed() -> inc(changes_processed).
ioq_called() -> inc(ioq_calls).
js_evaled() -> inc(js_evals).
js_filtered() -> inc(js_filter).
js_filtered_error() -> inc(js_filter_error).
js_filtered_doc() -> inc(js_filtered_docs).
mango_match_evaled() -> inc(mango_eval_match).
get_kv_node() -> inc(get_kv_node).
get_kp_node() -> inc(get_kp_node).

js_filtered_docs(N) -> inc(js_filtered_docs, N).
io_bytes_read(N) -> inc(io_bytes_read, N).
io_bytes_written(N) -> inc(io_bytes_written, N).

inc(db_open) ->
    inc(db_open, 1);
inc(docs_read) ->
    inc(docs_read, 1);
inc(rows_read) ->
    inc(rows_read, 1);
inc(changes_processed) ->
    inc(changes_processed, 1);
inc(ioq_calls) ->
    inc(ioq_calls, 1);
inc(io_bytes_read) ->
    inc(io_bytes_read, 1);
inc(io_bytes_written) ->
    inc(io_bytes_written, 1);
inc(js_evals) ->
    inc(js_evals, 1);
inc(js_filter) ->
    inc(js_filter, 1);
inc(js_filter_error) ->
    inc(js_filter_error, 1);
inc(js_filtered_docs) ->
    inc(js_filtered_docs, 1);
inc(?MANGO_EVAL_MATCH) ->
    inc(?MANGO_EVAL_MATCH, 1);
inc(get_kv_node) ->
    inc(get_kv_node, 1);
inc(get_kp_node) ->
    inc(get_kp_node, 1);
inc(_) ->
    0.


inc(db_open, N) ->
    update_counter(#rctx.db_open, N);
inc(rows_read, N) ->
    update_counter(#rctx.rows_read, N);
inc(ioq_calls, N) ->
    update_counter(#rctx.ioq_calls, N);
inc(io_bytes_read, N) ->
    update_counter(#rctx.io_bytes_read, N);
inc(io_bytes_written, N) ->
    update_counter(#rctx.io_bytes_written, N);
inc(js_evals, N) ->
    update_counter(#rctx.js_evals, N);
inc(js_filter, N) ->
    update_counter(#rctx.js_filter, N);
inc(js_filter_error, N) ->
    update_counter(#rctx.js_filter_error, N);
inc(js_filtered_docs, N) ->
    update_counter(#rctx.js_filtered_docs, N);
inc(?MANGO_EVAL_MATCH, N) ->
    update_counter(#rctx.?MANGO_EVAL_MATCH, N);
inc(?DB_OPEN_DOC, N) ->
    update_counter(#rctx.?DB_OPEN_DOC, N);
inc(?FRPC_CHANGES_ROW, N) ->
    update_counter(#rctx.?FRPC_CHANGES_ROW, N);
inc(get_kv_node, N) ->
    update_counter(#rctx.get_kv_node, N);
inc(get_kp_node, N) ->
    update_counter(#rctx.get_kp_node, N);
inc(_, _) ->
    0.

maybe_inc([mango, evaluate_selector], Val) ->
    inc(?MANGO_EVAL_MATCH, Val);
maybe_inc([couchdb, database_reads], Val) ->
    inc(?DB_OPEN_DOC, Val);
maybe_inc([fabric_rpc, changes, rows_read], Val) ->
    inc(?FRPC_CHANGES_ROW, Val);
maybe_inc(_, _) ->
    0.


%% TODO: update stats_descriptions.cfg for relevant apps
should_track([fabric_rpc, all_docs, spawned]) ->
    true;
should_track([fabric_rpc, changes, spawned]) ->
    true;
should_track([fabric_rpc, map_view, spawned]) ->
    true;
should_track([fabric_rpc, reduce_view, spawned]) ->
    true;
should_track([fabric_rpc, get_all_security, spawned]) ->
    true;
should_track([fabric_rpc, open_doc, spawned]) ->
    true;
should_track([fabric_rpc, update_docs, spawned]) ->
    true;
should_track([fabric_rpc, open_shard, spawned]) ->
    true;
should_track([mango_cursor, view, all_docs]) ->
    true;
should_track([mango_cursor, view, idx]) ->
    true;
should_track(_) ->
    false.

update_counter(Field, Count) ->
    update_counter(get_pid_ref(), Field, Count).


update_counter({_Pid,_Ref}=Key, Field, Count) ->
    ets:update_counter(?MODULE, Key, {Field, Count}, #rctx{pid_ref=Key}).


active() ->
    lists:map(fun to_json/1, ets:tab2list(?MODULE)).


to_json(#rctx{}=Rctx) ->
    #rctx{
        updated_at = TP,
        pid_ref = {_Pid, _Ref} = PidRef,
        mfa = MFA0,
        nonce = Nonce0,
        from = From0,
        docs_read = DocsRead,
        rows_read = RowsRead,
        state = State0,
        type = Type,
        changes_processed = ChangesProcessed
    } = Rctx,
    %%io:format("TO_JSON_MFA: ~p~n", [MFA0]),
    MFA = case MFA0 of
        {M, F, A} ->
            [M, F, A];
        undefined ->
            null;
        Other ->
            throw({error, {unexpected, Other}})
    end,
    From = case From0 of
        {Parent, ParentRef} ->
            [pid_to_list(Parent), ref_to_list(ParentRef)];
        undefined ->
            null
    end,
    State = case State0 of
        alive ->
            alive;
        {down, Reason} when is_atom(Reason) ->
            [down, Reason];
        Unknown ->
            [unknown, io_lib:format("~w", [Unknown])]
    end,
    Nonce = case Nonce0 of
        undefined ->
            null;
        Nonce0 ->
            list_to_binary(Nonce0)
    end,
    #{
        updated_at => term_to_json(TP),
        %%pid_ref => [pid_to_list(Pid), ref_to_list(Ref)],
        pid_ref => term_to_json(PidRef),
        mfa => term_to_json(MFA),
        nonce => term_to_json(Nonce),
        %%from => From,
        from => term_to_json(From),
        docs_read => DocsRead,
        rows_read => RowsRead,
        state => State,
        type => term_to_json(Type),
        changes_processed => ChangesProcessed
    }.

term_to_json({Pid, Ref}) when is_pid(Pid), is_reference(Ref) ->
    [?l2b(pid_to_list(Pid)), ?l2b(ref_to_list(Ref))];
term_to_json({A, B, C}) ->
    [A, B, C];
term_to_json(undefined) ->
    null;
term_to_json(T) ->
    T.

term_to_flat_json(Tuple) when is_tuple(Tuple) ->
    ?l2b(io_lib:format("~w", [Tuple]));
term_to_flat_json(undefined) ->
    null;
term_to_flat_json(T) ->
    T.

to_flat_json(#rctx{}=Rctx) ->
    #rctx{
        updated_at = TP,
        pid_ref = {_Pid, _Ref} = PidRef,
        mfa = MFA0,
        nonce = Nonce0,
        from = From0,
        docs_read = DocsRead,
        rows_read = RowsRead,
        state = State0,
        type = Type,
        changes_processed = ChangesProcessed
    } = Rctx,
    io:format("TO_JSON_MFA: ~p~n", [MFA0]),
    MFA = case MFA0 of
        {_M, _F, _A} ->
            ?l2b(io_lib:format("~w", [MFA0]));
        undefined ->
            null;
        Other ->
            throw({error, {unexpected, Other}})
    end,
    From = case From0 of
        {_Parent, _ParentRef} ->
            ?l2b(io_lib:format("~w", [From0]));
        undefined ->
            null
    end,
    State = case State0 of
        alive ->
            alive;
        State0 ->
            ?l2b(io_lib:format("~w", [State0]))
    end,
    Nonce = case Nonce0 of
        undefined ->
            null;
        Nonce0 ->
            list_to_binary(Nonce0)
    end,
    #{
        %%updated_at => ?l2b(io_lib:format("~w", [TP])),
        updated_at => term_to_flat_json(TP),
        %%pid_ref => [pid_to_list(Pid), ref_to_list(Ref)],
        pid_ref => ?l2b(io_lib:format("~w", [PidRef])),
        mfa => MFA,
        nonce => Nonce,
        from => From,
        docs_read => DocsRead,
        rows_read => RowsRead,
        state => State,
        type => term_to_flat_json(Type),
        changes_processed => ChangesProcessed
    }.

get_pid_ref() ->
    case get(?PID_REF) of
        undefined ->
            Ref = make_ref(),
            set_pid_ref({self(), Ref});
        PidRef ->
            PidRef
    end.


create_context() ->
    create_context(self()).


create_context(Pid) ->
    Ref = make_ref(),
    Rctx = make_record(Pid, Ref),
    track(Rctx),
    ets:insert(?MODULE, Rctx),
    Rctx.

%% add type to disnguish coordinator vs rpc_worker
create_context(From, {M,F,_A} = MFA, Nonce) ->
    io:format("CREAT_CONTEXT MFA[~p]: {~p}: ~p~n", [From, MFA, Nonce]),
    Ref = make_ref(),
    %%Rctx = make_record(self(), Ref),
    %% TODO: extract user_ctx and db/shard from 
    Rctx = #rctx{
        pid_ref = {self(), Ref},
        from = From,
        mfa = MFA,
        type = {worker, M, F},
        nonce = Nonce
    },
    track(Rctx),
    erlang:put(?DELTA_TZ, Rctx),
    ets:insert(?MODULE, Rctx),
    Rctx.

track(#rctx{}=Rctx) ->
    %% TODO: should this block or not? If no, what cleans up zombies?
    %% gen_server:call(?MODULE, {track, PR}).
    gen_server:cast(?MODULE, {track, Rctx}).


make_delta() ->
    TA = case get(?DELTA_TA) of
        undefined ->
            %% Need to handle this better, can't just make a new T0 at T' as
            %% the timestamps will be identical causing a divide by zero error.
            %%
            %% Realistically need to ensure that all invocations of database
            %% operations sets T0 appropriately. Perhaps it's possible to do
            %% this is the couch_db:open chain, and then similarly, in
            %% couch_server, and uhhhh... couch_file, and...
            %%
            %% I think we need some type of approach for establishing a T0 that
            %% doesn't result in outrageous deltas. For now zero out the
            %% microseconds field, or subtract a second on the off chance that
            %% microseconds is zero. I'm not uptodate on the latest Erlang time
            %% libraries and don't remember how to easily get an
            %% `os:timestamp()` out of now() - 100ms or some such.
            %%
            %% I think it's unavoidable that we'll have some codepaths that do
            %% not properly instantiate the T0 at spawn resulting in needing to
            %% do some time of "time warp" or ignoring the timing collection
            %% entirely. Perhaps if we hoisted out the stats collection into
            %% the primary flow of the database and funnel that through all the
            %% function clauses we could then utilize Dialyzer to statically
            %% analyze and assert all code paths that invoke database
            %% operations have properly instantinated a T0 at the appropriate
            %% start time such that we don't have to "fudge" deltas with a
            %% missing start point, but we're a long ways from that happening
            %% so I feel it necessary to address the NULL start time.

            %% Track how often we fail to initiate T0 correctly
            %% Perhaps somewhat naughty we're incrementing stats from within
            %% couch_stats itself? Might need to handle this differently
            %% TODO: determine appropriate course of action here
            couch_stats:increment_counter(
                [couchdb, csrt, delta_missing_t0]),
                %%[couch_stats_resource_tracker, delta_missing_t0]),

            case erlang:get(?DELTA_TZ) of
                undefined ->
                    TA0 = make_delta_base(),
                    %% TODO: handline missing deltas, otherwise divide by zero
                    set_delta_a(TA0),
                    TA0;
                TA0 ->
                    TA0
            end;
        %%?RCTX{} = TA0 ->
        #rctx{} = TA0 ->
            TA0
    end,
    TB = get_resource(),
    make_delta(TA, TB).


make_delta(#rctx{}=TA, #rctx{}=TB) ->
    Delta = #{
        docs_read => TB#rctx.docs_read - TA#rctx.docs_read,
        rows_read => TB#rctx.rows_read - TA#rctx.rows_read,
        changes_processed => TB#rctx.changes_processed - TA#rctx.changes_processed,
        dt => timer:now_diff(TB#rctx.updated_at, TA#rctx.updated_at)
    },
    %% TODO: reevaluate this decision
    %% Only return non zero (and also positive) delta fields
    maps:filter(fun(_K,V) -> V > 0 end, Delta);
make_delta(_, #rctx{}) ->
    #{error => missing_beg_rctx};
make_delta(#rctx{}, _) ->
    #{error => missing_fin_rctx}.

make_delta_base() ->
    Ref = make_ref(),
    %%Rctx = make_record(self(), Ref),
    %% TODO: extract user_ctx and db/shard from request
    TA0 = #rctx{
        pid_ref = {self(), Ref}
    },
    case TA0#rctx.updated_at of
        {Me, S, Mi} when Mi > 0 ->
            TA0#rctx{updated_at = {Me, S, 0}};
        {Me, S, Mi} when S > 0 ->
            TA0#rctx{updated_at = {Me, S - 1, Mi}}
    end.

set_delta_a(TA) ->
    erlang:put(?DELTA_TA, TA).

set_pid_ref(PidRef) ->
    erlang:put(?PID_REF, PidRef),
    PidRef.

get_resource() ->
    get_resource(get_pid_ref()).

get_resource(PidRef) ->
    case ets:lookup(?MODULE, PidRef) of
        [#rctx{}=TP] ->
            TP;
        [] ->
            undefined
    end.

make_record(Pid, Ref) ->
    #rctx{pid_ref = {Pid, Ref}}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(?MODULE, [
        named_table,
        public,
        {decentralized_counters, true}, %% TODO: test impact of this
        {write_concurrency, true},
        {read_concurrency, true},
        {keypos, #rctx.pid_ref}
    ]),
    {ok, #st{}}.

handle_call(fetch, _from, #st{} = St) ->
    {reply, {ok, St}, St};
handle_call({track, _}, _From, St) ->
    {reply, ok, St};
handle_call(Msg, _From, St) ->
    {stop, {unknown_call, Msg}, error, St}.

handle_cast({track, #rctx{pid_ref={Pid,_}=PidRef}}, #st{tracking=AT0} = St0) ->
    St = case maps:is_key(PidRef, AT0) of
        true -> %% noop, we're already tracking this PidRef
            St0;
        false -> %% setup new monitor and double bookkeep refs
            Mon = erlang:monitor(process, Pid),
            AT = maps:put(Mon, PidRef, maps:put(PidRef, Mon, AT0)),
            St0#st{tracking=AT}
    end,
    {noreply, St};
handle_cast(Msg, St) ->
    {stop, {unknown_cast, Msg}, St}.

handle_info({'DOWN', MonRef, Type, DPid, Reason}, #st{tracking=AT0} = St0) ->
    io:format("CSRT:HI(~p)~n", [{'DOWN', MonRef, Type, DPid, Reason}]),
    St = case maps:get(MonRef, AT0, undefined) of
        undefined ->
            io:format("ERROR: UNEXPECTED MISSING MONITOR IN TRACKING TABLE: {~p, ~p}~n", [MonRef, DPid]),
            St0;
        {RPid, _Ref} = PidRef ->
            if
                RPid =:= DPid -> ok;
                true -> erlang:halt(io_lib:format("CSRT:HI PID MISMATCH ABORT: ~p =/= ~p~n", [DPid, RPid]))
            end,
            %% remove double bookkeeping
            AT = maps:remove(MonRef, maps:remove(PidRef, AT0)),
            %% TODO: Assert Pid matches Object
            %% update process state in live table
            %% TODO: decide whether we want the true match to crash this process on failure
            true = ets:update_element(?MODULE, PidRef,
                [{#rctx.state, {down, Reason}}, {#rctx.updated_at, os:timestamp()}]),
            log_process_lifetime_report(PidRef),
            %% Delay eviction to allow human visibility on short lived pids
            erlang:send_after(St0#st.eviction_delay, self(), {evict, PidRef}),
            St0#st{tracking=AT}
    end,
    {noreply, St};
handle_info({evict, {_Pid, _Ref}=PidRef}, #st{}=St) ->
    ets:delete(?MODULE, PidRef),
    {noreply, St};
handle_info(Msg, St) ->
    {stop, {unknown_info, Msg}, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

log_process_lifetime_report(PidRef) ->
    %% More safely assert this can't ever be undefined
    #rctx{} = Rctx = get_resource(PidRef),
    %% TODO: catch error out of here, report crashes on depth>1 json
    couch_log:report("csrt-pid-cost-lifetime", to_flat_json(Rctx)).
