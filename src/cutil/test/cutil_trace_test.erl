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
-module(cutil_trace_test).

-export([
    init/2,
    event/3
]).

-include_lib("eunit/include/eunit.hrl").
-include("cutil.hrl").

-define(T(Name), {foreachx, fun setup/1, fun teardown/2, [{Name, fun Name/2}]}).
-define(LOC, [
    {module, ?MODULE}, {line, ?LINE}, {function, ?FUNCTION_NAME}, {arity, ?FUNCTION_ARITY}
]).

setup(TestCase) ->
    Collector = spawn(fun() -> loop([]) end),
    PoolPid = cutil_trace:get_pool(TestCase, #{tracer => ?MODULE, test_case => TestCase, collector => Collector}),
    {Collector, PoolPid}.


teardown(TestCase, {Collector, _PoolPid}) ->
    collector(Collector, stop, ?LOC, "Timeout stopping collector"),
    erlang:trace(all, false, [all]),
    erlang:trace_pattern({'_','_','_'}, false, [local,meta,call_count,call_time]),
    erlang:trace_pattern({'_','_','_'}, false, []), % unsets global
    ok = cutil_trace:stop_pool(TestCase),
    ok.


cutil_trace_test_() ->
    {
        "Tracing functionality",
        {
            setup,
            fun() -> application:start(cutil) end,
            fun(_) -> application:stop(cutil) end,
            [
                ?T(should_do_non_filtered_call_trace),
                ?T(should_do_filtered_call_trace)
            ]
        }
    }.


should_do_non_filtered_call_trace(_, {Collector, PoolPid}) ->
    ?_test(begin
        Earliest = erlang:system_time(microsecond),
        Match = [{['$1', '$2'], [], [{return_trace}]}],
        erlang:trace_pattern({lists, seq, 2}, Match, [local]),
        cutil_trace:trace(PoolPid, ?FUNCTION_NAME, trace),
        fut(1),
        fut(2),
        fut(3),
        {ok, Events0} = wait(Collector, 8, ?LOC, "Cannot get all events we expect"),
        Events = lists:keysort(3, Events0),
        ?assert(is_list(Events)),
        ?assertNotEqual([], Events),
        %% check the pid of a process is set in each event
        ?assert(lists:all(fun(#cutil_tracer_event{tracee = Pid}) -> Pid == self() end, Events)),

        %% check timestamp is set sometime after the test start
        Times = [Ts || #cutil_tracer_event{ts = Ts} <- Events],
        ?assert(lists:all(fun(T) ->
            Earliest < T
        end, Times)),


        Striped = lists:map(fun(#cutil_tracer_event{} = Event) ->
            #cutil_tracer_event{tag = Tag, term = Term, extra = Extra} = Event,
            {Tag, Term, Extra}
        end, Events),

        ?assertEqual([
            {call, {lists, seq, "EH"}, nil},
            {return_from, {lists, seq, 2}, "EFGH"},
            {call, {lists, seq, "AD"}, nil},
            {return_from, {lists, seq, 2}, "ABCD"},
            {call, {lists, seq, [1, 3]}, nil},
            {return_from, {lists, seq, 2}, [1, 2, 3]},
            {call, {lists, seq,[3,5]}, nil},
            {return_from, {lists, seq, 2}, [3, 4, 5]},
            {call, {lists, seq, "AD"}, nil},
            {return_from, {lists, seq, 2}, "ABCD"},
            {call, {lists, seq, "IL"}, nil},
            {return_from, {lists, seq, 2}, "IJKL"}

        ], Striped),
        ok
    end).

should_do_filtered_call_trace(_, {Collector, PoolPid}) ->
    ?_test(begin
        Earliest = erlang:system_time(microsecond),
        TriggerMFA = {?MODULE, seq, 1},
        TargetMFA = {lists, seq, 2},
        TargetId = <<"target_id">>,
        TraceId = erlang:md5(term_to_binary(?FUNCTION_NAME)),
        TargetMS = [{['$1', '$2'], [{'andalso',{is_seq_trace},{'<','$1',10}}], [{message, #{
            caller_mfa => {caller},
            seq_token => {get_seq_token},
            target_id => TargetId,
            trace_id => TraceId,
            ms_return => [{'+', '$2', 1}],
            trigger_mfa => {TriggerMFA}
        }}]}],
        TriggerMS = [{['$1'],[],[
            {set_seq_token, label, TraceId},
            {message, #{
                trace_id => TraceId,
                seq_token => {get_seq_token}
            }}
        ]}],
        erlang:trace_pattern(TriggerMFA, TriggerMS, [local]),
        erlang:trace_pattern(TargetMFA, TargetMS, [local]),
        cutil_trace:trace(PoolPid, TraceId, filter),
        fut(1),
        fut(2),
        fut(3),
        {ok, Events0} = wait(Collector, 2, ?LOC, "Cannot get all events we expect"),
        Events = lists:keysort(#cutil_tracer_event.ts, Events0),
        ?assert(is_list(Events)),
        ?assertNotEqual([], Events),
        %% check the pid of a process is set in each event
        ?assert(lists:all(fun(#cutil_tracer_event{tracee = Pid}) -> Pid == self() end, Events)),

        %% check timestamp is set sometime after the test start
        Times = [Ts || #cutil_tracer_event{ts = Ts} <- Events],
        ?assert(lists:all(fun(T) ->
            Earliest < T
        end, Times)),

        %% Check args is nil for filtered traces
        Args = [A || #cutil_tracer_event{args = A} <- Events],
        ?assertEqual([nil], lists:usort(Args)),

        Striped = lists:map(fun(#cutil_tracer_event{} = Event) ->
            #cutil_tracer_event{tag = Tag, term = Term, mspec = #{ms_return := Return}} = Event,
            {Tag, Term, Return}
        end, Events),

        % make sure we don't have events for
        % - [2, 4] which is protected by sensitive flag
        % - [$I, $L] which is not in the scope of the trigger
        ?assertEqual([
            {call, {lists,seq, 2}, [4]},
            {call, {lists,seq, 2}, [6]}
        ], Striped),
        ok
    end).


init(_Idx, #{collector := Collector}) ->
    {ok, Collector}.


event(Event, _Index, Collector) ->
    collector(Collector, {store, Event}, ?LOC, "Timeout storing event"),
    ok.


fut(1) ->
    lists:seq($E, $H),
    ok;
fut(2) ->
    lists:seq($A, $D),
    seq(3),
    lists:seq($A, $D),
    ok;
fut(3) ->
    lists:seq($I, $L),
    ok.


seq(_N) ->
    lists:seq(1, 3),
    process_flag(sensitive, true),
    lists:seq(2, 4),
    process_flag(sensitive, false),
    lists:seq(3, 5),
    ok.


wait(Collector, EventsCount, Loc, ErrorMsg) ->
    wait(Collector, EventsCount, 5, Loc, ErrorMsg).


wait(_Collector, _EventCount, 0, Loc, ErrorMsg) ->
    erlang:error({timeout, [{error, ErrorMsg} | Loc]});

wait(Collector, EventsCount, TryCount, Loc, ErrorMsg) ->
    {ok, Events} = collector(Collector, events, Loc, "Timeout getting events"),
    case length(Events) >= EventsCount of
        true ->
            {ok, Events};
        false ->
            timer:sleep(500),
            wait(Collector, EventsCount, TryCount - 1, Loc, ErrorMsg)
    end.


collector(Collector, Event, Loc, ErrorMsg) ->
    collector(Collector, Event, Loc, ErrorMsg, 1000).


collector(Collector, Event, Loc, ErrorMsg, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    Collector ! {{Parent, Ref}, Event},
    receive
        {Ref, Reply} ->
            Reply
        after Timeout ->
            erlang:error({timeout, [{error, ErrorMsg} | Loc]})
    end.


loop(Events) ->
    receive
        {{From, Ref}, {store, Event}} ->
            From ! {Ref, ok},
            loop([Event | Events]);
        {{From, Ref}, events} ->
            From ! {Ref, {ok, lists:reverse(Events)}},
            loop(Events);
        {{From, Ref}, clear} ->
            From ! {Ref, ok},
            loop([]);
        {{From, Ref}, stop} ->
            From ! {Ref, {ok, Events}}
    end.
