% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

% This module provides a faster interface to erlang tracing
% The main purpose of this module to provide fast filtering of events so we
% drop unneded events as soon as we possibly can.
% There are multiple architectural decission worth documenting.
% We use NIF (see cutil_tracer.c) to filter events and dispatch them to one of the
% receivers in the pool. The NIF is using Erlang's https://erlang.org/doc/man/erl_tracer.html
% facility.
% The receiver is selected based on `hash(TraceePid) % length(Receivers)`.
% We use seq_token to pass `trace_id` to the NIF. So we can run multiple traces in parallel.
% We use `cutil_term` to rememeber match_spec source.
% The matchspec we pass to `erlang:trace_pattern/3` includes additional elements:
% - seq_token
% - trace_id
% - target_id
% - ms_return
% - caller_mfa
% - trigger_mfa
% The NIF filter's logic is
% - return `ok` if `TracerState(argv[1])` doesn't contain `trace_id` field
% - return `ok` if `Options(argv[4])` map doesn't containing `match_spec_result` field
% - return `ok` if `match_spec_result` map doesn't contain `target_id` field
% - return `ok` if `match_spec_result` map doesn't contain `seq_token` field
% - return `ok` if `seq_token` tuple is less then 2 elements
% - extract expected `trace_id` from first element of `seq_token` tuple
% - return `ok` if `trace_id` extracted from `TracerState` and `seq_token` are not equal
% - extract `triger_mfa` and `caller_mfa` from `match_spec_result`
% - return `ok` if `triger_mfa` and `caller_mfa` are not equal
% - send `{Tag, Tracee, Ts, Term, Result}` or `{Tag, Tracee, Ts, Term, Extra, Result}`
%
% The event is received by a receiver process which is running `cutil_trace_receiver` code.
% The role of `cutil_trace_receiver` is to
% - format event
% - augment the event with original matchspec
% - convert timestampt to system time
% - pass the event to configured (user provided) tracer module

-module(cutil_trace).

-export([
    trace/3,
    trace/4,
    get_pool/2,
    stop_pool/1
]).

-type pool_id() :: atom().
-type tracing_mode() :: trace
    | filter
    | profile.

%% @see http://erlang.org/doc/man/erlang.html#trace-3
-type pid_port_spec() :: pid()
    | port()
    | all
    | processes
    | ports
    | existing
    | existing_processes
    | existing_ports
    | new
    | new_processes
    | new_ports.

-type pool_options() :: #{
    pool_size => pos_integer()
}.

-type id() :: binary().
-type trace_id() :: id().
-type error(_Error) :: no_return().

-spec trace(PoolPid :: pid(), TraceId :: trace_id(), Mode :: tracing_mode()) ->
    integer() | error({unknown_mode, Mode :: term()}).

trace(PoolPid, TraceId, Mode) ->
    trace(PoolPid, TraceId, all, Mode).


-spec trace(
        PoolPid :: pid(),
        TraceId :: trace_id(),
        PidPortSpec :: pid_port_spec(),
        Mode :: tracing_mode()
    ) -> integer() | error({unknown_mode, Mode :: term()}).

trace(PoolPid, TraceId, PidPortSpec, trace) ->
    do_trace(PoolPid, TraceId, PidPortSpec, trace);

trace(PoolPid, TraceId, PidPortSpec, filter) ->
    do_trace(PoolPid, TraceId, PidPortSpec, filter);

trace(_, _, _, Mode) ->
    erlang:error({unknown_mode, Mode}).

do_trace(PoolPid, TraceId, PidPortSpec, Mode) ->
    Tracers = cutil_tracer_pool:tracers(PoolPid),
    TracersMap = maps:from_list(
        lists:zip(lists:seq(0, length(Tracers) - 1), Tracers)),
    TracerState = #{mode => Mode, tracers => TracersMap, trace_id => TraceId},
    TraceOpts = [{tracer, cutil_tracer, TracerState}],
    erlang:trace(PidPortSpec, true, [call | TraceOpts]).


-spec get_pool(Name :: pool_id(), Options :: pool_options()) ->
    pid().

get_pool(Name, #{pool_size := PoolSize} = Options) ->
    case whereis(Name) of
        undefined ->
            {ok, PoolPid} = cutil_sup:start_pool(Name, PoolSize, Options),
            register(Name, PoolPid),
            PoolPid;
        PoolPid ->
            PoolPid
    end;

get_pool(Name, #{} = Options) ->
    PoolSize = erlang:system_info(schedulers),
    get_pool(Name, maps:put(pool_size, PoolSize, Options)).


-spec stop_pool(NameOrPid :: pid() | pool_id()) ->
    ok | {error, Reason :: term()}.

stop_pool(NameOrPid) ->
    cutil_sup:stop_pool(NameOrPid).
