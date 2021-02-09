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

-module(cutil_trace_receiver).

%% API.
-export([start_link/2]).

-export([
    init/1,
    system_continue/3,
    system_terminate/4,
    system_code_change/4
]).

-define(STATE, ?MODULE).

-record(?STATE,
    {
        tracer :: module(),
        user_data  :: term(),
        parent :: pid(),
        idx :: pos_integer()
    }).

-define(REGISTRY, cutil_trace_registry).

-include("cutil.hrl").

start_link(Nth, Opts) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [[self(), Nth, Opts]]),
    {ok, Pid}.


init([Parent, Nth, Opts]) ->
    Tracer = maps:get(tracer, Opts, cutil_none_tracer),

    %% Store all messages off the heap to avoid unnecessary GC.
    process_flag(message_queue_data, off_heap),

    {ok, UserData} = Tracer:init(Nth, Opts),
    State = #?STATE{
        tracer = Tracer,
        user_data = UserData,
        parent = Parent,
        idx = Nth
    },
    loop(State).


loop(State) ->
    #?STATE{
        parent = Parent,
        tracer = Tracer,
        user_data = UserData,
        idx = Idx
    } = State,
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], Parent);
        Event ->
            Tracer:event(format_event(Event), Idx, UserData),
            loop(State)
    end.


system_continue(_, _, State) ->
    loop(State).


-spec system_terminate(any(), _, _, _) -> no_return().

system_terminate(Reason, _, _, _) ->
    exit(Reason).


system_code_change(Misc, _, _, _) ->
    {ok, Misc}.


format_event(#cutil_tracer_event{ts = Ts, mspec = Info} = Event) ->
    %% Convert the event's monotonic time to its system time.
    TimeStamp = erlang:time_offset(microsecond) + Ts,
    MS = lookup_ms(Info),
    Event#cutil_tracer_event{ts = TimeStamp, mspec = maps:put(ms, MS, Info)}.


lookup_ms(Info) ->
    #{
        trigger_mfa := TriggerMFA,
        target_id := TargetId
    } = Info,
    cutil_term:get(?REGISTRY, {TriggerMFA, TargetId}, undefined).