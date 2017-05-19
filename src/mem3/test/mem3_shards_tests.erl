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

-module(mem3_shards_tests).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-include_lib("couch/include/couch_eunit.hrl").


-define(DB, <<"eunit_db_name">>).
-define(INFINITY, 99999999).

-define(DBS, mem3_dbs).
-define(SHARDS, mem3_shards).
-define(ATIMES, mem3_atimes).
-define(OPENERS, mem3_openers).
-define(RELISTEN_DELAY, 5000).

mem3_shards_load_shards_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            t_spawn_writer_in_load_shards_from_db()
        ]
    }.


setup() ->
    ets:new(?SHARDS, [bag, public, named_table, {keypos, #shard.dbname}]),
    ets:new(?OPENERS, [bag, public, named_table]),
    ets:new(?DBS, [set, public, named_table]),
    ets:new(?ATIMES, [ordered_set, public, named_table]),
    meck:expect(config, get, ["mem3", "shards_db", '_'], "_dbs"),
    ok.


teardown(_) ->
    meck:unload(),
    ets:delete(?ATIMES),
    ets:delete(?DBS),
    ets:delete(?OPENERS),
    ets:delete(?SHARDS).


t_spawn_writer_in_load_shards_from_db() ->
    ?_test(begin
        meck:expect(couch_db, open_doc, 3, {ok, #doc{body = {[]}}}),
        meck:expect(couch_db, get_update_seq, 1, 1),
        meck:expect(mem3_util, build_ordered_shards, 2, mock_shards()),
        erlang:register(mem3_shards, self()), % register to get cache_insert cast
        mem3_shards:load_shards_from_db(#db{name = <<"testdb">>}, ?DB),
        meck:validate(couch_db),
        meck:validate(mem3_util),
        Cast = receive
                {'$gen_cast', Msg} -> Msg
            after 1000 ->
                timeout
        end,
        ?assertMatch({cache_insert, ?DB, Pid, 1} when is_pid(Pid), Cast),
        {cache_insert, _, WPid, _} = Cast,
        exit(WPid, kill),
        ?assertEqual([{?DB, WPid}], ets:tab2list(?OPENERS))
    end).

mock_shards() ->
    [
        #ordered_shard{
            name = <<"testshardname">>,
            node = node(),
            dbname = ?DB,
            range = [0,1],
            order = 1
        }
    ].


mem3_shards_changes_test_() -> {
    "Test mem3_shards changes listener", {
        foreach,
        fun setup_changes/0, fun teardown_changes/1,
        [
            fun should_kill_changes_listener_on_shutdown/1
        ]
    }
}.

setup_changes() ->
    ok = meck:expect(mem3_util, ensure_exists, ['_'],
        {ok, #db{name = <<"dbs">>, update_seq = 0}}),
    ok = meck:expect(couch_db, close, ['_'], ok),
    ok = application:start(config),
    {ok, Pid} = mem3_shards:start_link(),
    true = erlang:unlink(Pid),
    Pid.


teardown_changes(Pid) ->
    true = exit(Pid, shutdown),
    ok = application:stop(config),
    meck:unload().


should_kill_changes_listener_on_shutdown(Pid) ->
    ?_test(begin
        ?assert(is_process_alive(Pid)),
        {ok, ChangesPid} = mem3_shards:get_changes_pid(),
        ?assert(is_process_alive(ChangesPid)),
        true = test_util:stop_sync_throw(
            ChangesPid, fun() -> exit(Pid, shutdown) end, wait_timeout),
        ?assertNot(is_process_alive(ChangesPid)),
        ok
    end).
