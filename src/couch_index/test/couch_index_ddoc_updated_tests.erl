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

-module(couch_index_ddoc_updated_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

setup() ->
    DbName = ?tempdb(),
    fake_mem3(DbName),
    fake_index(),

    DbNames = [mem3:name(Sh) || Sh <- mem3:local_shards(mem3:dbname(DbName))],
    [Db1| RestDbs] = lists:map(fun(DBName) ->
        {ok, DB} = couch_db:create(DBName, [?ADMIN_CTX]),
        DB
    end, DbNames),

    % create a DDoc on the 1st Db
    DDocID = <<"idx_name">>,
    DDocJson = couch_doc:from_json_obj({[
        {<<"_id">>, DDocID},
        {<<"value">>, 1}
    ]}),
    {ok, _Rev} = couch_db:update_doc(Db1, DDocJson, []),
    {ok, Db} = couch_db:reopen(Db1),
    {ok, DDoc} = couch_db:open_doc(Db, DDocID, [ejson_body, ?ADMIN_CTX]),
    Dbs = [Db | RestDbs],
    {Dbs, DDoc}.


teardown({Dbs, _DDoc}) ->
    lists:foreach(fun(Db) ->
        couch_db:close(Db),
        couch_server:delete(Db#db.name, [?ADMIN_CTX])
    end, Dbs),
    (catch meck:unload(mem3)),
    (catch meck:unload(test_index)),
    ok.


ddoc_update_test_() ->
    {
        "Check ddoc update actions",
        {
            setup,
            fun() -> test_util:start_couch([]) end, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun ddoc_update/1
                ]
            }
        }
    }.


ddoc_update({[Db | _] = Dbs, DDoc}) ->
    ?_test(begin
        N = length(Dbs),
        DDocID = DDoc#doc.id,

        % run couch_index process for each Db
        lists:foreach(fun(DB) ->
            couch_index_server:get_index(test_index, DB, DDoc)
        end, Dbs),
        IndexesBefore = ets:match_object(
            couchdb_indexes_by_db, {'$1', {DDocID, '$2'}}),
        ?assertEqual(N, length(IndexesBefore)),

        % update ddoc
        DDocJson2 = couch_doc:from_json_obj({[
            {<<"_id">>, DDocID},
            {<<"value">>, 2},
            {<<"_rev">>, couch_doc:rev_to_str(DDoc#doc.revs)}
        ]}),
        {ok, _} = couch_db:update_doc(Db, DDocJson2, []),

        % assert that all index processes exited after ddoc updated
        couch_index_server:handle_db_event(
            Db#db.name, {ddoc_updated, DDocID}, {st, ""}),
        timer:sleep(1000),
        IndexesAfter = ets:match_object(
            couchdb_indexes_by_db, {'$1', {DDocID, '$2'}}),
        ?assertEqual(0, length(IndexesAfter)),
        ok
    end).


fake_mem3(DbName0) ->
    ok = meck:expect(mem3, dbname, fun(_DbName) -> DbName0 end),
    ok = meck:expect(mem3, name, fun(DbName) -> DbName end),
    ok = meck:expect(mem3, local_shards, fun(DbName) -> [
        <<DbName/binary, <<"/00000000-1fffffff">>/binary>>,
        <<DbName/binary, <<"/20000000-3fffffff">>/binary>>,
        <<DbName/binary, <<"/40000000-5fffffff">>/binary>>,
        <<DbName/binary, <<"/60000000-7fffffff">>/binary>>,
        <<DbName/binary, <<"/80000000-9fffffff">>/binary>>,
        <<DbName/binary, <<"/a0000000-bfffffff">>/binary>>,
        <<DbName/binary, <<"/c0000000-dfffffff">>/binary>>,
        <<DbName/binary, <<"/e0000000-ffffffff">>/binary>>
    ] end).


fake_index() ->
    ok = meck:new([test_index], [non_strict]),
    ok = meck:expect(test_index, init, fun(Db, DDoc) ->
        {ok, {couch_db:name(Db), DDoc}}
    end),
    ok = meck:expect(test_index, open, fun(_Db, State) ->
        {ok, State}
    end),
    ok = meck:expect(test_index, get, fun
        (db_name, {DbName, _DDoc}) ->
            DbName;
        (idx_name, {_DbName, DDoc}) ->
            DDoc#doc.id;
        (signature, {_DbName, DDoc}) ->
            couch_crypto:hash(md5, term_to_binary(DDoc));
        (update_seq, Seq) ->
            Seq
    end).

