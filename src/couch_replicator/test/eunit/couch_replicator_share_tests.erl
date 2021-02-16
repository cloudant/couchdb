-module(couch_replicator_share_tests).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include("couch_replicator_test.hrl").


-define(SHARES, couch_replicator_shares).
-define(PRIORITIES, couch_replicator_priorities).
-define(USAGE, couch_replicator_usage).
-define(CHARGES, couch_replicator_stopped_usage).
-define(NUM_JOBS, couch_replicator_num_jobs).

-define(DB1, <<"db1">>).
-define(DB2, <<"db2">>).
-define(DB3, <<"db3">>).
-define(J1, <<"j1">>).
-define(J2, <<"j2">>).
-define(J3, <<"j3">>).


fair_share_test_() ->
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(init_works),
                ?TDEF_FE(shares_are_updated_and_reset),
                ?TDEF_FE(jobs_are_added_and_removed),
                ?TDEF_FE(can_fetch_job_priority),
                ?TDEF_FE(jobs_are_charged),
                ?TDEF_FE(usage_is_updated),
                ?TDEF_FE(priority_coefficient_works),
                ?TDEF_FE(priority_decays_when_jobs_stop_running),
                ?TDEF_FE(priority_increases_when_jobs_run),
                ?TDEF_FE(two_dbs_equal_shares_equal_number_of_jobs),
                ?TDEF_FE(two_dbs_unequal_shares_equal_number_of_jobs),
                ?TDEF_FE(two_dbs_equal_shares_unequal_number_of_jobs),
                ?TDEF_FE(two_dbs_unequal_shares_unequal_number_of_jobs),
                ?TDEF_FE(three_dbs_equal_shares_equal_number_of_jobs),
                ?TDEF_FE(three_dbs_unequal_shares_equal_number_of_jobs),
                ?TDEF_FE(three_dbs_equal_shares_unequal_number_of_jobs),
                ?TDEF_FE(three_dbs_unequal_shares_unequal_number_of_jobs)
            ]
        }
    }.


setup_all() ->
    test_util:start_couch().


teardown_all(Ctx) ->
    config_delete("priority_coeff"),
    config_delete("usage_coeff"),
    config_shares_delete(),
    test_util:stop_couch(Ctx).


setup() ->
    couch_replicator_share:init(),
    ok.


teardown(_) ->
    couch_replicator_share:clear(),
    config_delete("priority_coeff"),
    config_delete("usage_coeff"),
    config_shares_delete().


init_works(_)->
    Tables = [?SHARES, ?PRIORITIES, ?USAGE, ?CHARGES, ?NUM_JOBS],
    [?assert(is_list(ets:info(T))) || T <- Tables],
    ?assertEqual(#{}, tab2map(?SHARES)),

    couch_replicator_share:clear(),
    [?assertEqual(undefined, ets:info(T)) || T <- Tables],

    config_share_set("db1", "200"),
    couch_replicator_share:init(),
    ?assertEqual(200, couch_replicator_share:shares(?DB1)),
    ?assertEqual(#{?DB1 => 200}, tab2map(?SHARES)).


shares_are_updated_and_reset(_) ->
    ?assertEqual(#{}, tab2map(?SHARES)),

    couch_replicator_share:update_shares(?DB1, 42),
    ?assertEqual(42, couch_replicator_share:shares(?DB1)),

    couch_replicator_share:reset_shares(?DB1),
    ?assertEqual(100, couch_replicator_share:shares(?DB1)),
    ?assertEqual(#{}, tab2map(?SHARES)),

    % min shares
    couch_replicator_share:update_shares(?DB1, 0),
    ?assertEqual(1, couch_replicator_share:shares(?DB1)),

    % max shares
    couch_replicator_share:update_shares(?DB1, 1001),
    ?assertEqual(1000, couch_replicator_share:shares(?DB1)).


jobs_are_added_and_removed(_) ->
    couch_replicator_share:job_added(job(?J1, ?DB1)),
    ?assertEqual(1, couch_replicator_share:num_jobs(?DB1)),
    ?assertEqual(#{?J1 => 0}, tab2map(?PRIORITIES)),

    couch_replicator_share:job_added(job(?J2, ?DB1)),
    ?assertEqual(2, couch_replicator_share:num_jobs(?DB1)),
    ?assertEqual(#{?J1 => 0, ?J2 => 0}, tab2map(?PRIORITIES)),

    couch_replicator_share:job_added(job(?J3, ?DB2)),
    ?assertEqual(1, couch_replicator_share:num_jobs(?DB2)),
    ?assertEqual(#{?J1 => 0, ?J2 => 0, ?J3 => 0}, tab2map(?PRIORITIES)),

    couch_replicator_share:job_removed(job(?J1, ?DB1)),
    ?assertEqual(1, couch_replicator_share:num_jobs(?DB1)),
    ?assertEqual(#{?J2 => 0, ?J3 => 0}, tab2map(?PRIORITIES)),

    couch_replicator_share:job_removed(job(?J3, ?DB2)),
    ?assertEqual(0, couch_replicator_share:num_jobs(?DB2)),
    ?assertEqual(0, couch_replicator_share:priority(?J3)),

    couch_replicator_share:job_removed(job(?J2, ?DB1)),
    ?assertEqual(0, couch_replicator_share:num_jobs(?DB2)),
    ?assertEqual(#{}, tab2map(?NUM_JOBS)),
    ?assertEqual(0, couch_replicator_share:priority(?J2)),
    ?assertEqual(#{}, tab2map(?PRIORITIES)).


can_fetch_job_priority(_) ->
    couch_replicator_share:job_added(job(?J1, ?DB1)),
    ?assertEqual(0, couch_replicator_share:priority(?J1)),

    ets:insert(?PRIORITIES, {?J1, 42}),
    ?assertEqual(42, couch_replicator_share:priority(?J1)),

    ets:delete(?PRIORITIES, ?J1),
    ?assertEqual(0, couch_replicator_share:priority(?J1)).


jobs_are_charged(_) ->
    Job1 = running_job(?J1, ?DB1),
    couch_replicator_share:job_added(Job1),
    ?assertEqual(#{}, tab2map(?CHARGES)),

    couch_replicator_share:charge(Job1, 1000, {0, 1, 0}),
    ?assertEqual(#{?DB1 => 1000000}, tab2map(?CHARGES)),

    % Stopped jobs are not charged
    couch_replicator_share:charge(stop(Job1), 1000, {0, 1, 0}),
    ?assertEqual(#{?DB1 => 1000000}, tab2map(?CHARGES)),

    % Only charge up to one interval's worth even if job ran longer
    couch_replicator_share:charge(Job1, 1000, {0, 5, 0}),
    ?assertEqual(#{?DB1 => 2000000}, tab2map(?CHARGES)),

    % Charges are accumulated from jobs in same db
    Job2 = running_job(?J2, ?DB1),
    couch_replicator_share:job_added(Job2),
    couch_replicator_share:charge(Job2, 1000, {0, 0, 1}),
    ?assertEqual(#{?DB1 => 2000001}, tab2map(?CHARGES)),

    % Charges are not cleared if jobs are removed
    couch_replicator_share:job_removed(Job1),
    couch_replicator_share:job_removed(Job2),
    ?assertEqual(#{?DB1 => 2000001}, tab2map(?CHARGES)).


usage_is_updated(_) ->
    Job = running_job(?J1, ?DB1),
    couch_replicator_share:job_added(Job),

    couch_replicator_share:charge(Job, 60000, {0, 60, 0}),
    couch_replicator_share:update_usage(),
    ?assertEqual(60000000, couch_replicator_share:usage(?DB1)),

    % Charges table is cleared after usage is updated
    ?assertEqual(#{}, tab2map(?CHARGES)),

    % Check that usage decay works
    config_set("usage_coeff", "0.2"),
    couch_replicator_share:update_usage(),
    ?assertEqual(12000000, couch_replicator_share:usage(?DB1)),

    config_set("usage_coeff", "0.5"),
    couch_replicator_share:update_usage(),
    ?assertEqual(6000000, couch_replicator_share:usage(?DB1)),

    % Check that function both decays and updates from charges
    couch_replicator_share:charge(Job, 60000, {0, 60, 0}),
    couch_replicator_share:update_usage(),
    ?assertEqual(63000000, couch_replicator_share:usage(?DB1)),

    % Usage eventually decays to 0 and is removed from the table
    [couch_replicator_share:update_usage() || _ <- lists:seq(1, 100)],
    ?assertEqual(0, couch_replicator_share:usage(?DB1)),
    ?assertEqual(#{}, tab2map(?USAGE)).


priority_coefficient_works(_) ->
    couch_replicator_share:job_added(job(?J1, ?DB1)),
    ets:insert(?PRIORITIES, {?J1, 1000}),

    config_set("priority_coeff", "0.8"),
    couch_replicator_share:decay_priorities(),
    ?assertEqual(800, couch_replicator_share:priority(?J1)),

    config_set("priority_coeff", "0.5"),
    couch_replicator_share:decay_priorities(),
    ?assertEqual(400, couch_replicator_share:priority(?J1)),

    % If non-float junk value is set then the default is used
    config_set("priority_coeff", "junk"),
    couch_replicator_share:decay_priorities(),
    ?assertEqual(392, couch_replicator_share:priority(?J1)),

    % Clipped to 1.0 max
    config_set("priority_coeff", "1.1"),
    couch_replicator_share:decay_priorities(),
    ?assertEqual(392, couch_replicator_share:priority(?J1)),

    % Clipped to 0.0 min and removed when =< 0
    config_set("priority_coeff", "-0.1"),
    couch_replicator_share:decay_priorities(),
    ?assertEqual(0, couch_replicator_share:priority(?J1)),
    ?assertEqual(#{}, tab2map(?PRIORITIES)).


priority_decays_when_jobs_stop_running(_) ->
    Job = running_job(?J1, ?DB1),
    couch_replicator_share:job_added(Job),

    % Ran for one cycle then stop
    {[], Pending} = reschedule(1, {[Job], []}),

    % Priority is non-0 initially
    ?assert(couch_replicator_share:priority(?J1) > 0),

    % Priority decays to 0 after some cycles
    [reschedule(0, {[], Pending}) || _ <- lists:seq(1, 500)],
    ?assertEqual(0, couch_replicator_share:priority(?J1)).


priority_increases_when_jobs_run(_) ->
    Job = running_job(?J1, ?DB1),
    couch_replicator_share:job_added(Job),

    Running = [Job],
    reschedule(0, {Running, []}),
    P1 = couch_replicator_share:priority(?J1),
    ?assert(P1 > 0),

    % Priority increases
    reschedule(0, {Running, []}),
    P2 = couch_replicator_share:priority(?J1),
    ?assert(P2 > P1),

    % Additive priority increase is balanced out by priority decay
    [reschedule(0, {Running, []}) || _ <- lists:seq(1, 500)],
    Pn = couch_replicator_share:priority(?J1),
    ?assert(Pn > P2),

    reschedule(0, {Running, []}),
    Pm = couch_replicator_share:priority(?J1),
    ?assertEqual(Pn, Pm).


two_dbs_equal_shares_equal_number_of_jobs(_) ->
    couch_replicator_share:update_shares(?DB1, 100),
    couch_replicator_share:update_shares(?DB2, 100),
    Jobs = jobs(#{?DB1 => {25, 75}, ?DB2 => {25, 75}}),
    #{?DB1 := Db1, ?DB2 := Db2} = run_scheduler(1000, 10, Jobs),
    ?assert(49 =< Db1 andalso Db1 =< 51),
    ?assert(49 =< Db2 andalso Db2 =< 51).


two_dbs_unequal_shares_equal_number_of_jobs(_) ->
    couch_replicator_share:update_shares(?DB1, 100),
    couch_replicator_share:update_shares(?DB1, 900),
    Jobs = jobs(#{?DB1 => {25, 75}, ?DB2 => {25, 75}}),
    #{?DB1 := Db1, ?DB2 := Db2} = run_scheduler(1000, 10, Jobs),
    ?assert(89 =< Db1 andalso Db1 =< 91),
    ?assert(9 =< Db2 andalso Db2 =< 11).


two_dbs_equal_shares_unequal_number_of_jobs(_) ->
    couch_replicator_share:update_shares(?DB1, 100),
    couch_replicator_share:update_shares(?DB2, 100),
    Jobs = jobs(#{?DB1 => {25, 25}, ?DB2 => {25, 125}}),
    #{?DB1 := Db1, ?DB2 := Db2} = run_scheduler(1000, 10, Jobs),
    ?assert(49 =< Db1 andalso Db1 =< 51),
    ?assert(49 =< Db2 andalso Db2 =< 51).


two_dbs_unequal_shares_unequal_number_of_jobs(_) ->
    couch_replicator_share:update_shares(?DB1, 1),
    couch_replicator_share:update_shares(?DB2, 100),
    Jobs = jobs(#{?DB1 => {25, 25}, ?DB2 => {25, 125}}),
    #{?DB1 := Db1, ?DB2 := Db2} = run_scheduler(1000, 10, Jobs),
    ?assert(0 =< Db1 andalso Db1 =< 2),
    ?assert(98 =< Db2 andalso Db2 =< 100).


three_dbs_equal_shares_equal_number_of_jobs(_) ->
    couch_replicator_share:update_shares(?DB1, 100),
    couch_replicator_share:update_shares(?DB2, 100),
    couch_replicator_share:update_shares(?DB3, 100),
    Jobs = jobs(#{?DB1 => {25, 75}, ?DB2 => {25, 75}, ?DB3 => {25, 75}}),
    #{?DB1 := Db1, ?DB2 := Db2, ?DB3 := Db3} = run_scheduler(1000, 10, Jobs),
    ?assert(32 =< Db1 andalso Db1 =< 34),
    ?assert(32 =< Db2 andalso Db2 =< 34),
    ?assert(32 =< Db3 andalso Db3 =< 34).


three_dbs_unequal_shares_equal_number_of_jobs(_) ->
    couch_replicator_share:update_shares(?DB1, 100),
    couch_replicator_share:update_shares(?DB2, 700),
    couch_replicator_share:update_shares(?DB3, 200),
    Jobs = jobs(#{?DB1 => {25, 75}, ?DB2 => {25, 75}, ?DB3 => {25, 75}}),
    #{?DB1 := Db1, ?DB2 := Db2, ?DB3 := Db3} = run_scheduler(1000, 10, Jobs),
    ?assert(9 =<  Db1 andalso Db1 =< 11),
    ?assert(69 =< Db2 andalso Db2 =< 71),
    ?assert(19 =< Db3 andalso Db3 =< 21).


three_dbs_equal_shares_unequal_number_of_jobs(_) ->
    couch_replicator_share:update_shares(?DB1, 100),
    couch_replicator_share:update_shares(?DB2, 100),
    couch_replicator_share:update_shares(?DB3, 100),
    Jobs = jobs(#{?DB1 => {25, 25}, ?DB2 => {25, 100}, ?DB3 => {25, 75}}),
    #{?DB1 := Db1, ?DB2 := Db2, ?DB3 := Db3} = run_scheduler(1000, 10, Jobs),
    ?assert(32 =< Db1 andalso Db1 =< 34),
    ?assert(32 =< Db2 andalso Db2 =< 34),
    ?assert(32 =< Db3 andalso Db3 =< 34).


three_dbs_unequal_shares_unequal_number_of_jobs(_) ->
    couch_replicator_share:update_shares(?DB1, 1000),
    couch_replicator_share:update_shares(?DB2, 100),
    couch_replicator_share:update_shares(?DB3, 1),
    Jobs = jobs(#{?DB1 => {25, 100}, ?DB2 => {25, 125}, ?DB3 => {25, 875}}),
    #{?DB1 := Db1, ?DB2 := Db2, ?DB3 := Db3} = run_scheduler(1000, 10, Jobs),
    ?assert(87 =< Db1 andalso Db1 =< 89),
    ?assert(9 =< Db2 andalso Db2 =< 11),
    ?assert(2 =< Db3 andalso Db3 =< 4).


config_set(K, V) ->
    config:set("replicator", K, V, _Persist = false).


config_delete(K) ->
    config:delete("replicator", K, _Persist = false).


config_share_set(K, V) ->
    config:set("replicator.shares", K, V, _Persist = false).


config_shares_delete() ->
    [config:delete("replicator.shares", K, _Persist = false) ||
        {K, _} <- config:get("replicator.shares")].


tab2map(T) when is_atom(T) ->
    maps:from_list(ets:tab2list(T)).


job(rand, Db) ->
    job(rand:uniform(1 bsl 59), Db);

job(Id, Db) ->
    Job = #job{
        id = Id,
        rep = #rep{
            db_name = Db,
            user_ctx = #user_ctx{}
        }
    },
    stop(Job).


running_job(Id, Db) ->
    run(job(Id, Db)).


run(#job{} = Job) ->
    Job#job{
        pid = list_to_pid("<0.9999.999>"),
        history = [{started, {0, 0, 0}}, {added, {0, 0, 0}}]
    }.


stop(#job{} = Job) ->
    Job#job{
        pid = undefined,
        history = [{added, {0, 0, 0}}]
    }.


% Simple scheduler simulator. Start and stop N jobs and do the
% accounting steps. Return a new list of running and pending jobs. If
% N is 0 then jobs which were running stay running and jobs were
% pending stay pending.
%
reschedule(N, {Running, Pending}) ->
    [couch_replicator_share:charge(Job, 60000, {0, 60, 0}) || Job <- Running],
    couch_replicator_share:update_usage(),
    couch_replicator_share:decay_priorities(),
    [couch_replicator_share:update_priority(Job) || Job <- Running],

    RunPr = [{couch_replicator_share:priority(Job#job.id), Job} ||
        Job <- Running],
    StopPr = [{couch_replicator_share:priority(Job#job.id), Job} ||
        Job <- Pending],

    {_, Running1} = lists:unzip(lists:reverse(lists:sort(RunPr))),
    {_, Pending1} = lists:unzip(lists:sort(StopPr)),

    ToStop = lists:sublist(Running1, N),
    ToStart = lists:sublist(Pending1, N),

    Running2 = [run(Job) || Job <- ToStart] ++ Running1 -- ToStop,
    Pending2 = [stop(Job) || Job <- ToStop] ++ Pending1 -- ToStart,

    {Running2, Pending2}.


% Run a few scheduling cycles and calculate usage percentage for each db
%
run_scheduler(Cycles, Churn, Jobs0) ->
    Acc0 = {#{}, Jobs0},

    {Sum, _} = lists:foldl(fun(_CycleCnt, {UsageAcc, {Running, _} = Jobs}) ->
        UsageAcc1 = lists:foldl(fun(#job{} = Job, Acc) ->
            Db = Job#job.rep#rep.db_name,
            maps:update_with(Db, fun(V) -> V + 1 end, 0, Acc)
        end, UsageAcc, Running),
        {UsageAcc1, reschedule(Churn, Jobs)}
    end, Acc0, lists:seq(1, Cycles)),

    Total = maps:fold(fun(_, V, Acc) -> Acc + V end, 0, Sum),
    maps:map(fun(_Db, V) -> round(V / Total * 100) end, Sum).


% Dbs = #{Db => {RunningCount, PendingCount}
%
jobs(#{} = Dbs) ->
    maps:fold(fun(Db, {RCnt, PCnt}, {Running, Pending}) ->
        RJobs = [running_job(rand, Db) || _ <- lists:seq(1, RCnt)],
        PJobs = [job(rand, Db) || _ <- lists:seq(1, PCnt)],
        [couch_replicator_share:job_added(Job) || Job <- RJobs ++ PJobs],
        {Running ++ RJobs, Pending ++ PJobs}
    end, {[], []}, Dbs).
