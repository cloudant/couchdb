% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_flags_tests).

-include_lib("couch/include/couch_eunit.hrl").

%% couch_epi_plugin behaviour callbacks
-export([
    app/0,
    providers/0,
    services/0,
    data_providers/0,
    data_subscriptions/0,
    processes/0,
    notify/3
]).

-export([
    data/0
]).

app() ->
    test_app.

providers() ->
    [].

services() ->
    [].

data_providers() ->
    [
        {{flags, config}, {callback_module, ?MODULE}, [{interval, 100}]}
    ].

data_subscriptions() ->
    [].

processes() ->
    [].


notify(_, _, _) ->
    ok.

data() ->
    couch_flags_config:data(test_config()).

setup() ->
    %% FIXME after we upgrade couch_epi
    application:stop(couch_epi), % in case it's already running from other tests...
    application:unload(couch_epi),

    application:load(couch_epi),
    application:set_env(couch_epi, plugins, [couch_db_epi, ?MODULE]),
    test_util:start_couch([couch_epi]).


teardown(_) ->
    ok.


couch_flags_test_() ->
    {
        "test couch_flags",
        {
           setup, fun setup/0, fun teardown/1,
           enabled_flags_tests()
              ++ disabled_flags_tests()
              ++ is_enabled()
              ++ match_performance()
        }
    }.

enabled_flags_tests() ->
    [{"enabled_flags_tests", [
        {"flags_default_rule",
         ?_assertEqual(
            [flag_foo], couch_flags:enabled("something"))},
        {"flags_wildcard_rule",
         ?_assertEqual(
            [baz, flag_bar, flag_foo],
            couch_flags:enabled("shards/test/something"))},
        {"flags_exact_rule",
         ?_assertEqual(
            [baz, flag_bar, flag_foo],
            couch_flags:enabled("shards/test/exact"))},
        {"flags_blacklist_rule",
         ?_assertEqual(
            [],
            couch_flags:enabled("shards/blacklist/4"))}
    ]}].

disabled_flags_tests() ->
    [{"disabled_flags_tests", [
        {"flags_default_rule",
         ?_assertEqual(
            [flag_bar], couch_flags:disabled("something"))},
        {"flags_wildcard_rule",
         ?_assertEqual(
            [],
            couch_flags:disabled("shards/test/something"))},
        {"flags_exact_rule",
         ?_assertEqual(
            [],
            couch_flags:disabled("shards/test/exact"))},
        {"flags_blacklist_rule",
         ?_assertEqual(
            [flag_bar, flag_foo],
            couch_flags:disabled("shards/blacklist/4"))}
    ]}].

is_enabled() ->
     [{"is_enabled_tests", [
        {"flags_default_rule [enabled]",
            ?_assert(couch_flags:is_enabled(flag_foo, "something"))},
        {"flags_default_rule [disabled]",
            ?_assertNot(couch_flags:is_enabled(baz, "something"))},
        {"flags_default_rule [not_existent]",
            ?_assertNot(couch_flags:is_enabled(non_existent, "something"))},

        {"flags_wildcard_rule [enabled]",
            ?_assert(couch_flags:is_enabled(flag_bar, "shards/test/something"))},
        {"flags_wildcard_rule [not_existent]",
            ?_assertNot(couch_flags:is_enabled(non_existent, "shards/test/something"))},

        {"flags_exact_rule [overide_disbled]",
            ?_assert(couch_flags:is_enabled(flag_bar, "shards/test/exact"))},
        {"flags_exact_rule [not_existent]",
            ?_assertNot(couch_flags:is_enabled(non_existent, "shards/test/exact"))},

        {"flags_blacklist_rule [overide_enabled]",
            ?_assertNot(couch_flags:is_enabled(flag_foo, "shards/blacklist/4"))},
        {"flags_blacklist_rule [not_existent]",
            ?_assertNot(couch_flags:is_enabled(non_existent, "shards/blacklist/4"))}
    ]}].

match_performance() ->
    [{"match_performance", [
        ?_test(begin
            ?debugTime("1 million of operations took", lists:foreach(fun(_) ->
                couch_flags:is_enabled(flag_bar, "shards/test/exact")
            end, lists:seq(1, 1000000)))
        end)
    ]}].


test_config() ->
    [
     {"flag_foo||*", "true"},
     {"flag_bar||*", "false"},
     {"flag_bar||shards/test*", "true"},
     {"flag_foo||shards/blacklist*", "false"},
     {"shards/test*", "[baz]"},
     {"flag_bar||shards/exact", "true"},
     {"shards/test/exact", "[flag_bar]"}
    ].
