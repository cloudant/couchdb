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

% This module implements {flags, config} data provider
-module(couch_flags_config).

-export([
    data_provider/0,
    data/0,
    collect_flags/1,
    by_flag/1
]).

-export([
    enable/2,
    data/1
]).

-define(DATA_INTERVAL, 1000).

-type pattern()
    :: binary(). %% non empty binary which optionally can end with *


-type flag_id() :: atom().

-type key()
    :: {enabled, pattern()}
       | {disabled, pattern()}
       | {enabled_for, flag_id(), pattern()}
    .

-type flags() :: list(flag_id()).

-type value()
   :: list(atom())
      | true
   .

-type pattern_rule()
    :: {
        binary(), %% pattern without trainig * if it is present
        pattern(),
        IsWildCard :: boolean(), %% true if the pattern has training *
        Enabled :: flags(),
        Disabled :: flags()
    }.

-type by_pattern() :: [{pattern(), pattern_rule()}].

-type by_key() :: [{key(), value()}].

-type by_flag() :: [{flag_id(), [pattern()]}].

data_provider() ->
    {
        {flags, config},
        {callback_module, ?MODULE},
        [{interval, ?DATA_INTERVAL}]
    }.


enable(FlagId, Pattern) ->
    Key = atom_to_list(FlagId) ++ "||" ++ Pattern,
    config:set("feature_flags", Key, "true", false).

-spec data() -> by_key().

data() ->
    data(config:get("feature_flags")).

data(Config) ->
    ByPattern = collect_flags(Config),
    {Enabled, Disabled} = by_flag(ByPattern),
    lists:reverse(lists:usort(
        [{{enabled, Pattern}, E} || {Pattern, {_, E, _}} <- ByPattern]))
     ++
     lists:reverse(lists:usort(
        [{{disabled, Pattern}, D} || {Pattern, {_, _, D}} <- ByPattern]))
     ++
     lists:flatmap(fun({Flag, Patterns}) ->
        Sorted = lists:sort(fun sort_by_length/2, Patterns),
        [{{enabled_for, Flag, P}, {P, size(P)}} || P <- lists:reverse(Sorted)]
     end, Enabled)
     ++
     lists:flatmap(fun({Flag, Patterns}) ->
        Sorted = lists:sort(fun sort_by_length/2, Patterns),
        [{{disabled_for, Flag, P}, {P, size(P)}} || P <- lists:reverse(Sorted)]
     end, Disabled).


-spec by_flag(by_pattern()) -> {Enabled :: by_flag(), Disabled :: by_flag()}.

by_flag(ByPattern) ->
    {Enabled, Disabled} = lists:foldl(fun({Pattern, {_, E, D}}, {On, Off}) ->
        {append_pattern(On, E, Pattern), append_pattern(Off, D, Pattern)}
    end, {gb_trees:empty(), gb_trees:empty()}, ByPattern),
    {gb_trees:to_list(Enabled), gb_trees:to_list(Disabled)}.

append_pattern(Tree, Items, Element) ->
    lists:foldl(fun(Item, Acc) ->
        update_element(Acc, Item, [Element],
            fun(List) -> [Element | List] end
        )
    end, Tree, Items).

parse(Key, Value) ->
    parse(list_to_binary(Key), Key, Value).

parse(KeyBin, _Key, Value) ->
    case binary:split(KeyBin, [<<"||">>]) of
        [FlagBin, PatternBin] when Value =:= "true" ->
            {parse_pattern(PatternBin), [to_atom(FlagBin)], []};
        [FlagBin, PatternBin] when Value =:= "false" ->
            {parse_pattern(PatternBin), [], [to_atom(FlagBin)]};
        [_FlagBin, _PatternBin] ->
            error;
        _ ->
            case couch_util:parse_term(Value) of
                {ok, Flags} when is_list(Flags) ->
                    {parse_pattern(KeyBin), lists:usort(Flags), []};
                _ ->
                    error
            end
    end.

parse_pattern(PatternBin) ->
    PatternSize = size(PatternBin),
    case binary:last(PatternBin) of
        $* ->
            PrefixBin = binary:part(PatternBin, 0, PatternSize - 1),
            {PrefixBin, PatternBin, true, PatternSize - 1};
        _ ->
            {PatternBin, PatternBin, false, PatternSize}
    end.

collect_flags(ConfigData) ->
    Parsed = [parse(K, V) || {K, V} <- ConfigData],
    Acc = merge(Parsed),
    Keys = lists:sort(fun sort_by_length/2, gb_trees:keys(Acc)),
    FuzzyKeys = lists:sort(fun sort_by_length/2,
        [K || {K, {{_, _, true, _}, _, _}} <- gb_trees:to_list(Acc)]),
    Rules = collect_flags(lists:reverse(Keys), FuzzyKeys, Acc),
    gb_trees:to_list(Rules).

sort_by_length(A, B) ->
    size(A) =< size(B).

merge(Items) ->
    lists:foldl(fun({{_, K, _, _}, _, _} = Item, Acc) ->
        update_element(Acc, K, Item, fun(Value) ->
            update_flags(Value, Item)
        end)
    end, gb_trees:empty(), Items).

update_element(Tree, Key, Default, Fun) ->
    case gb_trees:lookup(Key, Tree) of
        none ->
            gb_trees:insert(Key, Default, Tree);
        {value, Value} ->
            gb_trees:update(Key, Fun(Value), Tree)
    end.

collect_flags([], _, Acc) ->
    Acc;
collect_flags([Current | Rest], Items, Acc) ->
    collect_flags(Rest, Items -- [Current], inherit_flags(Current, Items, Acc)).

inherit_flags(_Current, [], Acc) ->
    Acc;
inherit_flags(Current, [Item | Items], Acc) ->
    case match_prefix(Current, Item, Acc) of
        true ->
            inherit_flags(Current, Items, update_flags(Current, Item, Acc));
        false ->
            inherit_flags(Current, Items, Acc)
    end.

match_prefix(AKey, BKey, Acc) ->
    {value, A} = gb_trees:lookup(AKey, Acc),
    {value, B} = gb_trees:lookup(BKey, Acc),
    match_prefix(A, B).

match_prefix({{_, _, _, _}, _, _}, {{_, _, false, _}, _, _}) ->
    false;
match_prefix({{Key, _, _, _}, _, _}, {{Key, _, true, _}, _, _}) ->
    true;
match_prefix({{Key0, _, _, _}, _, _}, {{Key1, _, true, S1}, _, _}) ->
    case Key0 of
        <<Key1:S1/binary, _/binary>> -> true;
        _ -> false
    end.

update_flags(AKey, BKey, Acc) ->
    {value, A} = gb_trees:lookup(AKey, Acc),
    {value, B} = gb_trees:lookup(BKey, Acc),
    gb_trees:update(AKey, update_flags(A, B), Acc).

update_flags({Pattern, E0, D0}, {_, E1, D1}) ->
    DisabledByParent = lists:usort(D1 -- E0),
    E = lists:usort(lists:usort(E0 ++ E1) -- D0),
    D = lists:usort(D0 ++ DisabledByParent),
    {Pattern, E, D}.

to_atom(String) when is_list(String) ->
    list_to_atom(String);
to_atom(Binary) when is_binary(Binary) ->
    list_to_atom(binary_to_list(Binary)).


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

all_combinations_return_same_result_test_() ->
    Expected =
        {[
            {baz,[<<"shards/test/exact">>,<<"shards/test*">>]},
            {flag_bar,[<<"shards/test/exact">>,<<"shards/test*">>,<<"shards/exact">>]},
            {flag_foo,[<<"shards/test/exact">>,<<"shards/test*">>,<<"shards/exact">>,<<"*">>]}],
         [
            {flag_bar,[<<"shards/blacklist*">>,<<"*">>]},
            {flag_foo,[<<"shards/blacklist*">>]}
         ]},
    Combinations = couch_tests_combinatorics:permutations(test_config()),
    [{test_id(Items), ?_assertEqual(Expected, by_flag(collect_flags(Items)))}
        || Items <- Combinations].

rules_are_sorted_test() ->
    Expected = [
        {{enabled,<<"shards/test/exact">>},[baz,flag_bar,flag_foo]},
        {{enabled,<<"shards/test*">>},[baz,flag_bar,flag_foo]},
        {{enabled,<<"shards/exact">>},[flag_bar,flag_foo]},
        {{enabled,<<"shards/blacklist*">>},[]},
        {{enabled,<<"*">>},[flag_foo]},
        {{disabled,<<"shards/test/exact">>},[]},
        {{disabled,<<"shards/test*">>},[]},
        {{disabled,<<"shards/exact">>},[]},
        {{disabled,<<"shards/blacklist*">>},[flag_bar,flag_foo]},
        {{disabled,<<"*">>},[flag_bar]},
        {{enabled_for,baz,<<"shards/test/exact">>},
            {<<"shards/test/exact">>, 17}},
        {{enabled_for,baz,<<"shards/test*">>},
            {<<"shards/test*">>, 12}},
        {{enabled_for,flag_bar,<<"shards/test/exact">>},
            {<<"shards/test/exact">>, 17}},
        {{enabled_for,flag_bar,<<"shards/exact">>},
            {<<"shards/exact">>, 12}},
        {{enabled_for,flag_bar,<<"shards/test*">>},
            {<<"shards/test*">>, 12}},
        {{enabled_for,flag_foo,<<"shards/test/exact">>},
            {<<"shards/test/exact">>, 17}},
        {{enabled_for,flag_foo,<<"shards/exact">>},
            {<<"shards/exact">>, 12}},
        {{enabled_for,flag_foo,<<"shards/test*">>},
            {<<"shards/test*">>, 12}},
        {{enabled_for,flag_foo,<<"*">>},
            {<<"*">>, 1}},
        {{disabled_for,flag_bar,<<"shards/blacklist*">>},
            {<<"shards/blacklist*">>, 17}},
        {{disabled_for,flag_bar,<<"*">>},
            {<<"*">>, 1}},
        {{disabled_for,flag_foo,<<"shards/blacklist*">>},
            {<<"shards/blacklist*">>, 17}}
    ],
    ?assertEqual(Expected, data(test_config())).

test_id(Items) ->
    lists:flatten(io_lib:format("~p", [[P || {P, _} <- Items]])).

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

-endif.
