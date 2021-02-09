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
%
% This module implements API similar to the one provided by persistent_term
% The implementation relies on dynamic generation of a module with following structure
%
% ```
% -module(NS).
% -export([to_list/0, get/1, to_map/0, keys/0, info/0]).
% to_list() ->
%    [{key1, 1}, {key2, 2}].
% get(key1) ->
%    {ok, 1};
% get(key2) ->
%    {ok, 2};
% get(_) ->
%    undefined.
% to_map() ->
%    #{key1 => 1, key2 => 2}.
% keys() ->
%    [key1, key2].
% info() ->
%    #{count => 2}.
% ```
%
% As you can see the same data is stored multiple times.
% Because the optimization goal is access time.
%
% The major differences from persistent_term module are
% - presence of additional operations
% - lack of `memory` field in the response from `info()` call.
% - additional parameter for each API function to expecitelly provide a namespace
% The namespace parameter is used as a name for generated module.
% Do NOT use ?MODULE as a namespace.
%
% In terms of memory the module overhead is about 656 bytes

-module(cutil_term).

-export([
    erase_all/1,
    erase/2,
    to_list/1,
    get/2,
    get/3,
    merge/2,
    put/3,
    pop/2,
    info/1,
    keys/1,
    to_map/1
]).

% this value is calculated by compiling a module without any terms stored.
-define(MODULE_OVERHEAD, 644).

-include_lib("syntax_tools/include/merl.hrl").

-type ns() :: module().
-type key() :: term().
-type value() :: term().
-type error(_Error) :: no_return().
-type beam_size() :: pos_integer().

-spec erase_all(ns()) -> boolean().

erase_all(NS) ->
    code:purge(NS),
    code:delete(NS),
    true.


-spec erase(ns(), key()) -> boolean().

erase(NS, Key) ->
    maps:size(pop(NS, [Key])) /= 0.


-spec to_list(ns()) -> [{key(), value()}].

to_list(NS) ->
    case erlang:function_exported(NS, to_list, 0) of
        true -> NS:to_list();
        false -> []
    end.


-spec get(ns(), key()) -> value() | error(badarg).

get(NS, Key) ->
    case erlang:function_exported(NS, get, 1) of
        true ->
            case NS:get(Key) of
                {ok, Value} -> Value;
                _ -> erlang:error(badarg)
            end;
        false ->
            erlang:error(badarg)
    end.


-spec get(ns(), key(), Default :: value()) -> value().

get(NS, Key, Default) ->
    case erlang:function_exported(NS, get, 1) of
        true ->
            case NS:get(Key) of
                {ok, Value} -> Value;
                _ -> Default
            end;
        false ->
            Default
    end.


-spec info(ns()) -> #{count := non_neg_integer()}.

info(NS) ->
    % here we different from persisten_term API
    % we don't have `memory` field.
    % we could have it if we compile twice
    % however it would slow down the update of a term
    case erlang:function_exported(NS, info, 0) of
        true -> NS:info();
        false -> #{count => 0}
    end.


-spec to_map(ns()) -> #{key() => value()}.

to_map(NS) ->
    case erlang:function_exported(NS, to_map, 0) of
        true -> NS:to_map();
        false -> #{}
    end.


-spec pop(ns(), [key()]) -> #{key() => value()}.

pop(NS, Keys) ->
    Map = to_map(NS),
    Values = maps:with(Keys, Map),
    case maps:size(Values) == 0 of
        true ->
            ok;
        false ->
            store(NS, maps:without(Keys, Map))
    end,
    Values.


-spec merge(ns(), [{key(), term()}] | #{key() => value()}) -> ok.

merge(NS, KVs) when is_list(KVs) ->
    merge(NS, maps:from_list(KVs));

merge(NS, Map) ->
    Current = to_map(NS),
    case maps:merge(Current, Map) of
        Current ->
            ok;
        Updated ->
            store(NS, Updated),
            ok
    end.


- spec put(ns(), key(), value()) -> ok.

put(NS, Key, Value) ->
    case get(NS, Key, undefined) of
        Value ->
            ok;
        _ ->
            store(NS, maps:put(Key, Value, to_map(NS))),
            ok
    end.


-spec keys(ns()) -> [key()].

keys(NS) ->
    case erlang:function_exported(NS, keys, 0) of
        true -> NS:keys();
        false -> []
    end.


-spec store(ns(), #{key() => value()}) -> beam_size().

store(NS, Map) ->
    AST = generate(NS, Map),
    {ok, Beam} = merl:compile_and_load(AST),
    erlang:byte_size(Beam).


-spec generate(module(), #{key() => value()}) -> erl_syntax:forms().

generate(ModuleName, Map) ->
    AsList = maps:to_list(Map),
    Keys = maps:keys(Map),
    Info = #{
        count => maps:size(Map)
    },
    Module = ?Q("-module('@ModuleName@')."),
    Export = ?Q("-export([to_list/0, get/1, to_map/0, keys/0, info/0])."),
    ToListFun = erl_syntax:function(merl:term(to_list), [
        ?Q("() -> _@AsList@")
    ]),
    Get1Fun = erl_syntax:function(merl:term(get), [
        ?Q("(_@Key@) -> {ok, _@Value@}")
        || {Key, Value} <- AsList
    ] ++ [?Q("(_@) -> undefined")]),
    ToMapFun = erl_syntax:function(merl:term(to_map), [
        ?Q("() -> _@Map@")
    ]),
    KeysFun = erl_syntax:function(merl:term(keys), [
        ?Q("() -> _@Keys@")
    ]),
    InfoFun = erl_syntax:function(merl:term(info), [
        ?Q("() -> _@Info@")
    ]),
    lists:flatten([Module, Export, ToListFun, Get1Fun, ToMapFun, KeysFun, InfoFun]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

size_test() ->
    EmptySize = store(foo, #{}),
    %% should be very close to ?MODULE_OVERHEAD let's assume 5% error
    ?assert(?MODULE_OVERHEAD =< EmptySize
        andalso EmptySize < ?MODULE_OVERHEAD * 1.05),

    IncreasingSize = [
        #{1 => 1},
        #{1 => 1, 2 => 2},
        #{1 => 1, 2 => 2, 3 => 3},
        #{1 => 1, 2 => 2, 3 => 3, 4 => 4},
        #{1 => 1, 2 => 2, 3 => 3, 4 => 4, 5 => 5},
        #{1 => 1, 2 => 2, 3 => 3, 4 => 4, 5 => 5, 6 => 6},
        #{1 => 1, 2 => 2, 3 => 3, 4 => 4, 5 => 5, 6 => 6, 7 => 7},
        #{1 => 1, 2 => 2, 3 => 3, 4 => 4, 5 => 5, 6 => 6, 7 => 7, 8 => 8},
        maps:from_list([{I, I} || I <- lists:seq(1, 1000)])
    ],
    % ensure the size of the module is growing as we inset more keys
    [First | Rest] = Sizes = [store(foo, Map) || Map <- IncreasingSize],
    {Res, _} = lists:mapfoldl(fun(Size, Acc) ->
        {Size > Acc, Size}
    end, First, Rest),
    ?assertEqual([true], lists:usort(Res)),
    ok.


put_test() ->
    erase_all(foo),
    ok = put(foo, key1, 1),
    ?assertEqual({ok, 1}, foo:get(key1)),
    ?assertEqual(undefined, foo:get(not_present)),
    ?assertEqual([key1], foo:keys()),
    ?assertEqual([{key1, 1}], foo:to_list()),
    ?assertEqual(#{key1 => 1}, foo:to_map()),
    ?assertEqual(#{count => 1}, foo:info()),

    % make sure insert of the same is noop
    ok = put(foo, key1, 1),
    ?assertEqual({ok, 1}, foo:get(key1)),
    ?assertEqual(#{count => 1}, foo:info()),

    % make sure we can add another element and first is still there
    ok = put(foo, key2, 2),
    ?assertEqual({ok, 1}, foo:get(key1)),
    ?assertEqual({ok, 2}, foo:get(key2)),
    ?assertEqual(undefined, foo:get(not_present)),
    ?assertEqual([key1, key2], foo:keys()),
    ?assertEqual([{key1, 1}, {key2, 2}], foo:to_list()),
    ?assertEqual(#{key1 => 1, key2 => 2}, foo:to_map()),
    ?assertEqual(#{count => 2}, foo:info()),
    ok.


get_test() ->
    erase_all(foo),
    store(foo, #{key1 => 1}),
    ?assertEqual(1, get(foo, key1)),
    ?assertError(badarg, get(foo, not_present)),
    ?assertEqual(default, get(foo, not_present, default)),
    ok.


erase_test() ->
    erase_all(foo),
    store(foo, #{
        key1 => 1,
        key2 => 2
    }),
    ?assertEqual({ok, 1}, foo:get(key1)),
    ?assertEqual(#{count => 2}, foo:info()),
    ?assertEqual(true, erase(foo, key1)),
    ?assertEqual(#{count => 1}, foo:info()),
    ?assertEqual(false, erase(foo, key1)),
    ?assertEqual(#{count => 1}, foo:info()),
    ?assertEqual(undefined, foo:get(key1)),
    ?assertEqual({ok, 2}, foo:get(key2)),
    ok.


pop_test() ->
    erase_all(foo),
    store(foo, #{
        key1 => 1,
        key2 => 2
    }),
    ?assertEqual({ok, 1}, foo:get(key1)),
    ?assertEqual(#{count => 2}, foo:info()),
    ?assertEqual(#{key1 => 1}, pop(foo, [key1])),
    ?assertEqual(#{count => 1}, foo:info()),
    ?assertEqual(undefined, foo:get(key1)),
    ?assertEqual({ok, 2}, foo:get(key2)),
    ok.


merge_test() ->
    erase_all(foo),
    store(foo, #{
        key1 => 1,
        key2 => 2
    }),
    ?assertEqual(#{count => 2}, foo:info()),
    ok = merge(foo, #{
        key1 => 4,
        key3 => 3
    }),
    ?assertEqual({ok, 4}, foo:get(key1)),
    ?assertEqual({ok, 2}, foo:get(key2)),
    ?assertEqual({ok, 3}, foo:get(key3)),
    ?assertEqual(#{count => 3}, foo:info()),
    ok.


keys_test() ->
    erase_all(foo),
    ?assertEqual([], keys(foo)),
    store(foo, #{
        key1 => 1,
        key2 => 2
    }),
    ?assertEqual([key1, key2], keys(foo)),
    ok.


to_map_test() ->
    erase_all(foo),
    ?assertEqual(#{}, to_map(foo)),
    store(foo, #{
        key1 => 1,
        key2 => 2
    }),
    ?assertEqual(#{key1 => 1, key2 => 2}, to_map(foo)),
    ok.


to_list_test() ->
    erase_all(foo),
    ?assertEqual([], to_list(foo)),
    store(foo, #{
        key1 => 1,
        key2 => 2
    }),
    ?assertEqual([{key1, 1}, {key2, 2}], to_list(foo)),
    ok.

-endif.