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
-module(cutil_json).

-export([
    pop_keys/2,
    pop_keys/3,
    pop/2,
    pop/3,
    take_keys/2,
    take_keys/3,
    get/2,
    get/3,
    is_json_object/1
]).

-type json_value() :: null
                    | true
                    | false
                    | json_string()
                    | json_number()
                    | json_object()
                    | json_array().

-type json_array()  :: [json_value()].
-type json_string() :: atom() | binary().
-type json_number() :: integer() | float().

-type json_object() :: {[{json_string(), json_value()}]}
                        | #{json_string() => json_value()}.

-type error(_Error) :: no_return().

%% Takes all entries corresponding to the given keys and returns them as a list.
%% The keys in the returned list are in the order of keys.
%% The taken entries are removed from the original json_object().
%% If the key is not found the the default value `undefined` is returned.

-spec pop_keys(
        json_object(),
        Keys :: list(json_string())
    ) -> {list(json_value()), json_object()}.

pop_keys(JSON, Keys) ->
    pop_keys(JSON, Keys, [undefined || _ <- Keys]).

%% Takes all entries corresponding to the given keys and returns them as a list.
%% The keys in the returned list are in the order of keys.
%% The taken entries are removed from the original json_object().
%% If the key is not found the the default value is returned.
%% The function can raise an {badarg, non_equal_length} error in case when
%% given number of keys do not match the given number of defaults.

-spec pop_keys(
        json_object(),
        Keys :: list(json_string()),
        Defaults :: list(json_value())
    ) -> {list(json_value()), json_object()}
        | error({badarg, Reason :: term()}).

pop_keys(JSONObj, Keys, Defaults) when is_list(Keys) andalso is_list(Defaults) ->
    {Taken, {Extra, NewProps}} = lists:mapfoldr(fun
        (Key, {[Default | Rest], Props}) ->
            {Value, NewProps} = pop(Props, Key, Default),
            {Value, {Rest, NewProps}};
        (_, {[], _}) ->
            error({badarg, non_equal_length})
    end, {Defaults, JSONObj}, Keys),
    Extra == [] orelse error({badarg, non_equal_length}),
    {Taken, NewProps};

pop_keys(_Props, Keys, _Defaults) when not is_list(Keys) ->
    error({badarg, {non_list, keys}});

pop_keys(_Props, _Keys, Defaults) when not is_list(Defaults) ->
    error({badarg, {non_list, defaults}}).

get(Props, Key) ->
    get(Props, Key, undefined).

get({Props}, Key, Default) when is_list(Props) ->
    case lists:keyfind(Key, 1, Props) of
        {Key, Value} ->
            Value;
        false ->
            Default
    end;

get(Props, Key, Default) when is_map(Props) ->
    maps:get(Key, Props, Default);

get(_Props, _Key, _Default) ->
    error({badarg, non_json_object}).

pop(Props, Key) ->
    pop(Props, Key, undefined).

pop({Props}, Key, Default) when is_list(Props) ->
    case lists:keytake(Key, 1, Props) of
        {value, {Key, Value}, NewProps} ->
            {Value, NewProps};
        false ->
            {Default, Props}
    end;

pop(Props, Key, Default) when is_map(Props) ->
    case maps:take(Key, Props) of
        {Value, NewProps} ->
            {Value, NewProps};
        error ->
            {Default, Props}
    end;

pop(_, _, _) ->
    error({badarg, non_json_object}).


%% Return all entries corresponding to the given keys as a list in the order of keys.
%% If the key is not found the the default value `undefined` is returned.

-spec take_keys(
        json_object(),
        Keys :: list(json_string())
    ) -> list(json_value()).

take_keys(JSONObj, Keys) ->
    take_keys(JSONObj, Keys, [undefined || _ <- Keys]).

%% Return all entries corresponding to the given keys as a list in the order of keys.
%% If the key is not found the the default value is returned.
%% The function can raise an {badarg, non_equal_length} error in case when
%% given number of keys do not match the given number of defaults.

-spec take_keys(
        json_object(),
        Keys :: list(json_string()),
        Defaults :: list(json_value())
    ) -> list(json_value())
        | error({badarg, Reason :: term()}).

take_keys(Props, Keys, Defaults) when is_list(Keys) andalso is_list(Defaults) ->
    {Taken, Extra} = lists:mapfoldr(fun
        (Key, [Default | Rest]) ->
            {get(Props, Key, Default), Rest};
        (_, []) ->
            error({badarg, non_equal_length})
    end, Defaults, Keys),
    Extra == [] orelse error({badarg, non_equal_length}),
    Taken;

take_keys(_Props, Keys, _Defaults) when not is_list(Keys) ->
    error({badarg, {non_list, keys}});

take_keys(_Props, _Keys, Defaults) when not is_list(Defaults) ->
    error({badarg, {non_list, defaults}});

take_keys(_, _, _) ->
    error({badarg, non_json_object}).

is_json_object({Props}) when is_list(Props) ->
    true;
is_json_object(Props) when is_map(Props) ->
    true;
is_json_object(_) ->
    false.