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

% This module serves two functions
% - provides public API to use to get value for a given feature flag and subject
% - implements {feature_flags, couch_flags} service

% The module rely on couch_epi_data_gen which uses the data returned by
% `couch_flags_config:data()` to generate callback module `couch_epi_data_gen_flags_config`.
% The generated module shouldn't be used directly. We use following APIs
% - `couch_epi:get_handle({flags, config})` - to get handler (name of generated module)
% - `couch_epi:get(Handle, Key) - to do efficient matching
%
% The generated module implements clauses like the following
%  - get(couch, {disabeled_for | enabled_for, binary_match_rule()} ->
%       [atom()] | undefined;
%  - get(couch, {disabeled | enabled, binary_match_rule()} ->
%       [atom()];
% For example
%  - get(couch, {disabled_for, flag_bar, <<"shards/blacklist", _/binary>>}) ->
%       {<<"shards/blacklist", 17};
%  - get(couch, {enabled_for, flag_foo, <<"shards/test", _/binary>>}) ->
%       {<<"shards/test">>, 12};
%  - get(couch, {disabled, <<"shards/blacklist", _/binary>>}) ->[flag_bar,flag_foo];
%  - get(couch, {disabled, <<"", _/binary>>}) ->[flag_bar];
%  - get(couch, {enabled, <<"shards/test/exact">>}) ->[baz,flag_bar,flag_foo];
%  - get(couch, {enabled, <<"shards/test", _/binary>>}) ->[baz,flag_bar,flag_foo];
%  - get(_, _) -> undefined.
%
% The `couch_epi:get/2` uses the Handler module to implement efficient matching.

-module(couch_flags).

%% Public API
-export([
     enabled/1,
     disabled/1,
     is_enabled/2,
     extract_subject_key/1
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").
-include("couch_db_int.hrl").

-define(SERVICE_ID, feature_flags).
-define(DATA_INTERVAL, 1000).

enabled(Subject) ->
    maybe_handle(enabled, [Subject], fun default_enabled_flags/1).

disabled(Subject) ->
    maybe_handle(disabled, [Subject], fun default_disabled_flags/1).

is_enabled(FlagId, Subject) ->
    maybe_handle(is_enabled, [FlagId, Subject], fun default_is_enabled/2).

default_enabled_flags(Subject) ->
    Key = extract_subject_key(Subject),
    lists:usort(enabled_flags(Key)
        ++ enabled_flags(couch_db:normalize_dbname(Key))).

default_disabled_flags(Subject) ->
    Key = extract_subject_key(Subject),
    lists:usort(disabled_flags(Key)
        ++ disabled_flags(couch_db:normalize_dbname(Key))).

default_is_enabled(FlagId, Subject) ->
    Key = extract_subject_key(Subject),
    is_flag_enabled(FlagId, Key)
        orelse is_flag_enabled(FlagId, couch_db:normalize_dbname(Key)).


enabled_flags(Key) ->
    Handle = couch_epi:get_handle({flags, config}),
    lists:flatten(couch_epi:get(Handle, {enabled, Key}))
        -- lists:flatten(couch_epi:get(Handle, {disabled, Key})).

disabled_flags(Key) ->
    Handle = couch_epi:get_handle({flags, config}),
    lists:flatten(couch_epi:get(Handle, {disabled, Key}))
        -- lists:flatten(couch_epi:get(Handle, {enabled, Key})).

is_flag_enabled(FlagId, Key) ->
    Handle = couch_epi:get_handle({flags, config}),
    Enabled = lists:max([-1 |
                         [S || {_, S} <- couch_epi:get(Handle, {enabled_for, FlagId, Key})]]),
    Disabled = lists:max([-1 |
                          [S || {_, S} <- couch_epi:get(Handle, {disabled_for, FlagId, Key})]]),
    Enabled > 0 andalso (Enabled > Disabled).

%% how to support clustered dbs?
extract_subject_key(#db{name = Name}) ->
    to_binary(Name);
extract_subject_key(#httpd{path_parts=[Name | _Rest]}) ->
    to_binary(Name);
extract_subject_key(#shard{name = Name}) ->
    to_binary(Name);
extract_subject_key(#ordered_shard{name = Name}) ->
    to_binary(Name);
extract_subject_key(Name) when is_list(Name) ->
    to_binary(Name);
extract_subject_key(Name) when is_binary(Name) ->
    Name.

to_binary(List) when is_list(List) ->
    list_to_binary(List);
to_binary(Binary) when is_binary(Binary) ->
    Binary.

maybe_handle(Func, Args, Default) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    case couch_epi:decide(Handle, ?SERVICE_ID, Func, Args, []) of
        no_decision when is_function(Default) ->
            apply(Default, Args);
        {decided, Result} ->
            Result
    end.
