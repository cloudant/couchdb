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

-module(cutil_trace_book).

-export([
    parse_tracebook/2,
    parse_points/2
]).

-type id() :: binary().
-type ms() :: [{Head :: list(), Conditions :: list(), Body :: list()}].
-type rule() :: {mfa(), ms(), string()}.
-type point() :: {Trigger :: rule(), Targets :: #{id() => rule()}}.
-type trace_book() :: #{id := binary(), points := list(point())}.

-spec parse_tracebook(JSONObj :: cutil_json:json_object(), cutil_syntax:records()) ->
    {trace_book(), Errors :: [Reason :: term()]}.

parse_tracebook(JSONObj, Records) ->
    [BookId, Points] = cutil_json:take_keys(
        JSONObj, [<<"id">>, <<"points">>], [undefined, []]),
    case BookId of
        undefine ->
            {#{}, [{missing_key, <<"id">>}]};
        BookId ->
            {Results, Errors} = parse_points(Points, Records),
            Book = #{
                id => BookId,
                points => Results
            },
            {Book, Errors}
    end.


%% Expects list of JSON objects of the following structure
%% {[
%%     {<<"trigger">>, TriggerSrc :: string()},
%%     {<<"targets">>, [TargetSrc :: string()]}
%% ]}

-spec parse_points(cutil_json:json_object(), cutil_syntax:records()) ->
    {Parsed :: #{TriggerMFA :: mfa() => point()}, Errors :: list(Reason :: term())}.

parse_points(Points, Records) when is_list(Points) ->
    {_, Results, Errors} = lists:foldl(fun(PointObj, {Idx, Acc, ErrorsAcc}) ->
        [TriggerStr, Targets] = cutil_json:take_keys(
            PointObj, [<<"trigger">>, <<"targets">>], [undefined, []]),
        case TriggerStr of
            undefined ->
                {Idx + 1, Acc, [{missing_key, <<"trigger">>, in, PointObj} | ErrorsAcc]};
            TriggerStr ->
                case parse_point(TriggerStr, Targets, Records) of
                    {error, Reason} ->
                        {Idx + 1, Acc, [{in, TriggerStr, Reason} | ErrorsAcc]};
                    {{TriggerMFA, _, _}, _} = Point ->
                        {Idx + 1, maps:put(TriggerMFA, Point, Acc), ErrorsAcc}
                end
        end
    end, {1, #{}, []}, Points),
    {Results, Errors}.


-spec parse_point(
        TriggerStr :: string(),
        Targets :: list(string()),
        cutil_syntax:records()
    ) -> point().

parse_point(TriggerStr, Targets, Records) ->
    case parse_rule(TriggerStr, Records) of
        {ok, TriggerRule} ->
            case parse_targets(Targets, Records) of
                {ParsedTargets, []} ->
                    {TriggerRule, ParsedTargets};
                {_, Errors} ->
                    {error, Errors}
            end;
        {error, _} = Error ->
            Error
    end.


-spec parse_targets(Targets :: list(string()), cutil_syntax:records()) ->
    {list(rule()), Errors :: [Reason :: term()]}.

parse_targets(Targets, Records) ->
    {_, Parsed, Errors} = lists:foldl(fun(TargetStr, {Idx, Acc, ErrorsAcc}) ->
        case parse_target(TargetStr, Records) of
            {error, Reason} ->
                {Idx + 1, Acc, [Reason | ErrorsAcc]};
            {Id, Target} ->
                {Idx + 1, maps:put(Id, Target, Acc), ErrorsAcc}
        end
    end, {1, #{}, []}, Targets),
    {Parsed, Errors}.


-spec parse_target(TargetSrc :: string(), cutil_syntax:records()) ->
    {id(), rule()} | {error, Reason :: term()}.

parse_target(TargetStr, Records) ->
    case parse_rule(TargetStr, Records) of
        {error, _} = Error ->
            {in, TargetStr, Error};
        {ok, {TargetMFA, MS, _} = Target} ->
            TargetId = erlang:md5(term_to_binary({TargetMFA, MS})),
            {TargetId, Target}
    end.


-spec parse_rule(binary(), cutil_syntax:records()) ->
    {ok, rule()} | {error, Reason :: term()}.

parse_rule(String, Records)  ->
    Steps = [
        {"split module:function", fun(S) ->
            case string:split(S, ":") of
                [Module, Function] ->
                    {ok, {Module, Function}};
                _ ->
                    {error, "expecting to find ':' character"}
            end
        end},
        {"parse module name", fun({ModuleString, Function}) ->
            case cutil_syntax:parse_atom(ModuleString, {error}) of
                {error} ->
                    {error, binary_to_list(<<"'", ModuleString/binary, "' doesn't look like atom">>)};
                Module ->
                    {ok, {Module, Function}}
            end
        end},
        {"extract function name", fun({Module, FunctionDef}) ->
            case string:split(FunctionDef, "(") of
                [FunctionName, FunctionClause] ->
                    {ok, {Module, FunctionName, <<"(", FunctionClause/binary>>}};
                _ ->
                    {error, "expecting to find '(' character"}
            end
        end},
        {"parse function name", fun({Module, FunctionNameStr, FunctionDef}) ->
            case cutil_syntax:parse_atom(FunctionNameStr, {error}) of
                {error} ->
                    {error, binary_to_list(<<"'", FunctionNameStr/binary, "' doesn't look like atom">>)};
                Function ->
                    {ok, {Module, Function, FunctionDef}}
            end
        end},
        {"parse function clause", fun({Module, Function, FunctionClause}) ->
            case str2ms(remove_trailing_dot(FunctionClause), Records) of
                [{Args, _Guards, _Body}] = MS ->
                    {ok, {{Module, Function, length(Args)}, MS, String}};
                {error, [{"TOP_LEVEL", _}], _} ->
                    {error, "expecting single list argument '([_, _, ...]) -> ...'"};
                {error, Reason} ->
                    {error, Reason}
            end
        end}
    ],
    lists:foldl(fun
        ({StepName, _StepFun}, {error, Reason}) ->
            {error, {"Cannot " ++ StepName, Reason}};
        ({_StepName, StepFun}, {ok, Acc}) ->
            StepFun(Acc);
        ({_StepName, StepFun}, Acc) ->
            {ok, StepFun(Acc)}
    end, {ok, String}, Steps).


remove_trailing_dot(Binary) ->
    string:trim(Binary, trailing, " \n\r\t.").




str2ms(String, Records) ->
    try
        cutil_ms:str2ms(String, Records)
    catch
        throw:{error, _} = Error ->
            Error;
        Kind:Error ->
            {error, {Kind, Error}}
    end.
