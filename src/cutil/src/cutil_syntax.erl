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

% Collection of helper functions to facilitate source code analysis and
% code generation. These include functions to work with:
% - `erl_syntax` trees
% - `merl` nodes
%

-module(cutil_syntax).

-export([
    quote/1,
    define_record/2,
    source/1,
    get_pos/1,
    term/2,
    variable/2,
    expand_records/2,
    parse_atom/2
]).

-include_lib("syntax_tools/include/merl.hrl").

-export_type([
    pos/0,
    tree/0,
    records/0
]).

-type pos() :: pos_integer() | {Line :: pos_integer(), Col :: pos_integer()}.
-type tree() :: erl_syntax:syntaxTree().
-type records() :: #{atom() := [atom()]}.


%% =====================================================================
%% @doc Parse text and return syntax tree. 

-spec quote(string()) -> tree() | {error, Reason :: term()}.

quote(String) ->
    try
        merl:quote(String)
    catch throw:Error ->
        Error
    end.


%% =====================================================================
%% @doc Defines a record. I.e. creates a syntax tree representing
%% `-record(myrecord, {field1 = something}).`

-spec define_record(RecordName :: atom(), FieldsQuoted :: tree()) -> tree().

define_record(RecordName, FieldsQuoted) when is_atom(RecordName) ->
    Pos = get_pos(FieldsQuoted),
    Name = term(Pos, RecordName),
    erl_syntax:set_pos(
        erl_syntax:revert(
            merl:quote("-record('@Name',{'@_FieldsQuoted'=[]}).")
        ),
    Pos).

%% =====================================================================
%% @doc Pretty print a given syntax tree

-spec source(Quoted :: tree()) -> string().

source(Quoted) when is_list(Quoted) ->
    Options = [{paper, 160}, {ribbon, 80}],
    erl_prettypr:format(erl_syntax:form_list(Quoted), Options);

source(Tree) ->
    source([Tree]).

%% =====================================================================
%% @doc Returns position of the term in a file

-spec get_pos(Tree :: tree()) -> pos().

get_pos(Tree) ->
    Pos = erl_syntax:get_pos(Tree),
    case erl_anno:is_anno(Pos) of
        true ->
            erl_anno:location(Pos);
        false ->
            Pos
    end.

%% =====================================================================
%% @doc Creates a syntax tree for a given term and sets specified position

-spec term(Pos :: pos(), T :: term()) -> tree().

term(Pos, T) ->
    erl_syntax:set_pos(erl_syntax:abstract(T), Pos).

%% =====================================================================
%% @doc Creates a syntax tree representing a variable with given name
%% and sets specified position

-spec variable(Pos :: pos(), Name :: atom()) -> tree().

variable(Pos, Name) ->
    erl_syntax:set_pos(erl_syntax:revert(erl_syntax:variable(Name)), Pos).

%% =====================================================================
%% @doc Takes a record expression and replaces it with a coresponding tuple
%% representation. If some fields are not mentioned in the passed expression,
%% the corresponding positions of the tuple would have '_' variable.
%%
%% The Records is a map #{record_name => list_of_fields}. The function
%% is designed to be called as
%% ```
%% cutil_ms:expand_records(merl:quote("#httpd{path_parts = [DB]}"), #{
%%   httpd => record_info(fields, httpd)
%% })`
%% The result would be a syntax tree which represent following expression
%% ```
%% {httpd, _, _, _, _, DB, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _}
%% ```
%%
%% The function returns `{Tree, Errors}`
%% The `Errors` contains a list of errors (usually `{record_not_found, RecordName}`)

-spec expand_records(Quoted :: tree(), Records :: records()) ->
    {tree(), Errors :: [term()]}.

expand_records(Quoted, Records) ->
    {Tree, Errors} = erl_syntax_lib:mapfold(fun(Node, Acc) ->
        case erl_syntax:type(Node) of
            record_expr ->
                {New, E} = expand_record_expr(Node, Records),
                {New, E ++ Acc};
            record_index_expr ->
                {New, E} = expand_record_index_expr(Node, Records),
                {New, E ++ Acc};
            record_access ->
                {New, E} = expand_record_access(Node, Records),
                {New, E ++ Acc};
            _ ->
                {Node, Acc}
        end
    end, [], Quoted),
    % we do usort for Errors to remove duplicates
    {erl_syntax:revert(Tree), lists:usort(Errors)}.

parse_atom(String, Error) ->
    Quoted = case quote(String) of
        ?Q("'@AtomAST'") when erl_syntax:type(AtomAST) == atom ->
            AtomAST;
        _ ->
            error
    end,
    case Quoted of
        error when is_function(Error) ->
            Error();
        error ->
            Error;
        Quoted ->
            erl_syntax:atom_value(Quoted)
    end.

% ----------
% Helper functions

expand_record_index_expr(Tree, Records) ->
    RecordName = erl_syntax:atom_value(erl_syntax:record_index_expr_type(Tree)),
    FieldName = erl_syntax:atom_value(erl_syntax:record_index_expr_field(Tree)),
    case maps:get(RecordName, Records, nil) of
        nil ->
            {Tree, [{record_not_found, RecordName}]};
        RecordFields ->
            % +1 for record name
            {erl_syntax:integer(idx_in_list(FieldName, RecordFields) + 1), []}
    end.


expand_record_access(Tree, Records) ->
    Argument = erl_syntax:record_access_argument(Tree),
    RecordName = erl_syntax:atom_value(erl_syntax:record_access_type(Tree)),
    FieldName = erl_syntax:atom_value(erl_syntax:record_access_field(Tree)),
    case maps:get(RecordName, Records, nil) of
        nil ->
            {Tree, [{record_not_found, RecordName}]};
        RecordFields ->
            % +1 for record name
            Idx = erl_syntax:integer(idx_in_list(FieldName, RecordFields) + 1),
            {?Q("element(_@Idx, _@Argument)"), []}
    end.


expand_record_expr(Tree, Records) ->
    RecordName = erl_syntax:atom_value(erl_syntax:record_expr_type(Tree)),
    case maps:get(RecordName, Records, nil) of
        nil ->
            {Tree, [{record_not_found, RecordName}]};
        RecordFields ->
            expand_record_expr(Tree, RecordFields, Records)
    end.


expand_record_expr(Tree, RecordFields, Records) ->
    Pos = get_pos(Tree),
    Type = erl_syntax:record_expr_type(Tree),
    {Fields, Errors} = lists:mapfoldl(fun(Node, Acc) ->
        Key = erl_syntax:record_field_name(Node),
        Value = erl_syntax:record_field_value(Node),
        case erl_syntax:type(Value) of
            variable ->
                {{erl_syntax:atom_value(Key), Value}, Acc};
            _ ->
                {New, E} = expand_records(Value, Records),
                {{erl_syntax:atom_value(Key), New}, E ++ Acc}
        end
    end, [], erl_syntax:record_expr_fields(Tree)),
    FieldsMap = maps:from_list(Fields),
    RFs = lists:map(fun(Name) ->
        case maps:get(Name, FieldsMap, nil) of
            nil ->
                underscore(Pos);
            FieldExpression ->
                FieldExpression
        end
    end, RecordFields),
    {erl_syntax:tuple([Type | RFs]), Errors}.


underscore(Pos) ->
    variable(Pos, '_').


idx_in_list(Value, List) ->
    case length(lists:takewhile(fun(K) -> K /= Value end, List)) of
        I when I =< length(List) ->
            % we add 1 because [] corresponds to Value in index 1
            I + 1;
        _ ->
            0
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("cutil_asserts.hrl").

expand_records_test() ->
    Records = #{
        record1 => [r1f1, r1f2],
        record2 => [r2f1, r2f2]
    },
    {Expanded1, Errors1} = expand_records(
        merl:quote("{#record1{r1f1 = #record2{r2f2 = B, r2f1 = 1}}}"), Records),
    ?cassertAST("Nested records", "{{record1, {record2, 1, B}, _}}", Expanded1),
    ?assertEqual([], Errors1),

    {Expanded2, Errors2} = expand_records(merl:quote("{#record1{r1f1 = #record2.r2f2}}"), Records),
    ?cassertAST("Record field index", "{{record1, 3, _}}", Expanded2),
    ?assertEqual([], Errors2),

    {Expanded3, Errors3} = expand_records(merl:quote("{M#record1.r1f1}"), Records),
    ?cassertAST("Named records access", "{element(2, M)}", Expanded3),
    ?assertEqual([], Errors3),

    {Expanded4, Errors4} = expand_records(merl:quote("{#record1{r1f1 = #record3.r2f2}}"), Records),
    ?cassertAST("Record field index", "{{record1, #record3.r2f2, _}}", Expanded4),
    ?assertEqual([{record_not_found,record3}], Errors4),
    ok.

-endif.