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

% Functions to parse string representation of a function into dbg namespec
% @see https://erlang.org/doc/apps/erts/match_spec.html

-module(cutil_ms).

-export([
    str2ms/1,
    str2ms/2,
    syntax2ms/1,
    syntax2ms/2
]).

-export_type([
    ms/0
]).
-type ms() :: term().

-include_lib("syntax_tools/include/merl.hrl").

-ifdef(TEST).
-record(test_record, {
    foo = 1 :: integer(),
    bar = [] :: list()
}).
-endif.

%% =====================================================================
%% @doc Parse string representation of a match function into matchspec
%%
%% We expect sting as follows
%%   `([A, B]) when B == true andalso B < 10 -> return_trace()`
%% Notice that there is neither `end` or `.` at the end of the expression.
%% This is equivalent to `str2ms(Quoted, [])`, but potentially more efficient.
%%
%% @see str2ms/2

-spec str2ms(String :: string()) -> 
    ms() | {error, Reason :: term()}.

str2ms(String) ->
    case cutil_syntax:quote(String) of
        {error, _} = Error ->
            Error;
        Tree ->
            syntax2ms(Tree)
    end.

%% =====================================================================
%% @doc Parse string representation of a match function into matchspec
%%
%% We expect sting as follows
%%   `([A, B]) when B == true andalso B < 10 -> return_trace()`
%% Notice that there is neither `end` or `.` at the end of the expression.
%%
%% The Records is a map #{record_name => list_of_fields}. The function
%% is designed to be called as
%% ```
%% cutil_ms:str2ms("([#httpd{path_parts = [DB]}]) when DB == <<"mydb">> -> return_trace()", #{
%%   chttpd => record_info(fields, httpd)
%% })
%% ```
%%
%% This is equivalent to `syntax2ms(merl:quote(String), Records)`
%% @see syntax2ms/2

-spec str2ms(String :: string(), Records :: cutil_syntax:records()) ->
    ms() | {error, Reason :: term()}.
str2ms(String, Records) ->
    case cutil_syntax:quote(String) of
        {error, _} = Error ->
            Error;
        Tree ->
            syntax2ms(Tree, Records)
    end.


%% =====================================================================
%% @doc Convert merl quoted expression into matchspec
%%
%% The quoted expression we expect is representing the following
%%   `([A, B]) when B == true andalso B < 10 -> return_trace()`
%% Notice that there is neither `end` or `.` at the end of the expression.
%% This is equivalent to `syntax2ms(Quoted, [])`, but potentially more efficient.
%%
%% @see syntax2ms/2

-spec syntax2ms(Quoted :: cutil_syntax:quoted()) -> 
    ms() | {error, Reason :: term()}.

syntax2ms(Quoted) ->
    case ms_transform:transform_from_shell(dbg, [Quoted], []) of
        MS when is_list(MS) ->
            MS;
        Else ->
            Else
    end.

%% =====================================================================
%% @doc Convert merl quoted expression containing records into matchspec
%%
%% The quoted expression we expect is representing the following
%%   `([A, B]) when B == true andalso B < 10 -> return_trace()`
%% Notice that there is neither `end` or `.` at the end of the expression.
%%
%% The Records is a map #{record_name => list_of_fields}. The function
%% is designed to be called as
%% ```
%% cutil_ms:syntax2ms(merl:quote("([#httpd{path_parts = [DB]}]) when DB == <<"mydb">> -> return_trace()"), #{
%%   chttpd => record_info(fields, httpd)
%% })
%% ```
%%
%% @see syntax2ms/2

-spec syntax2ms(QuotedClause :: cutil_syntax:quoted(), Records :: cutil_syntax:records()) -> 
    ms() | {error, Reason :: term()}.
syntax2ms(QuotedClause, Records) ->
    case cutil_syntax:expand_records(QuotedClause, Records) of
        {Expanded, []} ->
            case ms_transform:transform_from_shell(dbg, [Expanded], []) of
                MS when is_list(MS) ->
                    MS;
                Else ->
                    Else
            end;
        {_, Errors} ->
            {error, Errors}
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

record_test() ->
    Str = "([#test_record{bar = B}]) when B == true -> return_trace()",
    MS = str2ms(Str, #{test_record => record_info(fields, test_record)}),
    ?assertEqual([{[{test_record,'_','$1'}],[{'==','$1',true}],[{return_trace}]}], MS),
    ok.


nested_record_test() ->
    Str = "([#test_record{bar = #bar{baz = B}}]) when B == true -> return_trace()",
    MS = str2ms(Str, #{
        test_record => record_info(fields, test_record),
        bar => [baz]
    }),
    ?assertEqual([{[{test_record,'_',{bar,'$1'}}],[{'==','$1',true}],[{return_trace}]}], MS),
    ok.


map_test() ->
    Str = "([#{bar := B}]) when B == true -> return_trace()",
    MS1 = str2ms(Str, #{}),
    ?assertEqual([{[#{bar => '$1'}],[{'==','$1',true}],[{return_trace}]}], MS1),
    MS2 = str2ms(Str),
    ?assertEqual([{[#{bar => '$1'}],[{'==','$1',true}],[{return_trace}]}], MS2),
    ok.

-endif.