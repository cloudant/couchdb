-module(couch_ejson_size).

-export([ejson_size/1]).

-ifdef(TEST).
    -compile(export_all).

    -include_lib("proper/include/proper.hrl").
-endif.


-define(quotation_mark, 16#0022).
-define(reverse_solidus, 16#005C).
-define(solidus, 16#002F).
-define(backspace, 16#0008).
-define(horizontal_tab, 16#0009).
-define(newline, 16#000A).
-define(formfeed, 16#000C).
-define(carriage_return, 16#000D).

ejson_size({[]}) ->
    2;  % opening { and closing }

ejson_size({Props}) ->
    % 2 is because opening { and closing }
    % Inside the lc 2 is for : and , but counts an extra , at end
    % -1 is to subtract last , from lc part
    2 + lists:sum([ejson_size(K) + ejson_size(V) + 2 || {K, V} <- Props]) - 1;

ejson_size([]) ->
    2;  % opening [ and closing ]

ejson_size(List) when is_list(List) ->
    % 2 is for [ and ]
    % inside the lc 1 is for , but it counts one extra , for last element
    2 + lists:sum([ejson_size(V) + 1 || V <- List]) - 1;

ejson_size(Float) when is_float(Float) ->
    erlang:byte_size(jiffy:encode(Float));

ejson_size(0) ->
    1; % log(0) is not defined

ejson_size(Integer) when is_integer(Integer), Integer > 0 ->
    trunc(math:log10(Integer)) + 1;

ejson_size(Integer) when is_integer(Integer), Integer < 0 ->
    % 2 is because 1 is for the - character
    trunc(math:log10(-Integer)) + 2;

ejson_size(Binary) when is_binary(Binary) ->
    utf8_string_size(Binary);

ejson_size(null) ->
    4;

ejson_size(true) ->
    4;

ejson_size(false) ->
    5;

ejson_size(Atom) when is_atom(Atom) ->
    ejson_size(atom_to_binary(Atom, utf8)).


utf8_string_size(Binary) ->
    %% 2 is for open and closing "
    utf8_string_size(Binary, 2).

utf8_string_size(<<0:1, C:7/integer, Rest/binary>>, Acc) ->
    case C of
        ?quotation_mark -> utf8_string_size(Rest, Acc + 2);
        ?reverse_solidus -> utf8_string_size(Rest, Acc + 2);
        ?backspace -> utf8_string_size(Rest, Acc + 2);
        ?horizontal_tab -> utf8_string_size(Rest, Acc + 2);
        ?newline -> utf8_string_size(Rest, Acc + 2);
        ?formfeed -> utf8_string_size(Rest, Acc + 2);
        ?carriage_return -> utf8_string_size(Rest, Acc + 2);
        NeedEscape when NeedEscape =< 16#001F -> utf8_string_size(Rest, Acc + 6);
        _Else -> utf8_string_size(Rest, Acc + 1)
    end;
%% 10000 .. 10000007F
utf8_string_size(<<2#1111:4, _:28/bitstring, Rest/binary>>, Acc) ->
    utf8_string_size(Rest, Acc + 4);
%% 0800 .. FFFF
utf8_string_size(<<2#111:3, _:21/bitstring, Rest/binary>>, Acc) ->
    utf8_string_size(Rest, Acc + 3);
%% 0080 .. 07ff
utf8_string_size(<<2#11:2, _:14/bitstring, Rest/binary>>, Acc) ->
    utf8_string_size(Rest, Acc + 2);
utf8_string_size(<<>>, Acc) ->
    Acc.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

proper_test_() ->
    PropErOpts = [
        {to_file, user},
        {max_size, 5},
        {numtests, 1000}
    ],
    properties_tests(properties(), 30, PropErOpts).

properties() ->
    Exported = ?MODULE:module_info(exports),
    PropertiesFilter = fun({FunName, Arrity}) ->
        Arrity =:= 0 andalso lists:prefix("prop_", atom_to_list(FunName))
    end,
    [F || {F, _} = Export <- Exported, PropertiesFilter(Export)].

properties_tests(Props, Timeout, Opts) ->
    [property_test(Name, Timeout, Opts) || Name <- Props].

property_test(Name, Timeout, Opts) ->
    {atom_to_list(Name),
        {timeout, Timeout,
            ?_assert(proper:quickcheck(?MODULE:Name(), Opts))}}.

%% Generators

json_object() -> ?LAZY({json_dict()}).

json_dict() ->
    ?LET(L, list({json_string(), json_value()}), L).

json_value() ->
    ?LAZY(
    frequency([
        {2, json_array()},
        {3, null()},
        {4, boolean()},
        {5, number()},
        {6, json_object()},
        {6, json_string()}
    ])).

json_array() ->
    ?LAZY(list(json_value())).

json_string() ->
    json_string(<<>>).

json_string(Fragments) ->
    frequency([
        {1, ?LET(Generated, Fragments, iolist_to_binary(Generated))},
        {50, ?LAZY(json_string([string_fragement() | Fragments]))}
    ]).

string_fragement() ->
    frequency([
        {1, integer(16#0000, 16#001F)}, %% \u+
        {10, list(oneof(
            [
                ?quotation_mark,
                ?reverse_solidus,
                ?solidus,
                ?backspace,
                ?horizontal_tab,
                ?newline,
                ?formfeed,
                ?carriage_return
        ]))},
        {50, utf8()}
    ]).

null() ->
    null.


%% Properties

prop_string_external_size() ->
    ?FORALL(String, json_string(), begin
        byte_size(jiffy:encode(String)) =:= utf8_string_size(String)
    end).

prop_number_external_size() ->
    ?FORALL(EJson, number(), begin
        byte_size(jiffy:encode(EJson)) =:= ejson_size(EJson)
    end).

prop_equal_to_jiffy() ->
    ?FORALL(EJson, json_array(), begin
        byte_size(jiffy:encode(EJson)) =:= ejson_size(EJson)
    end).

-endif.
