-module(couch_ejson_size).

-export([ejson_size/1]).


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
    % this doesn't match what jiffy does. so will be off
    byte_size(float_to_binary(Float, [{decimals, 16}, compact]));

ejson_size(0) ->
    1; % log(0) is not defined

ejson_size(Integer) when is_integer(Integer), Integer > 0 ->
    trunc(math:log10(Integer)) + 1;

ejson_size(Integer) when is_integer(Integer), Integer < 0 ->
    % 2 is because 1 is for the - character
    trunc(math:log10(Integer)) + 2;

ejson_size(Binary) when is_binary(Binary) ->
    % count escapes and assume they'd be escaped with one extra escape character
    Escapes = [<<"\b">>, <<"\f">>, <<"\n">>, <<"\r">>, <<"\t">>, <<"\\">>,
        <<"\"">>],
    MatchCount = length(binary:matches(Binary, Escapes)),
    % 2 is for open and closing "
    2 + byte_size(Binary) + MatchCount;

ejson_size(null) ->
    4;

ejson_size(true) ->
    4;

ejson_size(false) ->
    5;

ejson_size(Atom) when is_atom(Atom) ->
    ejson_size(atom_to_binary(Atom, utf8)).
