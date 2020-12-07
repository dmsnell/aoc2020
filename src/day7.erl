-module(day7).
-behavior(aoc).

-export([input_type/0, parse_input/1, p1/1, p2/1]).

input_type() -> lines.

parse_input(Lines) ->
    lists:map(fun parse_rule/1, Lines).

p1(Rules) ->
    utils:count(fun ({Color, _Contains}) -> can_contain(Rules, Color, <<"shiny gold">>) end, true, Rules).

p2(Rules) ->
    contains(Rules, <<"shiny gold">>, 0).

%% Internal functions

contains(Rules, <<Color/binary>>, Count) ->
    contains(Rules, lists:keyfind(Color, 1, Rules), Count);
contains(_Rules, {_Color, []}, Count) ->
    Count;
contains(Rules, {_OuterColor, Contents}, Count) ->
    Count + lists:sum(lists:map(fun ({Color, InnerCount}) -> InnerCount * (1 + contains(Rules, Color, 0)) end, Contents)).

can_contain(Rules, <<Outer/binary>>, Inner) ->
    can_contain(Rules, lists:keyfind(Outer, 1, Rules), Inner);
can_contain(_Rules, {_Outer, []}, _Inner) ->
    false; % empty bag contains nothing
can_contain(Rules, {_Outer, Contents} = Rule, Inner) ->
    case can_directly_contain(Rule, Inner) of
        true  -> true; % directly allowed in bag
        false -> lists:any(fun ({Color, _Count}) -> can_contain(Rules, Color, Inner) end, Contents)
    end.

can_directly_contain({_Outer, Contents}, Inner) ->
    lists:keymember(Inner, 1, Contents).

%% Rule parsing functions

parse_color(<<Input/binary>>) ->
    [Modifier, Rest1] = binary:split(Input, [<<" ">>]),
    [Color, Rest2] = binary:split(Rest1, [<<" ">>]),
    {<<Modifier/binary, " ", Color/binary>>, <<" ", Rest2/binary>>}.

parse_rule(<<Rule/binary>>) ->
    {BagColor, Rest1} = parse_color(Rule),
    <<" bags contain ", Rest2/binary>> = Rest1,
    {Contents, <<".">>} = parse_or([
        fun parse_empty_bag/1,
        fun parse_bag_list/1
    ], Rest2),
    {BagColor, Contents}.

parse_empty_bag(<<"no other bags", Rest/binary>>) ->
    {[], Rest}.

parse_bag_count(<<Input/binary>>) ->
    {Count, Rest1} = parse_count(Input),
    <<" ", Rest2/binary>> = Rest1,
    {Color, Rest3} = parse_color(Rest2),
    case Rest3 of
        <<" bags", Rest4/binary>> -> {{Color, Count}, Rest4};
        <<" bag", Rest4/binary>>  -> {{Color, Count}, Rest4}
    end.

parse_count(<<Input/binary>>) ->
    parse_count(Input, []).

parse_count(<<A, Rest/binary>>, Digits) when A >= $0, A =< $9 ->
    parse_count(Rest, [A | Digits]);
parse_count(<<Rest/binary>>, Digits) ->
    {list_to_integer(lists:reverse(Digits)), Rest}.

parse_bag_list(<<Input/binary>>) ->
    parse_list(fun parse_bag_count/1, Input).

parse_list(Parser, <<Input/binary>>) ->
    parse_list(Parser, [], <<", ", Input/binary>>).

parse_list(Parser, List, <<", ", Input/binary>>) ->
    {Value, Rest} = Parser(Input),
    parse_list(Parser, [Value | List], Rest);
parse_list(_Parser, List, <<Input/binary>>) ->
    {List, Input}.

parse_or([Parser | Parsers], <<Input/binary>>) ->
    try Parser(Input) of
        {Value, Rest} -> {Value, Rest}
    catch
        _:_ -> parse_or(Parsers, Input)
    end.
