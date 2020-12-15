-module(day7).
-behavior(aoc).

-export([input_type/0, parse_input/1, p1/1, p2/1]).
-export([parse_count/1, parse_or/2]).

input_type() -> lines.

parse_input(Lines) ->
    maps:from_list(lists:map(fun parse_rule/1, Lines)).

p1(Rules) ->
    utils:count(fun (Contents) -> can_contain(Rules, Contents, <<"shiny gold">>) end, true, maps:values(Rules)).

p2(Rules) ->
    contains(Rules, <<"shiny gold">>, 0).

%% Internal functions

contains(Rules, <<Color/binary>>, Count) ->
    contains(Rules, maps:get(Color, Rules), Count);
contains(Rules, Contents, Count) when is_map(Contents) ->
    Count + lists:sum(lists:map(fun ({Color, InnerCount}) -> InnerCount * (1 + contains(Rules, Color, 0)) end, maps:to_list(Contents))).

can_contain(Rules, <<Outer/binary>>, Inner) ->
    can_contain(Rules, maps:get(Outer, Rules), Inner);
can_contain(Rules, Contents, Inner) when is_map(Contents) ->
    case maps:is_key(Inner, Contents) of
        true  -> true; % directly allowed in bag
        false -> lists:any(fun (Color) -> can_contain(Rules, Color, Inner) end, maps:keys(Contents))
    end.

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
    {BagColor, maps:from_list(Contents)}.

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

parse_count(<<"-", Rest/binary>>, []) ->
    {Value, Unparsed} = parse_count(Rest, []),
    {-Value, Unparsed};
parse_count(<<"+", Rest/binary>>, []) ->
    parse_count(Rest, []);
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
