-module(day9).
-behavior(aoc).

-include_lib("eunit/include/eunit.hrl").

-export([input_type/0, p1/1, p2/1]).

input_type() -> number_list.

p1(Stream) ->
    find_invalid(lists:split(25, Stream)).

p2(Stream) ->
    hash(find_contiguous_sum(p1(Stream), Stream)).

%% Internal functions

eat({[_P | Reamble] = Preamble, [Next | Rest] = Stream}) ->
    case [valid || X <- Preamble, Y <- Preamble, X =/= Y, X + Y == Next] of
        [] -> {invalid, {Preamble, Stream}};
        _  -> {valid, {Reamble ++ [Next], Rest}}
    end.

find_invalid({P, [Next | _Rest]} = Stream) when is_list(P) ->
    case eat(Stream) of
        {valid,  S2} -> find_invalid(S2);
        {invalid, _} -> Next
    end.

find_contiguous_sum(Sum, [First | Rest]) ->
    find_contiguous_sum(Sum, [First], Rest, First).

find_contiguous_sum(Sum, [_, _ | _] = Guess, _Stream, Total) when Total == Sum ->
    Guess;
find_contiguous_sum(Sum, [First | Guess], Stream, Total) when Total > Sum ->
    find_contiguous_sum(Sum, Guess, Stream, Total - First);
find_contiguous_sum(Sum, Guess, [Next | Stream], Total) ->
    find_contiguous_sum(Sum, Guess ++ [Next], Stream, Total + Next).

hash(List) when is_list(List) ->
    lists:max(List) + lists:min(List).

-ifdef(TEST).

example1_test() ->
    Preamble = utils:shuffle(lists:seq(1, 25)),
    ?assertMatch({valid, _}, eat({Preamble, [26]})),
    ?assertMatch({valid, _}, eat({Preamble, [49]})),
    ?assertMatch({invalid, _}, eat({Preamble, [100]})),
    ?assertMatch({invalid, _}, eat({Preamble, [50]})).

example2_test() ->
    Preamble = utils:shuffle(lists:seq(1,25) -- [20]) ++ [45],
    ?assertMatch({valid, _}, eat({Preamble, [26]})),
    ?assertMatch({invalid, _}, eat({Preamble, [65]})),
    {Valid, S2} = eat({Preamble, [64, 66]}),
    ?assertEqual(valid, Valid),
    ?assertMatch({valid, _}, eat(S2)).

example3_test() ->
    S = lists:split(5, [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]),
    ?assertEqual(127, find_invalid(S)).

example4_test() ->
    S = [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576],
    ?assertEqual(62, hash(find_contiguous_sum(127, S))).

-endif.