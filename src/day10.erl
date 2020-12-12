-module(day10).
-behavior(aoc).

-export([input_type/0, parse_input/1, p1/1, p2/1]).

input_type() -> number_list.

% data has no duplicates
parse_input(Ratings) -> [0 | lists:sort(Ratings)].

p1(Ratings) ->
    Max = lists:last(Ratings),
    Device = Max + 3,
    Diffs = lists:zipwith(fun sub/2, tl(Ratings) ++ [Device], Ratings),
    Ones = utils:count(fun (A) -> A end, 1, Diffs),
    Threes = utils:count(fun (A) -> A end, 3, Diffs),
    Ones * Threes.

p2(Ratings) ->
    [Max | _] = Reversed = lists:reverse(Ratings),
    Cache0 = maps:from_list(lists:zip(Ratings, lists:duplicate(length(Ratings), 0))),
    Cache1 = lists:foldl(fun (R, Cache) ->
        maps:put(R, [X || X <- Ratings, X - R > 0, X - R =< 3], Cache)
    end, Cache0, [Max + 3 | Reversed]),
    Tree = maps:remove(Max + 3, Cache1),
    Paths = lists:foldl(fun (R, Cache) ->
        Count = case maps:get(R, Cache) of
            C when is_integer(C) -> C;
            [] -> 1;
            Rs when is_list(Rs) -> lists:sum([maps:get(C, Cache) || C <- Rs])
        end,
        maps:put(R, Count, Cache)
    end, Tree, Reversed),
    maps:get(0, Paths).

%% Internal functions

sub(A, B) -> A - B.