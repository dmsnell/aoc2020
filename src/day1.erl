-module(day1).
-behavior(aoc).

-export([input_type/0, p1/1, p2/1]).

input_type() -> number_list.

p1(Entries) ->
    [Product | _] = [X * Y || X <- Entries, Y <- Entries, X + Y == 2020],
    Product.

p2(Entries) ->
    [Product | _] = [X * Y * Z || X <- Entries, Y <- Entries, Z <- Entries, X + Y + Z == 2020],
    Product.
