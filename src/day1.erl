-module(day1).

-export([
    part_one/0,
    part_two/0
]).

part_one() ->
    Entries = input:number_lines("day1"),
    [Product | _] = [X * Y || X <- Entries, Y <- Entries, X + Y == 2020],
    Product.

part_two() ->
    Entries = input:number_lines("day1"),
    [Product | _] = [X * Y * Z || X <- Entries, Y <- Entries, Z <- Entries, X + Y + Z == 2020],
    Product.
