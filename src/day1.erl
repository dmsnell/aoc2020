-module(day1).

-export([
    input_type/0,
    p1/1,
    p2/1
]).

input_type() -> number_list.

-spec p1(Entries :: list(integer())) -> Product :: integer().

p1(Entries) ->
    [Product | _] = [X * Y || X <- Entries, Y <- Entries, X + Y == 2020],
    Product.

-spec p2(Entries :: list(integer())) -> Product :: integer().

p2(Entries) ->
    [Product | _] = [X * Y * Z || X <- Entries, Y <- Entries, Z <- Entries, X + Y + Z == 2020],
    Product.
