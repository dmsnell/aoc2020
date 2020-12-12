-module(day12).
-behavior(aoc).

-include_lib("eunit/include/eunit.hrl").

-export([input_type/0, parse_input/1, p1/1, p2/1]).

input_type() -> lines.

parse_input(Lines) -> lists:map(fun parse/1, Lines).

p1(Actions) ->
    {_, X, Y} = lists:foldl(fun act/2, {0, 0, 0}, Actions),
    abs(X) + abs(Y).

-ifdef(EUNIT).

example1_test() ->
    ?assertEqual(25, p1([{f, 10}, {n, 3}, {f, 7}, {r, 90}, {f, 11}])).

-endif.

p2(Actions) ->
    {{X, Y}, _WP} = lists:foldl(fun way/2, {{0, 0}, {10, 1}}, Actions),
    abs(X) + abs(Y).

-ifdef(EUNIT).

example2_test() ->
    ?assertEqual(286, p2([{f, 10}, {n, 3}, {f, 7}, {r, 90}, {f, 11}])).

-endif.

%% Internal functions

act({n, D}, {H, X, Y}) -> {H, X, Y + D};
act({s, D}, {H, X, Y}) -> {H, X, Y - D};
act({e, D}, {H, X, Y}) -> {H, X + D, Y};
act({w, D}, {H, X, Y}) -> {H, X - D, Y};
act({l, D}, {H, X, Y}) -> {rotate(H, D), X, Y};
act({r, D}, {H, X, Y}) -> {rotate(H, -D), X, Y};
act({f, D}, {H, X, Y}) -> {H, X + D * x(H), Y + D * y(H)}.

x(0) -> 1; x(90) -> 0; x(180) -> -1; x(270) -> 0.
y(0) -> 0; y(90) -> 1; y(180) -> 0; y(270) -> -1.

way({n, D}, {S, {WX, WY}}) -> {S, {WX, WY + D}};
way({s, D}, {S, {WX, WY}}) -> {S, {WX, WY - D}};
way({e, D}, {S, {WX, WY}}) -> {S, {WX + D, WY}};
way({w, D}, {S, {WX, WY}}) -> {S, {WX - D, WY}};
way({l, D}, {S, WP}) -> {S, rotate(WP, D)};
way({r, D}, {S, WP}) -> {S, rotate(WP, -D)};
way({f, D}, {{X, Y}, {DX, DY} = W}) -> {{X + D * DX, Y + D * DY}, W}.

rotate(H, D) when is_integer(H), is_integer(D) ->
    (360 + H + D) rem 360;

rotate({X, Y}, 0) -> {X, Y};
rotate({X, Y}, 90) -> {-Y, X};
rotate({X, Y}, 180) -> {-X, -Y};
rotate({X, Y}, 270) -> {Y, -X};
rotate({X, Y}, D) -> rotate({X, Y}, (360 + D) rem 360).

parse(<<"N", D/binary>>) -> {n, binary_to_integer(D)};
parse(<<"S", D/binary>>) -> {s, binary_to_integer(D)};
parse(<<"E", D/binary>>) -> {e, binary_to_integer(D)};
parse(<<"W", D/binary>>) -> {w, binary_to_integer(D)};
parse(<<"L", D/binary>>) -> {l, binary_to_integer(D)};
parse(<<"R", D/binary>>) -> {r, binary_to_integer(D)};
parse(<<"F", D/binary>>) -> {f, binary_to_integer(D)}.