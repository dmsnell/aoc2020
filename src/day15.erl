-module(day15).
-behavior(aoc).

-export([input_type/0, parse_input/1, p1/1, p2/1]).

input_type() -> raw.

parse_input(<<Text/binary>>) ->
    Pieces = binary:split(Text, [<<",">>], [global]),
    [element(1, day7:parse_count(S)) || S <- Pieces].

p1(Nums) ->
    {_State, Last, _Turn} = run_to(2020, init(Nums)),
    Last.

p2(Nums) ->
    {_State, Last, _Turn} = run_to(30000000, init(Nums)),
    Last.

%% Internal functions

run_to(N, {_, _, Turn} = Game) when Turn == N + 1 -> Game;
run_to(N, Game) -> run_to(N, next(Game)).

next({State, Last, Turn}) ->
    case maps:get(Last, State, missing) of
        missing -> {maps:put(0, Turn, State), 0, Turn + 1};
        {T1, T2} -> {maps:update_with(T2 - T1, fun ({_, T}) -> {T, Turn} end, {Turn, Turn}, State), T2 - T1, Turn + 1}
    end.

init(Numbers) ->
    init(Numbers, #{}, 0, 1).

init([], State, Last, Turn) ->
    {State, Last, Turn};
init([N | Numbers], State, _Last, Turn) ->
    init(Numbers, maps:put(N, {Turn, Turn}, State), N, Turn + 1).