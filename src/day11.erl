-module(day11).
-behavior(aoc).

-export([input_type/0, parse_input/1, p1/1, p2/1]).

input_type() -> lines.

parse_input([First | _ ] = Lines) ->
    H = length(Lines),
    W = byte_size(First),
    {H, W, parse_grid(Lines, H, W)}.

p1({_, _, Grid}) ->
    full_count(fix(Grid, fun (Next, Index, Cell) ->
        step(Cell, density(Next, Index))
    end)).

p2({H, W, Grid}) ->
    full_count(fix(Grid, fun (Next, Index, Cell) ->
        step(p2, Cell, sight_density(H, W, Next, Index))
    end)).

%% Internal functions

full_count(Grid) ->
    maps:size(maps:filter(fun (_Index, Cell) -> Cell == full end, Grid)).

fix(Grid, Step) ->
    case maps:map(fun (Index, Cell) -> Step(Grid, Index, Cell) end, Grid) of
        Grid -> Grid;
        Next -> fix(Next, Step)
    end.

step(empty, Density) when Density == 0 -> full;
step(full, Density) when Density >= 4 -> empty;
step(Cell, _Density) -> Cell.

step(p2, empty, Density) when Density == 0 -> full;
step(p2, full, Density) when Density >= 5 -> empty;
step(p2, Cell, _Density) -> Cell.

density(Grid, {Row, Col}) ->
    Deltas = [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}],
    Neighbors = [
        case maps:get({Row + DY, Col + DX}, Grid, floor) of
            full -> 1;
            _    -> 0
        end || {DY, DX} <- Deltas
    ],
    lists:sum(Neighbors).

sight_density(H, W, Grid, Index) ->
    Deltas = [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}],
    Neighbors = [delta_density(H, W, Grid, Index, Direction) || Direction <- Deltas],
    lists:sum(Neighbors).

delta_density(H, W, _Grid, {Row, Col}, _Delta)
    when Row == 0 orelse Col == 0 orelse Row > H orelse Col > W -> 0;
delta_density(H, W, Grid, {Row, Col}, {DY, DX} = Delta) ->
    This = {Row + DY, Col + DX},
    case maps:get(This, Grid, floor) of
        full  -> 1;
        empty -> 0;
        floor -> delta_density(H, W, Grid, This, Delta)
    end.

parse_grid(Lines, Height, Width) ->
    Rows = lists:seq(1, Height),
    Cols = lists:seq(1, Width),
    Pairs = lists:flatten([{Row, Col} || Row <- Rows, Col <- Cols]),
    Cells = lists:flatten([lists:map(fun parse_cell/1, binary_to_list(Col)) || Col <- Lines]),
    Grid = maps:from_list(lists:zip(Pairs, Cells)),
    maps:filter(fun (_Index, Cell) -> Cell =/= floor end, Grid).

parse_cell($.) -> floor;
parse_cell($L) -> empty;
parse_cell($#) -> full.