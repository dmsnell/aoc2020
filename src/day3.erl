-module(day3).
-behavior(aoc).

-export([input_type/0, p1/1, p2/1]).
-record(map, {
    w :: non_neg_integer(),
    h :: non_neg_integer(),
    grid :: map()
}).

input_type() -> lines.

p1(Lines) ->
    Map = build_map(Lines),
    tree_count(Map, path(Map, {3, 1})).

p2(Lines) ->
    Map = build_map(Lines),
    Steps = [
        {1, 1},
        {3, 1},
        {5, 1},
        {7, 1},
        {1, 2}
    ],
    lists:foldl(
        fun (X, Product) -> X * Product end,
        1,
        [tree_count(Map, path(Map, Step)) || Step <- Steps]
    ).

%% Internal functions

tree_count(Map, Path) ->
    length(lists:filtermap(fun (XY) -> at(Map, XY) == tree end, Path)).

at(#map{w = Width, grid = Grid}, {X, Y}) ->
    case maps:get(Y, Grid, missing) of
        missing -> empty;
        Row -> maps:get(X rem Width, Row, empty)
    end.

build_map([]) -> #map{w = 0, h = 0, grid = #{}};
build_map([FirstLine | _Rest] = Lines) ->
    #map{
        w = byte_size(FirstLine),
        h = length(Lines),
        grid = maps:from_list(
            lists:zip(
                lists:seq(0, length(Lines) - 1),
                [tree_spots(Line) || Line <- Lines]
            )
        )
    }.

path(#map{} = Map, Step) ->
    path(Map, Step, [{0, 0}], {0, 0}).

path(#map{h = Height}, _Step, [_OverStep | Steps], {_X, Y}) when Y >= Height -> lists:reverse(Steps);
path(#map{} = Map, {XS, YS} = Step, Steps, {X, Y}) ->
    path(Map, Step, [{X + XS, Y + YS} | Steps], {X + XS, Y + YS}).

tree_spots(<<Line/binary>>) ->
    maps:from_list([{X, tree} || {X, _Span} <- binary:matches(Line, [<<"#">>])]).