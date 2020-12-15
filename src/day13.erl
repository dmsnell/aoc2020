-module(day13).
-behavior(aoc).

-include_lib("eunit/include/eunit.hrl").

-export([input_type/0, parse_input/1, p1/1, p2/1]).

input_type() -> lines.

parse_input(Lines) -> parse_lines(Lines).

p1({Earliest, BusIds}) ->
    Times = [{Id, earliest(Earliest, Id)} || Id <- BusIds, Id =/= x],
    {BusId, Wait} = hd(lists:keysort(2, Times)),
    BusId * Wait.

-ifdef(EUNIT).

example1_test() ->
    Input = {939, [7, 13, x, x, 59, x, 31, 19]},
    ?assertEqual(295, p1(Input)).

-endif.

p2({_Earliest, BusIds}) ->
    Routes = [X || {_, A} = X <- lists:zip(lists:seq(0, length(BusIds) - 1), BusIds), A =/= x],
    Cycles = lists:map(fun cycle/1, Routes),
    {Wait, _Period} = join_em(Cycles, []),
    Wait.

join_em([], [Join]) ->
    Join;
join_em([], Joins) ->
    join_em(Joins, []);
join_em([A, B | Cycles], Joins) ->
    join_em(Cycles, [join(A, B) | Joins]);
join_em([A | Cycles], [J | Joins]) ->
    join_em(Cycles, [join(A, J) | Joins]).

-ifdef(EUNIT).

example2_test() ->
    ?assertEqual(3417, p2(parse_input([<<"0">>, <<"17,x,13,19">>]))),
    ?assertEqual(754018, p2(parse_input([<<"0">>, <<"67,7,59,61">>]))),
    ?assertEqual(779210, p2(parse_input([<<"0">>, <<"67,x,7,59,61">>]))),
    ?assertEqual(1261476, p2(parse_input([<<"0">>, <<"67,7,x,59,61">>]))),
    ?assertEqual(1202161486, p2(parse_input([<<"0">>, <<"1789,37,47,1889">>]))).

-endif.

%% Internal functions

cycle({Wait, BusId}) ->
    [Mod] = [I || I <- lists:seq(0, BusId - 1), earliest(I, BusId) == Wait rem BusId],
    {Mod, BusId}.

join(Left, Right) ->
    join(Left, Right, sync).

join({I, _StepA}, {I, _StepB}, period) ->
    I;
join({I, StepA}, {I, StepB}, sync) ->
    {I, lcm(StepA, StepB)};
join({A, StepA}, {B, StepB} = Right, Phase) when A + StepA < B + StepB ->
    join({A + StepA, StepA}, Right, Phase);
join({_A, _StepA} = Left, {B, StepB}, Phase) ->
    join(Left, {B + StepB, StepB}, Phase).

gcd(0, 0) -> 0;
gcd(A, A) -> A;
gcd(A, B) when A > B -> gcd(A - B, B);
gcd(A, B) -> gcd(A, B - A).

lcm(A, B) -> (A * B) div gcd(A, B).

earliest(Time, BusId) when is_integer(Time), is_integer(BusId) ->
    case Time rem BusId of
        0 ->
            0;
        _ -> 
            BusTime = BusId * ((Time div BusId) + 1),
            BusTime - Time
    end.

parse_lines([<<TimeLine/binary>>, <<BusLine/binary>>]) ->
    Earliest = binary_to_integer(TimeLine),
    BusIds = binary:split(BusLine, [<<",">>], [global]),
    {Earliest, [parse_id(Id) || Id <- BusIds]}.

parse_id(<<"x">>) -> x;
parse_id(<<Id/binary>>) -> binary_to_integer(Id).