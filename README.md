Advent of Code 2020 Solutions
=====

Running
-----

To run a particular solution call `aoc:solve/2` with the day
and the part specified. The day is an atom like `day1` and the
part is either `p1` for part one or `p2` for part two.

The output from each solution identifies which day and part ran,
the output for that problem, and the time it took to run.

To run all the solutions call `aoc:solve_all()`.

    $ rebar3 shell
    1> aoc:solve_all()
    [{day1,p1,970816,{0.522,ms}},
     {day1,p2,96047280,{4.055,ms}},
     {day2,p1,458,{1.423,ms}},
     {day2,p2,342,{1.44,ms}},
     {day3,p1,156,{0.528,ms}},
     {day3,p2,3521829480,{0.697,ms}}]
    2> aoc:solve(day2, p2).
    {day2,p2,342,{1.91,ms}}
    3> {Day, Part, Answer, Runtime} = aoc:solve(day3, p2).
    {day3,p2,3521829480,{1.178,ms}}