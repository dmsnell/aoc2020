Advent of Code 2020 Solutions
=====

Running the solutions
-----

To run a particular solution call `aoc:solve/2` with the day
and the part specified. The day is an atom like `day1` and the
part is either `p1` for part one or `p2` for part two.

The output from each solution identifies which day and part ran,
the output for that problem, and the time it took to run.

To run all the solutions call `aoc:solve_all()`.

Start by cloning the repo and running `rebar3 shell` for a REPL.

```erlang
aoc:solve_all() = [
    {day1,p1,970816,{0.438,ms}},
    {day1,p2,96047280,{3.614,ms}},
    {day2,p1,458,{0.33,ms}},
    {day2,p2,342,{0.144,ms}},
    {day3,p1,156,{0.121,ms}},
    {day3,p2,3521829480,{0.285,ms}},
    {day4,p1,247,{0.048,ms}},
    {day4,p2,145,{0.505,ms}},
    {day5,p1,935,{0.033,ms}},
    {day5,p2,[743],{0.525,ms}},
    {day6,p1,6612,{1.85,ms}},
    {day6,p2,3268,{2.676,ms}},
    {day7,p1,112,{44.543,ms}},
    {day7,p2,6260,{0.016,ms}},
    {day8,p1,1915,{0.045,ms}},
    {day8,p2,{944,{swap,#{now => {nop,-46},pc => 447,was => {jmp,-46}}}},{15.611,ms}},
    {day9,p1,70639851,{5.152,ms}},
    {day9,p2,8249240,{5.135,ms}}
].

aoc:solve(day2, p2) = {day2,p2,342,{1.91,ms}}.

{Day, Part, Answer, Runtime} = aoc:solve(day3, p2),
Day = day3,
Part = p2,
Answer = 3521829480,
Runtime = {1.178,ms}.
```

To develop with an open REPL call `r3:compile()` from within
the REPL to reload code with updates from your editor.

```erlang
% Reload code and re-run all solutions
r3:compile(), aoc:solve_all().
```