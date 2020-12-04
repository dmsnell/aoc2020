-module(aoc).

-export([
    solve/2,
    solve_all/0
]).

-type input_type() :: raw | lines | number_list.

-callback input_type() -> input_type().
-callback parse_input(input_type()) -> term().
-callback p1(term()) -> term().
-callback p2(term()) -> term().
-optional_callbacks([parse_input/1, p2/1]).

solve_all() ->
    Problems = lists:flatten([get_problems(Day) || Day <- get_days()]),
    [solve(Day, Part) || {Day, Part} <- Problems].

solve(Day, Part) ->
    InputType = Day:input_type(),
    InputName = atom_to_list(Day),
    {USecs, Value} = timer:tc(Day, Part, [parse_input(Day, get_input(InputName, InputType))]),
    {Day, Part, Value, {USecs / 1000, ms}}.

get_days() ->
    [list_to_atom(Module) || {[$d, $a, $y | _] = Module, _, _} <- code:all_available()].

-spec get_input(Name :: string(), lines)       -> list(binary())
      ;        (Name :: string(), number_list) -> list(number()).
get_input(Name, raw) ->
    input:read(Name);
get_input(Name, lines) ->
    input:lines(Name);
get_input(Name, number_list) ->
    input:number_lines(Name).

get_problems(Day) ->
    code:ensure_loaded(Day),
    [{Day, Part} || Part <- [p1, p2], erlang:function_exported(Day, Part, 1) == true].

parse_input(Day, Input) ->
    case erlang:function_exported(Day, parse_input, 1) of
        true  -> Day:parse_input(Input);
        false -> Input
    end.