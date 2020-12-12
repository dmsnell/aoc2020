-module(aoc).

-export([
    solve/2,
    solve_all/0
]).

-type input_type() :: raw | groups_and_lines | lines | number_list.

-callback input_type() -> input_type().
-callback parse_input(term()) -> term().
-callback p1(term()) -> term().
-callback p2(term()) -> term().
-optional_callbacks([parse_input/1, p2/1]).

solve_all() ->
    Problems = lists:flatten([get_problems(Day) || Day <- get_days()]),
    lists:sort(fun problem_sort/2, [solve(Day, Part) || {Day, Part} <- Problems]).

problem_sort(A, B) ->
    <<"day", DayA/binary>> = atom_to_binary(element(1, A)),
    <<"day", DayB/binary>> = atom_to_binary(element(1, B)),
    <<"p", PartA/binary>> = atom_to_binary(element(2, A)),
    <<"p", PartB/binary>> = atom_to_binary(element(2, B)),
    SortA = {binary_to_integer(DayA), binary_to_integer(PartA)},
    SortB = {binary_to_integer(DayB), binary_to_integer(PartB)},
    SortA < SortB.

solve(Day, Part) ->
    utils:isolated(fun () ->
        InputType = Day:input_type(),
        InputName = atom_to_list(Day),
        {USecs, Value} = timer:tc(Day, Part, [parse_input(Day, get_input(InputName, InputType))]),
        {Day, Part, Value, {USecs / 1000, ms}}
    end, infinity).

get_days() ->
    [list_to_atom(Module) || {[$d, $a, $y | _] = Module, _, _} <- code:all_available()].

-spec get_input(Name :: string(), lines)            -> list(binary())
      ;        (Name :: string(), groups_and_lines) -> list(list(binary()))
      ;        (Name :: string(), number_list)      -> list(number())
      ;        (Name :: string(), raw)              -> binary().
get_input(Name, Type) ->
    input:Type(Name).

get_problems(Day) ->
    code:ensure_loaded(Day),
    [{Day, Part} || Part <- [p1, p2], erlang:function_exported(Day, Part, 1) == true].

parse_input(Day, Input) ->
    case erlang:function_exported(Day, parse_input, 1) of
        true  -> Day:parse_input(Input);
        false -> Input
    end.