-module(day6).
-behavior(aoc).

-export([input_type/0, p1/1, p2/1]).

input_type() -> groups_and_lines.

p1(Groups) ->
    lists:sum(lists:map(fun any_yesses/1, Groups)).

p2(Groups) ->
    lists:sum(lists:map(fun unamimous_yesses/1, Groups)).

%% Internal functions

inc(A) -> A + 1.

any_yesses(Group) when is_list(Group) ->
    maps:size(group_yes_counts(Group)).

unamimous_yesses(Group) when is_list(Group) ->
    Questions = group_yes_counts(Group),
    AllYes = maps:filter(fun (_K, V) -> V == length(Group) end, Questions),
    maps:size(AllYes).

group_yes_counts(Answers) when is_list(Answers) ->
    group_yes_counts(Answers, #{}).

group_yes_counts(Answers, Counts0) when is_list(Answers), is_map(Counts0) ->
    lists:foldl(fun (Seat, Counts) -> seat_yes_counts(Seat, Counts) end, Counts0, Answers).

seat_yes_counts(Answer, Counts) ->
    lists:foldl(fun (Q, Qs) -> maps:update_with(Q, fun inc/1, 1, Qs) end, Counts, Answer).
