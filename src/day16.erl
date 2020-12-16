-module(day16).
-behavior(aoc).

-export([input_type/0, parse_input/1, p1/1, p2/1]).

-record(s, {
    rules = #{},
    yours = [] :: list(non_neg_integer()),
    nearby = [] :: list(list(non_neg_integer()))
}).

input_type() -> lines.

parse_input(Lines) -> parse_notes(Lines).

p1(#s{rules = R, nearby = N}) ->
    lists:sum([V || Ticket <- N, V <- Ticket, {no_match, V} == guess_rule(R, V)]).

p2(#s{rules = R, yours = Y, nearby = N}) ->
    ValidTickets = [T || T <- N, lists:all(fun (V) -> ok == element(1, guess_rule(R, V)) end, T)],
    Columns = transpose(ValidTickets),
    AllRules = sets:from_list(maps:keys(R)),
    PlausibleRules = maps:from_list(lists:zip(lists:seq(1, length(Columns)), [(overlapping_rules(R, AllRules))(Col) || Col <- Columns])),
    Fields = assign(PlausibleRules),
    Departures = maps:filter(fun (_Column, <<"departure ", _Field/binary>>) -> true; (_Column, _Field) -> false end, Fields),
    lists:foldl(fun (A, B) -> A * B end, 1, [lists:nth(C, Y) || C <- maps:keys(Departures)]).

%% Internal functions

assign(PlausibleRules) ->
    assign(PlausibleRules, #{}).

assign(PlausibleRules, Columns) ->
    assign(PlausibleRules, Columns, maps:filter(fun (_Column, Rules) -> sets:size(Rules) == 1 end, PlausibleRules)).

assign(PlausibleRules, Columns, Singles) ->
    case maps:size(Singles) of
        0 -> Columns;
        _ ->
            Assignments = maps:map(fun (_Column, Rules) -> hd(sets:to_list(Rules)) end, Singles),
            Assigned = sets:from_list(maps:values(Assignments)),
            Remaining = maps:without(maps:keys(Assignments), PlausibleRules),
            assign(maps:map(fun (_Col, Rules) -> sets:subtract(Rules, Assigned) end, Remaining), maps:merge(Columns, Assignments))
    end.

overlapping_rules(R, AllRules) ->
    fun (Column) ->
        lists:foldl(
            fun (V, Matches) ->
                NextMatches = matches(R, V),
                sets:intersection(Matches, NextMatches)
            end,
            AllRules,
            Column
        )
    end.

apply_range({Min, Max}, V) when V >= Min, V =< Max -> ok;
apply_range(_Rule, _V) -> no_match.

apply_rule([], _V) -> no_match;
apply_rule([Range | Ranges], V) ->
    case apply_range(Range, V) of
        ok -> ok;
        no_match -> apply_rule(Ranges, V)
    end.

guess_rule(Rules, Value) when is_map(Rules) ->
    guess_rule(maps:next(maps:iterator(Rules)), Value);

guess_rule(none, Value) ->
    {no_match, Value};
guess_rule({Name, Ranges, Next}, Value) ->
    case apply_rule(Ranges, Value) of
        ok -> {ok, Name};
        no_match -> guess_rule(maps:next(Next), Value)
    end.

matches(Rules, Value) ->
    sets:from_list(maps:keys(maps:filter(fun (_Name, Ranges) -> ok == apply_rule(Ranges, Value) end, Rules))).

transpose(Rows) when is_list(Rows) ->
    transpose(Rows, [[] || _ <- lists:seq(1, length(lists:nth(1, Rows)))]).

transpose([], Output) ->
    lists:map(fun lists:reverse/1, Output);
transpose([Row | Rows], Output) ->
    transpose(Rows, lists:zipwith(fun (L, R) -> [L | R] end, Row, Output)).

%% Parsing

parse_notes(Lines) when is_list(Lines) ->
    parse_notes(rules, Lines, #s{}).

parse_notes(rules, [<<>>, <<"your ticket:">> | Lines], S) ->
    parse_notes(yours, Lines, S);

parse_notes(rules, [Line | Lines], #s{rules = R} = S) ->
    {Name, Ranges} = parse_rule(Line),
    parse_notes(rules, Lines, S#s{rules = maps:put(Name, Ranges, R)});

parse_notes(yours, [<<>>, <<"nearby tickets:">> | Lines], S) ->
    parse_notes(nearby, Lines, S);

parse_notes(yours, [Line | Lines], S) ->
    parse_notes(yours, Lines, S#s{yours = parse_ticket(Line)});

parse_notes(nearby, [], #s{nearby = N} = S) ->
    S#s{nearby = lists:reverse(N)};

parse_notes(nearby, [Line | Lines], #s{nearby = N} = S) ->
    parse_notes(nearby, Lines, S#s{nearby = [parse_ticket(Line) | N]}).

parse_rule(<<Line/binary>>) ->
    [Name, RangeText] = binary:split(Line, [<<": ">>]),
    Ranges = [parse_range(Segment) || Segment <- binary:split(RangeText, [<<" or ">>], [global])],
    {Name, Ranges}.

parse_range(<<Range/binary>>) ->
    [Min, Max] = binary:split(Range, [<<"-">>]),
    {binary_to_integer(Min), binary_to_integer(Max)}.

parse_ticket(<<Line/binary>>) ->
    [binary_to_integer(S) || S <- binary:split(Line, [<<",">>], [global])].