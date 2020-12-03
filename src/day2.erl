-module(day2).
-behavior(aoc).

-export([input_type/0, parse_input/1, p1/1, p2/1]).

-record(rule, {
    char :: char(),
    min  :: non_neg_integer(),
    max  :: non_neg_integer()
}).

input_type() -> lines.
parse_input(Lines) -> lists:map(fun parse_line/1, Lines).

-spec p1(list({#rule{}, Password :: binary()})) -> non_neg_integer().

%% @doc Counts valid passwords in corrupted list
%% @end
p1(Pairs) ->
    length([Password || {Rule, Password} <- Pairs, check_password(Password, Rule) == valid]).

p2(Pairs) ->
    length([Password || {Rule, Password} <- Pairs, check_password_positions(Password, Rule) == valid]).

%% Internal functions

-spec check_password(Password :: binary(), #rule{}) -> valid | invalid.

check_password(<<Password/binary>>, #rule{char = Char, min = Min, max = Max}) ->
    case char_count(Password, Char) of
        Count when Count >= Min, Count =< Max -> valid;
        _ -> invalid
    end.

-spec char_count(Input :: binary(), Char :: char()) -> non_neg_integer().

char_count(<<Input/binary>>, Char) ->
    char_count(Input, Char, 0).

-spec char_count(Input :: binary(), Char :: char(), RunningCount :: non_neg_integer())
    -> non_neg_integer().

char_count(<<"">>, _Char, Count) -> Count;
char_count(<<Char, Input/binary>>, Char, Count) -> char_count(Input, Char, Count + 1);
char_count(<<_, Input/binary>>, Char, Count) -> char_count(Input, Char, Count).

-spec check_password_positions(Password :: binary(), #rule{}) -> valid | invalid.

check_password_positions(<<Password/binary>>, #rule{char = Char, min = P1, max = P2}) ->
    case {binary:at(Password, P1 - 1), binary:at(Password, P2 - 1)} of
        {Char, Char} -> invalid;
        {Char, _} -> valid;
        {_, Char} -> valid;
        _ -> invalid
    end.

-spec parse_line(Line :: binary()) -> {#rule{}, Password :: binary()}.

%% @doc Parses line in password list
%%
%% Lines contain a password rule and a password
%% Example:
%% ```
%%     2-5 g: jmvmgnghr
%%     | | |  |- password
%%     | | |---- required character
%%     | |------ max appearances of character
%%     |-------- min appearances of character
%% '''
%% @end
parse_line(<<Line/binary>>) ->
    [Rule, <<" ", Password/binary>>] = binary:split(Line, <<":">>),
    {parse_rule(Rule), Password}.

-spec parse_rule(RuleText :: binary()) -> #rule{}.

%% @doc Parses rule definition
%%
%% Example:
%% ```
%%     2-5 g
%%     | | |- required character
%%     | |------ max appearances of character
%%     |-------- min appearances of character
%% '''
%% @end
parse_rule(<<Rule/binary>>) ->
    [Limits, <<Char>>] = binary:split(Rule, <<" ">>),
    [Min, Max] = [binary_to_integer(S) || S <- binary:split(Limits, <<"-">>)],
    #rule{char = Char, min = Min, max = Max}.