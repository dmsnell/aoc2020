-module(day5).
-behavior(aoc).

-export([input_type/0, parse_input/1, p1/1, p2/1]).

-type seat() :: {
    Row :: non_neg_integer(),
    Col :: non_neg_integer(),
    Id  :: non_neg_integer()
}.

input_type() -> lines.
parse_input(Lines) -> lists:map(fun seat/1, Lines).

-spec p1(Seats :: list(seat())) -> non_neg_integer().

%% @doc Calculate highest seat ID given binary partitioning addresses
%%
%% Seat assignments are given by a sequence of characters
%% specifying one half of a region to split. For the row
%% we get `F' for "the forward half" and `B' for "the back half."
%% For the column in the row we get `L' for "left" and `R'
%% for the right half.
%%
%% Example:
%% ```
%%            --- column bits
%%     FBFBBFFRLR
%%     -------    row bits
%% '''
%%
%% This function translates a list of these addresses into their
%% row number, column number, and seat ID, which is the value of
%% the full binary address, or Row * 8 + Col.
%%
%% It then needs to return the largest seat ID in the manifest.
%% @end
p1(Seats :: list(seat())) ->
    lists:foldl(fun ({_R, _C, ID}, Max) -> max(Max, ID) end, 0, Seats).

-spec p2(Lines :: list(binary())) -> list(non_neg_integer()).

%% @doc Given a manifest of seat assignments, finds un-filled seats
%% @end
p2(Seats0) ->
    Seats1 = lists:sort(fun ({_, _, ID1}, {_, _, ID2}) -> ID1 =< ID2 end, Seats0),
    missing_seats(Seats1).

%% Internal functions

seat(<<R6, R5, R4, R3, R2, R1, R0, C2, C1, C0>>) ->
    Row = num_val([R0, R1, R2, R3, R4, R5, R6]),
    Col = num_val([C0, C1, C2]),
    {Row, Col, Row * 8 + Col}.

missing_seats([First | Seats]) ->
    missing_seats(Seats, First, []).

missing_seats([], _LastSeat, Missing) -> Missing;
missing_seats([{_, _, ID} = Seat | Seats], {_, _, LastId}, Missing) ->
    case ID - LastId of
        1 -> missing_seats(Seats, Seat, Missing);
        2 -> missing_seats(Seats, Seat, [ID - 1 | Missing])
    end.

num_val(Bits) when is_list(Bits) ->
    num_val(Bits, 1, 0).

num_val([], _Multiplier, Value) -> Value;
num_val([Bit | Bits], Multiplier, Value) ->
    num_val(Bits, Multiplier * 2, Value + binary(Bit) * Multiplier).

binary($F) -> 0;
binary($L) -> 0;
binary($B) -> 1;
binary($R) -> 1.