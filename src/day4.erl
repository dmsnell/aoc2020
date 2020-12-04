-module(day4).
-behavior(aoc).

-export([input_type/0, parse_input/1, p1/1, p2/1]).

input_type() -> raw.

parse_input(<<Input/binary>>) ->
    % Separate passport records
    Records0 = binary:split(Input, [<<"\n\n">>], [global]),

    % Split each record into key:value spans
    Records1 = [binary:split(Record, [<<" ">>, <<"\n">>], [global]) || Record <- Records0],

    % Split each key:value span into #{key => value} maps
    [
        begin
            Pairs = [binary:split(Span, [<<":">>], [global]) || Span <- KeyValueSpans],
            maps:from_list(lists:map(fun list_to_tuple/1, Pairs))
        end ||
        KeyValueSpans <- Records1
    ].

p1(Records) ->
    utils:count(fun valid_passport/1, valid, Records).

p2(Records) ->
    utils:count(fun validate_passport/1, valid, Records).

%% Internal functions

valid_passport(#{
    <<"byr">> := _,
    <<"iyr">> := _,
    <<"eyr">> := _,
    <<"hgt">> := _,
    <<"hcl">> := _,
    <<"ecl">> := _,
    <<"pid">> := _
}) -> valid;
valid_passport(_) -> invalid.

validate_passport(Record) ->
    try
        Fields = [<<"byr">>, <<"iyr">>, <<"eyr">>, <<"hgt">>, <<"hcl">>, <<"ecl">>, <<"pid">>],
        true = lists:all(fun (Field) -> valid(Field, maps:get(Field, Record)) == true end, Fields),
        valid
    catch
        _:_ -> invalid
    end.

valid(<<"byr">>, <<S/binary>>) -> valid(<<"byr">>, binary_to_integer(S));
valid(<<"byr">>, BYR) -> BYR >= 1920 andalso BYR =< 2020;

valid(<<"iyr">>, <<S/binary>>) -> valid(<<"iyr">>, binary_to_integer(S));
valid(<<"iyr">>, IYR) -> IYR >= 2010 andalso IYR =< 2020;

valid(<<"eyr">>, <<S/binary>>) -> valid(<<"eyr">>, binary_to_integer(S));
valid(<<"eyr">>, EYR) -> EYR >= 2020 andalso EYR =< 2030;

valid(<<"hgt">>, <<S/binary>>) -> valid(<<"hgt">>, parse_height(S));
valid(<<"hgt">>, {H, cm}) when H >= 150, H =< 193 -> true;
valid(<<"hgt">>, {H, in}) when H >= 59, H =< 76 -> true;

valid(<<"hcl">>, <<"#", Digits/binary>>) -> all_hex(Digits);

valid(<<"ecl">>, <<"amb">>) -> true;
valid(<<"ecl">>, <<"blu">>) -> true;
valid(<<"ecl">>, <<"brn">>) -> true;
valid(<<"ecl">>, <<"gry">>) -> true;
valid(<<"ecl">>, <<"grn">>) -> true;
valid(<<"ecl">>, <<"hzl">>) -> true;
valid(<<"ecl">>, <<"oth">>) -> true;

valid(<<"pid">>, <<PID/binary>>) when byte_size(PID) == 9 -> all_digits(PID).

parse_height(Input) ->
    {Size, UnitDesc} = split_binary(Input, byte_size(Input) - 2),
    Height = binary_to_integer(Size),
    Unit = case UnitDesc of <<"cm">> -> cm; <<"in">> -> in end,
    {Height, Unit}.

all_hex(<<"">>) -> true;
all_hex(<<A, Rest/binary>>) when (A >= $a andalso A =< $f) orelse (A >= $0 andalso A =< $9) -> all_hex(Rest);
all_hex(_) -> false.

all_digits(<<"">>) -> true;
all_digits(<<A, Rest/binary>>) when A >= $0, A =< $9 -> all_digits(Rest);
all_digits(_) -> false.