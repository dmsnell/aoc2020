-module(day14).
-behavior(aoc).

-include_lib("eunit/include/eunit.hrl").

-export([input_type/0, parse_input/1, p1/1, p2/1]).

input_type() -> lines.

parse_input(Lines) -> compile(Lines).

p1(Program) ->
    {Memory, _Mask} = eval(#{}, {or_mask, 0, and_mask, 0}, Program),
    lists:sum(maps:values(Memory)).

p2(Program) ->
    {Memory, _Mask} = eval_p2(#{}, [], Program),
    lists:sum(maps:values(Memory)).

%% Internal functions

eval(Memory, Mask, []) ->
    {Memory, Mask};
eval(Memory, _Mask, [{mask, Mask} | Program]) ->
    eval(Memory, decode_mask(Mask), Program);
eval(Memory, Mask, [{memset, Address, Value} | Program]) ->
    eval(maps:put(Address, mask(Mask, Value), Memory), Mask, Program).

eval_p2(Memory, Mask, []) ->
    {Memory, Mask};
eval_p2(Memory, _Mask, [{mask, Mask} | Program]) ->
    eval_p2(Memory, Mask, Program);
eval_p2(Memory, Mask, [{memset, Address, Value} | Program]) ->
    Addresses = masked_address(Mask, Address),
    Updates = maps:from_list([{MaskedAddress, Value} || MaskedAddress <- Addresses]),
    eval_p2(maps:merge(Memory, Updates), Mask, Program).

bits(Value) when is_integer(Value), Value < 16#FFFFFFFFF ->
    lists:droplast(bits(16#1000000000 bor Value));
bits(Value) when is_integer(Value) ->
    bits(Value, []).

bits(0, Bits) -> lists:reverse(Bits);
bits(V, Bits) -> bits(V div 2, [V band 1 | Bits]).

unbits(Bits) when is_list(Bits) ->
    unbits(Bits, 0, 1).

unbits([], Dec, _Place) -> Dec;
unbits([0 | Bits], Dec, Place) -> unbits(Bits, Dec, Place * 2);
unbits([1 | Bits], Dec, Place) -> unbits(Bits, Dec + Place, Place * 2).

mask({or_mask, Or, and_mask, And}, Value) ->
    (Value band (bnot And)) bor Or.

masked_address(Mask, Address) when is_integer(Address) ->
    Addresses = masked_address(Mask, bits(Address), []),
    split_addresses(lists:flatten(Addresses)).

masked_address([], _Bits, Out) -> lists:reverse(Out);
masked_address([0 | Mask], [Bit | Bits], Out) -> masked_address(Mask, Bits, [Bit | Out]);
masked_address([1 | Mask], [_Bit | Bits], Out) -> masked_address(Mask, Bits, [1 | Out]);
masked_address([x | Mask], [_Bit | Bits], Out) -> [
    masked_address(Mask, Bits, [0 | Out]),
    masked_address(Mask, Bits, [1 | Out])
].

split_addresses(Input) when is_list(Input) ->
    split_addresses(Input, []).

split_addresses([], Addresses) -> Addresses;
split_addresses(Input, Addresses) ->
    {Address, Remaining} = lists:split(36, Input),
    split_addresses(Remaining, [unbits(Address) | Addresses]).

parse_mask(<<Mask/binary>>) -> parse_mask(Mask, []).

parse_mask(<<>>, Mask) -> Mask;
parse_mask(<<"X", Rest/binary>>, Mask) -> parse_mask(Rest, [x | Mask]);
parse_mask(<<"1", Rest/binary>>, Mask) -> parse_mask(Rest, [1 | Mask]);
parse_mask(<<"0", Rest/binary>>, Mask) -> parse_mask(Rest, [0 | Mask]).

decode_mask(Mask) when is_list(Mask) ->
    decode_mask(Mask, 0, 0, 1).

decode_mask([], Or, And, _Bit) -> {or_mask, Or, and_mask, And};
decode_mask([x | Mask], Or, And, Bit) -> decode_mask(Mask, Or, And, Bit * 2);
decode_mask([1 | Mask], Or, And, Bit) -> decode_mask(Mask, Or bor Bit, And, Bit * 2);
decode_mask([0 | Mask], Or, And, Bit) -> decode_mask(Mask, Or, And bor Bit, Bit * 2).

compile(Lines) ->
    [element(1, day7:parse_or([fun parse_mask_line/1, fun parse_memset/1], Line)) || Line <- Lines].

parse_mask_line(<<"mask = ", Mask/binary>>) ->
    {{mask, parse_mask(Mask)}, <<>>}.

parse_memset(<<"mem[", Rest/binary>>) ->
    {Address, <<"] = ", Rest1/binary>>} = day7:parse_count(Rest),
    {Value, <<>>} = day7:parse_count(Rest1),
    {{memset, Address, Value}, <<>>}.


-ifdef(EUNIT).

example1_test_() ->
    Mask = parse_mask(<<"XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X">>),
    [
        ?_assertEqual(73, mask(Mask, 11)),
        ?_assertEqual(101, mask(Mask, 101)),
        ?_assertEqual(64, mask(Mask, 0))
    ].

-endif.