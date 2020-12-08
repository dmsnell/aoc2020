-module(day8).
-behavior(aoc).

-export([input_type/0, parse_input/1, p1/1, p2/1]).
-export([run/1, run/2, run/3]).

-type opcode() :: acc | jmp | nop.
-type instruction() :: {opcode(), integer()}.
-type program() :: list(instruction()).

-record(s, {
    code = [] :: program(),
    pc   = 1  :: non_neg_integer(),
    acc  = 0  :: integer()
}).

-type profiler(ProfilerState) :: {ProfilerState, fun ((ProfilerState, #s{}) -> ProfilerState)}.

input_type() -> lines.

parse_input(Lines) ->
    lists:map(fun decode/1, Lines).

p1(Program) ->
    try run(Program, profile_only_once(new)) of
        _NotExpected ->
            terminated
    catch
        error:{already_called, #s{acc = Acc}} ->
            Acc
    end.

p2(Program) ->
    case p2_run(Program) of
        Acc when is_integer(Acc) -> Acc;
        {loop_detected, _PC} -> p2(Program, 0)
    end.

p2(Program, PCGuess) when PCGuess < length(Program) ->
    case p2_run(swap_op(Program, PCGuess)) of
        Acc when is_integer(Acc) ->
            {Acc, {swap, #{
                pc  => PCGuess,
                was => lists:nth(PCGuess + 1, Program),
                now => swap_op(lists:nth(PCGuess + 1, Program))
            }}};
        {loop_detected, _PC} -> p2(Program, PCGuess + 1)
    end.

p2_run(Program) ->
    try run(Program, profile_only_once(new), 1000) of
        {_Pstate, #s{acc = Acc}} -> Acc
    catch
        error:{already_called, #s{pc = PC}} ->
            {loop_detected, PC}
    end.

swap_op({acc, _V} = Op) -> Op;
swap_op({jmp, V}) -> {nop, V};
swap_op({nop, V}) -> {jmp, V}.

swap_op(Program, PCGuess) ->
    {Before, [Op | After]} = lists:split(PCGuess, Program),
    Before ++ [swap_op(Op) | After].

%% Profilers

profile_only_once(new) ->
    {sets:new(), fun profile_only_once/2}.

profile_only_once(Called, #s{pc = PC} = State) ->
    case sets:is_element(PC, Called) of
        true  -> erlang:error({already_called, State});
        false -> sets:add_element(PC, Called)
    end.

%% Emulator

run(Program) ->
    run(Program, 1000).

run(Program, Timeout) when is_integer(Timeout) ->
    run(Program, {[], fun (_PState, #s{}) -> ok end}, Timeout);

run(Program, {_PState, PFunc} = Profiler) when is_function(PFunc) ->
    run(Program, Profiler, 1000).

run(Program, {_PState, PFunc} = Profiler, Timeout) when is_list(Program), is_function(PFunc) ->
    utils:isolated(fun () -> run_loop(Profiler, #s{code = Program}) end, Timeout).

run_loop({PState, _Profiler}, #s{code = Code, pc = PC} = State) when PC > length(Code) ->
    {PState, State};
run_loop({PState, Profiler}, #s{code = Code, pc = PC} = State) ->
    NextPState = Profiler(PState, State),
    case execute(State, lists:nth(PC, Code)) of
        {continue, NextState} -> run_loop({NextPState, Profiler}, NextState)
    end.

execute(#s{acc = Acc, pc = PC} = State, {acc, N}) when is_integer(N) ->
    {continue, State#s{acc = Acc + N, pc = PC + 1}};
execute(#s{pc = PC} = State, {jmp, RelOffset}) when is_integer(RelOffset) ->
    {continue, State#s{pc = PC + RelOffset}};
execute(#s{pc = PC} = State, {nop, _Ignored}) ->
    {continue, State#s{pc = PC + 1}}.

decode(<<Input/binary>>) ->
    {Opcode, <<" ", Rest1/binary>>} = parse_opcode(Input),
    {Value, <<"">>} = day7:parse_count(Rest1),
    {Opcode, Value}.

parse_opcode(<<"acc", Rest/binary>>) -> {acc, Rest};
parse_opcode(<<"jmp", Rest/binary>>) -> {jmp, Rest};
parse_opcode(<<"nop", Rest/binary>>) -> {nop, Rest}.