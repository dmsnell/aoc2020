-module(day8).
-behavior(aoc).

-export([input_type/0, parse_input/1, p1/1, p2/1]).
-export([run/1, run/2]).

-type opcode() :: acc | jmp | nop.
-type instruction() :: {opcode(), integer()}.
-type program() :: #{non_neg_integer() => instruction()}.

-record(s, {
    acc  = 0   :: integer(),
    code = #{} :: program(),
    ooc  = 0   :: non_neg_integer(),
    pc   = 1   :: non_neg_integer()
}).

-type profiler(ProfilerState) :: {ProfilerState, fun ((ProfilerState, #s{}) -> ProfilerState)}.
-type runopts(ProfilerState) :: #{
    isolation => spawned | in_process,
    profiler  => profiler(ProfilerState),
    timeout   => non_neg_integer() | infinity
}.

-export_type([opcode/0, instruction/0, program/0, profiler/1, runopts/1]).

input_type() -> lines.

parse_input(Lines) ->
    compile(lists:map(fun decode/1, Lines)).

p1(Program) ->
    try run(Program, #{profiler => profile_only_once(new, maps:size(Program))}) of
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

p2(Program, PCGuess) ->
    case p2_run(swap_op(Program, PCGuess)) of
        Acc when is_integer(Acc) ->
            {Acc, {swap, #{
                pc  => PCGuess,
                was => maps:get(PCGuess + 1, Program),
                now => swap_op(maps:get(PCGuess + 1, Program))
            }}};
        {loop_detected, _PC} -> p2(Program, PCGuess + 1)
    end.

p2_run(Program) ->
    try run(Program, #{profiler => profile_only_once(new, maps:size(Program)), timeout => 1000}) of
        {_Pstate, #s{acc = Acc}} -> Acc
    catch
        error:{already_called, #s{pc = PC}} ->
            {loop_detected, PC}
    end.

swap_op({acc, _V} = Op) -> Op;
swap_op({jmp, V}) -> {nop, V};
swap_op({nop, V}) -> {jmp, V}.

swap_op(Program, PCGuess) ->
    maps:update_with(PCGuess + 1, fun swap_op/1, Program).

%% Profilers

profile_only_once(new, Length) ->
    {atomics:new(Length, []), fun profile_only_once/2};

profile_only_once(Called, #s{pc = PC} = State) ->
    case atomics:add_get(Called, PC, 1) of
        1 -> Called;
        _ -> erlang:error({already_called, State})
    end.

%% Emulator

run(Program) ->
    run(Program, #{}).

run(Program, RunOpts) when is_map(RunOpts) ->
    #{timeout := Timeout} = WithDefaults = maps:merge(#{
        isolation => in_process,
        profiler  => {nil, fun (PState, _State) -> PState end},
        timeout   => 1000
    }, RunOpts),
    State = #s{code = Program, ooc = maps:size(Program)},
    case maps:get(isolation, WithDefaults) of
        spawned    -> utils:isolated(fun () -> run_loop(WithDefaults, State) end, Timeout);
        in_process -> run_loop(WithDefaults, State)
    end.

run_loop(#{profiler := {InitialPState, _Profiler}} = RunOpts, State) ->
    run_loop(RunOpts, State, InitialPState).

run_loop(_RunOpts, #s{ooc = OOC, pc = PC} = State, PState) when PC > OOC ->
    {PState, State};
run_loop(#{profiler := {_IPState, Profiler}} = RunOpts, #s{code = Code, pc = PC} = State, PState) ->
    NextPState = Profiler(PState, State),
    case execute(State, maps:get(PC, Code)) of
        {continue, NextState} -> run_loop(RunOpts, NextState, NextPState)
    end.

compile(Program) when is_list(Program) ->
    Pairs = lists:zip(lists:seq(1, length(Program)), Program),
    maps:from_list(Pairs).

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