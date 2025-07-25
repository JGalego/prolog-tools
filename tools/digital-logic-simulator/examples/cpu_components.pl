#!/usr/bin/env swipl
/**
 * CPU Components Simulation Example
 * 
 * This example demonstrates simulation of basic CPU components
 * like ALU, registers, and simple instruction execution.
 */

:- use_module('../src/logic_simulator').

% Example: Arithmetic Logic Unit (ALU) simulation
demo_alu_simulation :-
    format('=== ALU Simulation Demo ===~n'),
    
    % 4-bit ALU operations
    format('~n--- 4-bit ALU Operations ---~n'),
    A = [1, 0, 1, 1],  % 11 in decimal
    B = [0, 1, 1, 0],  % 6 in decimal
    
    % Addition
    test_alu_operation(A, B, add, Result1, Flags1),
    format('ADD: ~w + ~w = ~w (Flags: ~w)~n', [A, B, Result1, Flags1]),
    
    % Subtraction
    test_alu_operation(A, B, sub, Result2, Flags2),
    format('SUB: ~w - ~w = ~w (Flags: ~w)~n', [A, B, Result2, Flags2]),
    
    % Logical AND
    test_alu_operation(A, B, and, Result3, Flags3),
    format('AND: ~w & ~w = ~w (Flags: ~w)~n', [A, B, Result3, Flags3]),
    
    % Logical OR
    test_alu_operation(A, B, or, Result4, Flags4),
    format('OR:  ~w | ~w = ~w (Flags: ~w)~n', [A, B, Result4, Flags4]),
    
    % Shift left
    test_alu_operation(A, [0, 0, 0, 1], shl, Result5, Flags5),
    format('SHL: ~w << 1 = ~w (Flags: ~w)~n', [A, Result5, Flags5]).

% Example: Register file simulation
demo_register_file :-
    format('~n=== Register File Demo ===~n'),
    
    % Initialize register file
    init_register_file,
    
    % Write to registers
    format('~n--- Register Write Operations ---~n'),
    write_register(r1, [1, 0, 1, 1]),  % Write 11 to R1
    write_register(r2, [0, 1, 1, 0]),  % Write 6 to R2
    write_register(r3, [1, 1, 1, 1]),  % Write 15 to R3
    
    format('Written: R1=11, R2=6, R3=15~n'),
    
    % Read from registers
    format('~n--- Register Read Operations ---~n'),
    read_register(r1, Val1),
    read_register(r2, Val2),
    read_register(r3, Val3),
    
    format('Read: R1=~w, R2=~w, R3=~w~n', [Val1, Val2, Val3]),
    
    % Show all register contents
    format('~n--- Register File Contents ---~n'),
    show_register_file.

% Example: Simple CPU instruction execution
demo_simple_cpu :-
    format('~n=== Simple CPU Simulation ===~n'),
    
    % Initialize CPU state
    init_cpu_state,
    
    % Load program
    Program = [
        instr(1, load, r1, [0, 1, 0, 1]),    % LOAD R1, 5
        instr(2, load, r2, [0, 0, 1, 1]),    % LOAD R2, 3
        instr(3, add, r3, [r1, r2]),         % ADD R3, R1, R2
        instr(4, sub, r4, [r1, r2]),         % SUB R4, R1, R2
        instr(5, store, mem_addr(8), [r3])   % STORE R3 -> MEM[8]
    ],
    
    format('~n--- Program Loaded ---~n'),
    print_program(Program),
    
    format('~n--- Executing Instructions ---~n'),
    execute_program(Program).

% Example: Memory hierarchy simulation
demo_memory_hierarchy :-
    format('~n=== Memory Hierarchy Demo ===~n'),
    
    % Initialize memory components
    init_memory_hierarchy,
    
    % Cache simulation
    format('~n--- Cache Operations ---~n'),
    
    % Memory writes
    memory_write(addr(4), [1, 0, 1, 0]),   % Write 10 to address 4
    memory_write(addr(8), [1, 1, 0, 0]),   % Write 12 to address 8
    
    % Memory reads (cache misses)
    memory_read(addr(4), Data1, Status1),
    format('Read addr 4: Data=~w, Status=~w~n', [Data1, Status1]),
    
    % Memory read (cache hit)
    memory_read(addr(4), Data2, Status2),
    format('Read addr 4: Data=~w, Status=~w~n', [Data2, Status2]),
    
    % Show cache state
    show_cache_state.

% Example: Pipeline simulation
demo_pipeline_simulation :-
    format('~n=== Pipeline Simulation Demo ===~n'),
    
    % Initialize 5-stage pipeline
    init_pipeline,
    
    % Instruction sequence
    Instructions = [
        add(r1, r2, r3),
        sub(r4, r1, r2),
        load(r5, mem_addr(16)),
        store(mem_addr(20), r3),
        and(r6, r4, r5)
    ],
    
    format('~n--- Pipeline Execution ---~n'),
    simulate_pipeline(Instructions, 8).  % Simulate 8 clock cycles

% ALU operation implementation
test_alu_operation(A, B, Operation, Result, Flags) :-
    alu_4bit(A, B, Operation, Result, Flags).

alu_4bit(A, B, add, Result, Flags) :-
    binary_add_4bit(A, B, Result, Carry),
    calculate_flags(Result, Carry, 0, Flags).

alu_4bit(A, B, sub, Result, Flags) :-
    binary_subtract_4bit(A, B, Result, Borrow),
    calculate_flags(Result, 0, Borrow, Flags).

alu_4bit(A, B, and, Result, Flags) :-
    binary_and_4bit(A, B, Result),
    calculate_flags(Result, 0, 0, Flags).

alu_4bit(A, B, or, Result, Flags) :-
    binary_or_4bit(A, B, Result),
    calculate_flags(Result, 0, 0, Flags).

alu_4bit(A, [0, 0, 0, Shift], shl, Result, Flags) :-
    binary_shift_left(A, Shift, Result, Carry),
    calculate_flags(Result, Carry, 0, Flags).

% Binary arithmetic operations
binary_add_4bit([A3,A2,A1,A0], [B3,B2,B1,B0], [S3,S2,S1,S0], Cout) :-
    full_adder(A0, B0, 0, S0, C0),
    full_adder(A1, B1, C0, S1, C1),
    full_adder(A2, B2, C1, S2, C2),
    full_adder(A3, B3, C2, S3, Cout).

binary_subtract_4bit(A, B, Result, Borrow) :-
    % Implement subtraction using 2's complement addition
    binary_complement_4bit(B, NotB),
    binary_add_4bit(NotB, [0,0,0,1], TwosCompB, _),
    binary_add_4bit(A, TwosCompB, Result, Carry),
    Borrow is 1 - Carry.

binary_and_4bit([A3,A2,A1,A0], [B3,B2,B1,B0], [R3,R2,R1,R0]) :-
    R3 is A3 * B3,
    R2 is A2 * B2,
    R1 is A1 * B1,
    R0 is A0 * B0.

binary_or_4bit([A3,A2,A1,A0], [B3,B2,B1,B0], [R3,R2,R1,R0]) :-
    R3 is max(A3, B3),
    R2 is max(A2, B2),
    R1 is max(A1, B1),
    R0 is max(A0, B0).

binary_complement_4bit([A3,A2,A1,A0], [R3,R2,R1,R0]) :-
    R3 is 1 - A3,
    R2 is 1 - A2,
    R1 is 1 - A1,
    R0 is 1 - A0.

binary_shift_left([A3,A2,A1,A0], 1, [A2,A1,A0,0], A3).
binary_shift_left(Input, 0, Input, 0).

full_adder(A, B, Cin, Sum, Cout) :-
    Sum is (A + B + Cin) mod 2,
    Cout is (A + B + Cin) // 2.

calculate_flags([0,0,0,0], Carry, Borrow, [zero(1), carry(Carry), borrow(Borrow)]).
calculate_flags(Result, Carry, Borrow, [zero(0), carry(Carry), borrow(Borrow)]) :-
    Result \= [0,0,0,0].

% Register file implementation
:- dynamic register/2.

init_register_file :-
    retractall(register(_, _)),
    % Initialize 8 registers to zero
    forall(between(0, 7, N), (
        atom_concat(r, N, RegName),
        assertz(register(RegName, [0,0,0,0]))
    )).

write_register(RegName, Value) :-
    retractall(register(RegName, _)),
    assertz(register(RegName, Value)).

read_register(RegName, Value) :-
    register(RegName, Value).

show_register_file :-
    forall(register(RegName, Value), 
           format('~w: ~w~n', [RegName, Value])).

% CPU state management
:- dynamic cpu_state/2.

init_cpu_state :-
    init_register_file,
    retractall(cpu_state(_, _)),
    assertz(cpu_state(pc, 1)),           % Program counter
    assertz(cpu_state(flags, [0,0,0])).  % Zero, Carry, Borrow

% Simple instruction execution
execute_program([]).
execute_program([Instr|Rest]) :-
    execute_instruction(Instr),
    execute_program(Rest).

execute_instruction(instr(PC, load, Reg, Value)) :-
    format('PC ~w: LOAD ~w, ~w~n', [PC, Reg, Value]),
    write_register(Reg, Value).

execute_instruction(instr(PC, add, Dest, [Src1, Src2])) :-
    format('PC ~w: ADD ~w, ~w, ~w~n', [PC, Dest, Src1, Src2]),
    read_register(Src1, Val1),
    read_register(Src2, Val2),
    alu_4bit(Val1, Val2, add, Result, Flags),
    write_register(Dest, Result),
    retractall(cpu_state(flags, _)),
    assertz(cpu_state(flags, Flags)).

execute_instruction(instr(PC, sub, Dest, [Src1, Src2])) :-
    format('PC ~w: SUB ~w, ~w, ~w~n', [PC, Dest, Src1, Src2]),
    read_register(Src1, Val1),
    read_register(Src2, Val2),
    alu_4bit(Val1, Val2, sub, Result, Flags),
    write_register(Dest, Result),
    retractall(cpu_state(flags, _)),
    assertz(cpu_state(flags, Flags)).

execute_instruction(instr(PC, store, mem_addr(Addr), [Src])) :-
    format('PC ~w: STORE ~w -> MEM[~w]~n', [PC, Src, Addr]),
    read_register(Src, Value),
    % Simulate memory store
    assertz(memory(Addr, Value)).

print_program([]).
print_program([instr(PC, Op, Dest, Args)|Rest]) :-
    format('~w: ~w ~w, ~w~n', [PC, Op, Dest, Args]),
    print_program(Rest).

% Memory hierarchy simulation
:- dynamic cache_line/3, memory/2.

init_memory_hierarchy :-
    retractall(cache_line(_, _, _)),
    retractall(memory(_, _)).

memory_write(addr(Address), Data) :-
    % Write to cache and mark as dirty
    assertz(cache_line(Address, Data, dirty)),
    format('Cache write: addr ~w = ~w~n', [Address, Data]).

memory_read(addr(Address), Data, Status) :-
    (   cache_line(Address, Data, _)
    ->  Status = cache_hit
    ;   % Cache miss - load from memory
        (   memory(Address, Data)
        ->  true
        ;   Data = [0,0,0,0]  % Default value
        ),
        assertz(cache_line(Address, Data, clean)),
        Status = cache_miss
    ).

show_cache_state :-
    format('Cache contents:~n'),
    forall(cache_line(Addr, Data, Status),
           format('  addr ~w: ~w (~w)~n', [Addr, Data, Status])).

% Pipeline simulation
:- dynamic pipeline_stage/3.

init_pipeline :-
    retractall(pipeline_stage(_, _, _)),
    % Initialize 5-stage pipeline: IF, ID, EX, MEM, WB
    assertz(pipeline_stage(if, 0, empty)),
    assertz(pipeline_stage(id, 0, empty)),
    assertz(pipeline_stage(ex, 0, empty)),
    assertz(pipeline_stage(mem, 0, empty)),
    assertz(pipeline_stage(wb, 0, empty)).

simulate_pipeline(Instructions, NumCycles) :-
    format('Cycle | IF    | ID    | EX    | MEM   | WB~n'),
    format('------|-------|-------|-------|-------|-------~n'),
    simulate_pipeline_cycles(Instructions, 1, NumCcycles).

simulate_pipeline_cycles(_, Cycle, NumCycles) :-
    Cycle > NumCycles, !.
simulate_pipeline_cycles(Instructions, Cycle, NumCycles) :-
    advance_pipeline(Instructions, Cycle),
    show_pipeline_state(Cycle),
    NextCycle is Cycle + 1,
    simulate_pipeline_cycles(Instructions, NextCycle, NumCycles).

advance_pipeline(Instructions, Cycle) :-
    % Move instructions through pipeline stages
    % (Simplified implementation)
    (   Cycle =< length(Instructions)
    ->  nth1(Cycle, Instructions, NewInstr)
    ;   NewInstr = empty
    ),
    retractall(pipeline_stage(if, _, _)),
    assertz(pipeline_stage(if, Cycle, NewInstr)).

show_pipeline_state(Cycle) :-
    pipeline_stage(if, _, IF),
    pipeline_stage(id, _, ID),
    pipeline_stage(ex, _, EX),
    pipeline_stage(mem, _, MEM),
    pipeline_stage(wb, _, WB),
    format('~w     | ~w | ~w | ~w | ~w | ~w~n', 
           [Cycle, IF, ID, EX, MEM, WB]).

% Run all demos
run_cpu_demo :-
    demo_alu_simulation,
    demo_register_file,
    demo_simple_cpu,
    demo_memory_hierarchy,
    demo_pipeline_simulation,
    format('~n=== CPU components simulation demo completed ===~n').

% Query interface
?- write('Loading CPU components simulation demo...'), nl.
?- write('Type "run_cpu_demo." to see all examples'), nl.
?- write('Available demos: demo_alu_simulation, demo_register_file, demo_simple_cpu, demo_memory_hierarchy, demo_pipeline_simulation'), nl.
