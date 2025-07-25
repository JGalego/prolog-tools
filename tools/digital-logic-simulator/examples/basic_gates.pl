#!/usr/bin/env swipl
/**
 * Basic Digital Logic Gates Example
 * 
 * This example demonstrates basic logic gate simulation
 * using the digital logic simulator.
 */

:- use_module('../src/logic_simulator').

% Example: Basic gate operations
demo_basic_gates :-
    format('=== Basic Logic Gates Demo ===~n'),
    
    % Test AND gate
    format('~n--- AND Gate Truth Table ---~n'),
    format('A | B | Output~n'),
    format('--|---|-------~n'),
    test_and_gate(0, 0, Out1), format('0 | 0 | ~w~n', [Out1]),
    test_and_gate(0, 1, Out2), format('0 | 1 | ~w~n', [Out2]),
    test_and_gate(1, 0, Out3), format('1 | 0 | ~w~n', [Out3]),
    test_and_gate(1, 1, Out4), format('1 | 1 | ~w~n', [Out4]),
    
    % Test OR gate
    format('~n--- OR Gate Truth Table ---~n'),
    format('A | B | Output~n'),
    format('--|---|-------~n'),
    test_or_gate(0, 0, Out5), format('0 | 0 | ~w~n', [Out5]),
    test_or_gate(0, 1, Out6), format('0 | 1 | ~w~n', [Out6]),
    test_or_gate(1, 0, Out7), format('1 | 0 | ~w~n', [Out7]),
    test_or_gate(1, 1, Out8), format('1 | 1 | ~w~n', [Out8]),
    
    % Test NOT gate
    format('~n--- NOT Gate Truth Table ---~n'),
    format('A | Output~n'),
    format('--|-------~n'),
    test_not_gate(0, Out9), format('0 | ~w~n', [Out9]),
    test_not_gate(1, Out10), format('1 | ~w~n', [Out10]),
    
    % Test XOR gate
    format('~n--- XOR Gate Truth Table ---~n'),
    format('A | B | Output~n'),
    format('--|---|-------~n'),
    test_xor_gate(0, 0, Out11), format('0 | 0 | ~w~n', [Out11]),
    test_xor_gate(0, 1, Out12), format('0 | 1 | ~w~n', [Out12]),
    test_xor_gate(1, 0, Out13), format('1 | 0 | ~w~n', [Out13]),
    test_xor_gate(1, 1, Out14), format('1 | 1 | ~w~n', [Out14]).

% Example: Combinational circuits
demo_combinational_circuits :-
    format('~n=== Combinational Circuits Demo ===~n'),
    
    % Half Adder
    format('~n--- Half Adder ---~n'),
    format('A | B | Sum | Carry~n'),
    format('--|---|-----|------~n'),
    test_half_adder(0, 0, Sum1, Carry1), format('0 | 0 | ~w   | ~w~n', [Sum1, Carry1]),
    test_half_adder(0, 1, Sum2, Carry2), format('0 | 1 | ~w   | ~w~n', [Sum2, Carry2]),
    test_half_adder(1, 0, Sum3, Carry3), format('1 | 0 | ~w   | ~w~n', [Sum3, Carry3]),
    test_half_adder(1, 1, Sum4, Carry4), format('1 | 1 | ~w   | ~w~n', [Sum4, Carry4]),
    
    % Full Adder
    format('~n--- Full Adder ---~n'),
    format('A | B | Cin | Sum | Cout~n'),
    format('--|---|-----|-----|-----~n'),
    test_full_adder(0, 0, 0, S1, C1), format('0 | 0 | 0   | ~w   | ~w~n', [S1, C1]),
    test_full_adder(0, 0, 1, S2, C2), format('0 | 0 | 1   | ~w   | ~w~n', [S2, C2]),
    test_full_adder(0, 1, 0, S3, C3), format('0 | 1 | 0   | ~w   | ~w~n', [S3, C3]),
    test_full_adder(0, 1, 1, S4, C4), format('0 | 1 | 1   | ~w   | ~w~n', [S4, C4]),
    test_full_adder(1, 0, 0, S5, C5), format('1 | 0 | 0   | ~w   | ~w~n', [S5, C5]),
    test_full_adder(1, 0, 1, S6, C6), format('1 | 0 | 1   | ~w   | ~w~n', [S6, C6]),
    test_full_adder(1, 1, 0, S7, C7), format('1 | 1 | 0   | ~w   | ~w~n', [S7, C7]),
    test_full_adder(1, 1, 1, S8, C8), format('1 | 1 | 1   | ~w   | ~w~n', [S8, C8]).

% Example: Sequential circuits
demo_sequential_circuits :-
    format('~n=== Sequential Circuits Demo ===~n'),
    
    % SR Latch
    format('~n--- SR Latch Simulation ---~n'),
    format('S | R | Q | Q\'~n'),
    format('--|---|---|----~n'),
    
    % Initialize latch state
    reset_sequential_state,
    
    % Test different inputs
    test_sr_latch(0, 0, Q1, Qn1), format('0 | 0 | ~w | ~w~n', [Q1, Qn1]),
    test_sr_latch(1, 0, Q2, Qn2), format('1 | 0 | ~w | ~w~n', [Q2, Qn2]),
    test_sr_latch(0, 0, Q3, Qn3), format('0 | 0 | ~w | ~w~n', [Q3, Qn3]),
    test_sr_latch(0, 1, Q4, Qn4), format('0 | 1 | ~w | ~w~n', [Q4, Qn4]),
    test_sr_latch(0, 0, Q5, Qn5), format('0 | 0 | ~w | ~w~n', [Q5, Qn5]),
    
    % D Flip-Flop
    format('~n--- D Flip-Flop Simulation ---~n'),
    format('D | CLK | Q~n'),
    format('--|-----|--~n'),
    reset_sequential_state,
    test_d_flipflop(0, 0, Q6), format('0 | 0   | ~w~n', [Q6]),
    test_d_flipflop(0, 1, Q7), format('0 | 1   | ~w~n', [Q7]),
    test_d_flipflop(1, 1, Q8), format('1 | 1   | ~w~n', [Q8]),
    test_d_flipflop(1, 0, Q9), format('1 | 0   | ~w~n', [Q9]),
    test_d_flipflop(0, 1, Q10), format('0 | 1   | ~w~n', [Q10]).

% Example: Multi-bit operations
demo_multibit_operations :-
    format('~n=== Multi-bit Operations Demo ===~n'),
    
    % 4-bit binary addition
    format('~n--- 4-bit Binary Addition ---~n'),
    A = [1, 0, 1, 1],  % 11 in decimal
    B = [0, 1, 1, 0],  % 6 in decimal
    test_4bit_adder(A, B, Sum, Carry),
    format('A:     ~w (decimal: ~w)~n', [A, 11]),
    format('B:     ~w (decimal: ~w)~n', [B, 6]),
    format('Sum:   ~w (decimal: ~w)~n', [Sum, 17]),
    format('Carry: ~w~n', [Carry]),
    
    % 4-bit comparator
    format('~n--- 4-bit Comparator ---~n'),
    X = [1, 0, 1, 0],  % 10 in decimal
    Y = [0, 1, 1, 1],  % 7 in decimal
    test_4bit_comparator(X, Y, Greater, Equal, Less),
    format('X:      ~w (decimal: ~w)~n', [X, 10]),
    format('Y:      ~w (decimal: ~w)~n', [Y, 7]),
    format('X > Y:  ~w~n', [Greater]),
    format('X = Y:  ~w~n', [Equal]),
    format('X < Y:  ~w~n', [Less]).

% Helper predicates for testing basic gates
test_and_gate(A, B, Output) :-
    simulate_circuit([
        gate(g1, and, [A, B], Output)
    ], _).

test_or_gate(A, B, Output) :-
    simulate_circuit([
        gate(g1, or, [A, B], Output)
    ], _).

test_not_gate(A, Output) :-
    simulate_circuit([
        gate(g1, not, [A], Output)
    ], _).

test_xor_gate(A, B, Output) :-
    simulate_circuit([
        gate(g1, xor, [A, B], Output)
    ], _).

% Combinational circuit helpers
test_half_adder(A, B, Sum, Carry) :-
    simulate_circuit([
        gate(xor1, xor, [A, B], Sum),
        gate(and1, and, [A, B], Carry)
    ], _).

test_full_adder(A, B, Cin, Sum, Cout) :-
    simulate_circuit([
        gate(xor1, xor, [A, B], s1),
        gate(xor2, xor, [s1, Cin], Sum),
        gate(and1, and, [A, B], c1),
        gate(and2, and, [s1, Cin], c2),
        gate(or1, or, [c1, c2], Cout)
    ], _).

% Sequential circuit helpers
:- dynamic latch_state/2.

reset_sequential_state :-
    retractall(latch_state(_, _)),
    assertz(latch_state(q, 0)),
    assertz(latch_state(qn, 1)).

test_sr_latch(S, R, Q, Qn) :-
    latch_state(q, PrevQ),
    latch_state(qn, PrevQn),
    simulate_sr_latch(S, R, PrevQ, PrevQn, Q, Qn),
    retractall(latch_state(_, _)),
    assertz(latch_state(q, Q)),
    assertz(latch_state(qn, Qn)).

simulate_sr_latch(S, R, PrevQ, PrevQn, Q, Qn) :-
    (   S = 1, R = 0 -> Q = 1, Qn = 0
    ;   S = 0, R = 1 -> Q = 0, Qn = 1
    ;   S = 0, R = 0 -> Q = PrevQ, Qn = PrevQn
    ;   S = 1, R = 1 -> Q = 0, Qn = 0  % Invalid state
    ).

:- dynamic flipflop_state/1.

test_d_flipflop(D, CLK, Q) :-
    (   flipflop_state(PrevQ) -> true ; (assertz(flipflop_state(0)), PrevQ = 0)),
    simulate_d_flipflop(D, CLK, PrevQ, Q),
    retractall(flipflop_state(_)),
    assertz(flipflop_state(Q)).

simulate_d_flipflop(D, CLK, PrevQ, Q) :-
    (   CLK = 1 -> Q = D
    ;   Q = PrevQ
    ).

% Multi-bit operation helpers
test_4bit_adder(A, B, Sum, Carry) :-
    % Simulate 4-bit ripple carry adder
    A = [A3, A2, A1, A0],
    B = [B3, B2, B1, B0],
    test_full_adder(A0, B0, 0, S0, C0),
    test_full_adder(A1, B1, C0, S1, C1),
    test_full_adder(A2, B2, C1, S2, C2),
    test_full_adder(A3, B3, C2, S3, C3),
    Sum = [S3, S2, S1, S0],
    Carry = C3.

test_4bit_comparator(X, Y, Greater, Equal, Less) :-
    X = [X3, X2, X1, X0],
    Y = [Y3, Y2, Y1, Y0],
    
    % Compare bit by bit
    xor_gate(X3, Y3, Diff3),
    xor_gate(X2, Y2, Diff2),
    xor_gate(X1, Y1, Diff1),
    xor_gate(X0, Y0, Diff0),
    
    % Equal if no differences
    or_gate(Diff3, Diff2, T1),
    or_gate(T1, Diff1, T2),
    or_gate(T2, Diff0, AnyDiff),
    not_gate(AnyDiff, Equal),
    
    % Greater than logic (simplified)
    Greater is (X3 > Y3; (X3 =:= Y3, X2 > Y2); (X3 =:= Y3, X2 =:= Y2, X1 > Y1); 
                (X3 =:= Y3, X2 =:= Y2, X1 =:= Y1, X0 > Y0)),
    
    % Less than
    not_gate(Greater, NotGreater),
    not_gate(Equal, NotEqual),
    and_gate(NotGreater, NotEqual, Less).

% Basic gate operations (if not available from main module)
and_gate(A, B, Output) :- Output is A * B.
or_gate(A, B, Output) :- Output is max(A, B).
not_gate(A, Output) :- Output is 1 - A.
xor_gate(A, B, Output) :- Output is (A + B) mod 2.

% Circuit simulation (simplified interface)
simulate_circuit(Gates, State) :-
    % This would interface with the main logic_simulator module
    % For now, just process gates sequentially
    process_gates(Gates, State).

process_gates([], []).
process_gates([gate(ID, Type, Inputs, Output)|Rest], [result(ID, Output)|StateRest]) :-
    apply_gate_function(Type, Inputs, Output),
    process_gates(Rest, StateRest).

apply_gate_function(and, [A, B], Output) :- and_gate(A, B, Output).
apply_gate_function(or, [A, B], Output) :- or_gate(A, B, Output).
apply_gate_function(not, [A], Output) :- not_gate(A, Output).
apply_gate_function(xor, [A, B], Output) :- xor_gate(A, B, Output).

% Run all demos
run_logic_demo :-
    demo_basic_gates,
    demo_combinational_circuits,
    demo_sequential_circuits,
    demo_multibit_operations,
    format('~n=== Digital logic simulation demo completed ===~n').

% Query interface
?- write('Loading digital logic gates demo...'), nl.
?- write('Type "run_logic_demo." to see all examples'), nl.
?- write('Available demos: demo_basic_gates, demo_combinational_circuits, demo_sequential_circuits, demo_multibit_operations'), nl.
