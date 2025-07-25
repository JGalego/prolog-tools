:- module(logic_simulator, [
    create_circuit/1,             % create_circuit(-CircuitID)
    add_gate/4,                   % add_gate(+CircuitID, +GateType, +Inputs, +Output)
    add_input/3,                  % add_input(+CircuitID, +InputName, +Value)
    add_output/2,                 % add_output(+CircuitID, +OutputName)
    connect_nodes/3,              % connect_nodes(+CircuitID, +From, +To)
    simulate_circuit/2,           % simulate_circuit(+CircuitID, -Results)
    step_simulation/2,            % step_simulation(+CircuitID, -StepResults)
    generate_truth_table/2,       % generate_truth_table(+CircuitID, -TruthTable)
    optimize_circuit/2,           % optimize_circuit(+CircuitID, -OptimizedCircuit)
    export_verilog/2,             % export_verilog(+CircuitID, +File)
    export_vhdl/2,                % export_vhdl(+CircuitID, +File)
    visualize_circuit/2,          % visualize_circuit(+CircuitID, +Format)
    load_circuit/2,               % load_circuit(+File, -CircuitID)
    save_circuit/2,               % save_circuit(+CircuitID, +File)
    start_web_interface/1,        % start_web_interface(+Port)
    validate_circuit/2,           % validate_circuit(+CircuitID, -Issues)
    analyze_timing/2,             % analyze_timing(+CircuitID, -Analysis)
    detect_hazards/2              % detect_hazards(+CircuitID, -Hazards)
]).

/** <module> Interactive Digital Logic Simulator

A web-based tool for designing and simulating digital circuits with Prolog
backend for logic evaluation and signal propagation.

@author Prolog Tools Collection
@version 1.0.0
@license MIT
*/

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_json)).
:- use_module(library(uuid)).

:- dynamic circuit/2.           % circuit(ID, Name)
:- dynamic gate/5.              % gate(CircuitID, GateID, Type, Inputs, Output)
:- dynamic connection/3.        % connection(CircuitID, From, To)
:- dynamic input_node/3.        % input_node(CircuitID, Name, Value)
:- dynamic output_node/2.       % output_node(CircuitID, Name)
:- dynamic signal_value/3.      % signal_value(CircuitID, Node, Value)
:- dynamic simulation_step/2.   % simulation_step(CircuitID, Step)

%% create_circuit(-CircuitID) is det.
%
%  Creates a new empty circuit.
%
create_circuit(CircuitID) :-
    uuid(CircuitID),
    assertz(circuit(CircuitID, 'New Circuit')),
    assertz(simulation_step(CircuitID, 0)).

%% add_gate(+CircuitID, +GateType, +Inputs, +Output) is det.
%
%  Adds a logic gate to the circuit.
%
%  @param CircuitID Circuit identifier
%  @param GateType Type of gate (and, or, not, nand, nor, xor, xnor)
%  @param Inputs List of input node names
%  @param Output Output node name
%
add_gate(CircuitID, GateType, Inputs, Output) :-
    uuid(GateID),
    assertz(gate(CircuitID, GateID, GateType, Inputs, Output)),
    % Initialize output signal
    assertz(signal_value(CircuitID, Output, 0)).

%% add_input(+CircuitID, +InputName, +Value) is det.
%
%  Adds an input node to the circuit.
%
add_input(CircuitID, InputName, Value) :-
    assertz(input_node(CircuitID, InputName, Value)),
    assertz(signal_value(CircuitID, InputName, Value)).

%% add_output(+CircuitID, +OutputName) is det.
%
%  Adds an output node to the circuit.
%
add_output(CircuitID, OutputName) :-
    assertz(output_node(CircuitID, OutputName)).

%% connect_nodes(+CircuitID, +From, +To) is det.
%
%  Connects two nodes in the circuit.
%
connect_nodes(CircuitID, From, To) :-
    assertz(connection(CircuitID, From, To)).

%% simulate_circuit(+CircuitID, -Results) is det.
%
%  Simulates the entire circuit and returns results.
%
simulate_circuit(CircuitID, Results) :-
    propagate_signals(CircuitID),
    collect_output_values(CircuitID, Results).

%% step_simulation(+CircuitID, -StepResults) is det.
%
%  Performs one step of simulation with detailed intermediate results.
%
step_simulation(CircuitID, StepResults) :-
    simulation_step(CircuitID, Step),
    NextStep is Step + 1,
    retract(simulation_step(CircuitID, Step)),
    assertz(simulation_step(CircuitID, NextStep)),
    
    findall(change(Node, OldValue, NewValue),
            update_single_gate(CircuitID, Node, OldValue, NewValue),
            Changes),
    
    collect_all_signal_values(CircuitID, AllValues),
    StepResults = step(NextStep, Changes, AllValues).

%% generate_truth_table(+CircuitID, -TruthTable) is det.
%
%  Generates a complete truth table for the circuit.
%
generate_truth_table(CircuitID, TruthTable) :-
    findall(InputName, input_node(CircuitID, InputName, _), InputNames),
    findall(OutputName, output_node(CircuitID, OutputName), OutputNames),
    length(InputNames, NumInputs),
    NumRows is 2^NumInputs,
    findall(row(InputValues, OutputValues),
            (between(0, NumRows-1, I),
             number_to_binary_list(I, NumInputs, InputValues),
             set_input_values(CircuitID, InputNames, InputValues),
             simulate_circuit(CircuitID, OutputValues)),
            Rows),
    TruthTable = truth_table(InputNames, OutputNames, Rows).

%% optimize_circuit(+CircuitID, -OptimizedCircuit) is det.
%
%  Optimizes the circuit by removing redundant gates and simplifying logic.
%
optimize_circuit(CircuitID, OptimizedCircuit) :-
    create_circuit(OptimizedCircuit),
    copy_inputs_outputs(CircuitID, OptimizedCircuit),
    find_redundant_gates(CircuitID, RedundantGates),
    copy_essential_gates(CircuitID, OptimizedCircuit, RedundantGates),
    simplify_gate_combinations(OptimizedCircuit).

%% export_verilog(+CircuitID, +File) is det.
%
%  Exports circuit as Verilog HDL code.
%
export_verilog(CircuitID, File) :-
    open(File, write, Stream),
    write_verilog_header(Stream, CircuitID),
    write_verilog_inputs_outputs(Stream, CircuitID),
    write_verilog_gates(Stream, CircuitID),
    write_verilog_footer(Stream),
    close(Stream).

%% export_vhdl(+CircuitID, +File) is det.
%
%  Exports circuit as VHDL code.
%
export_vhdl(CircuitID, File) :-
    open(File, write, Stream),
    write_vhdl_header(Stream, CircuitID),
    write_vhdl_entity(Stream, CircuitID),
    write_vhdl_architecture(Stream, CircuitID),
    close(Stream).

%% visualize_circuit(+CircuitID, +Format) is det.
%
%  Generates circuit visualization in various formats.
%
visualize_circuit(CircuitID, svg) :-
    generate_svg_visualization(CircuitID).

visualize_circuit(CircuitID, dot) :-
    generate_dot_graph(CircuitID).

visualize_circuit(CircuitID, ascii) :-
    generate_ascii_art(CircuitID).

%% start_web_interface(+Port) is det.
%
%  Starts the web-based circuit designer interface.
%
start_web_interface(Port) :-
    setup_web_handlers,
    http_server(http_dispatch, [port(Port)]),
    format('Digital Logic Simulator started on port ~w~n', [Port]),
    format('Visit http://localhost:~w for the circuit designer~n', [Port]).

%% validate_circuit(+CircuitID, -Issues) is det.
%
%  Validates circuit for common issues.
%
validate_circuit(CircuitID, Issues) :-
    findall(Issue, find_circuit_issue(CircuitID, Issue), Issues).

%% analyze_timing(+CircuitID, -Analysis) is det.
%
%  Analyzes timing characteristics of the circuit.
%
analyze_timing(CircuitID, Analysis) :-
    calculate_propagation_delays(CircuitID, Delays),
    find_critical_path(CircuitID, CriticalPath),
    calculate_maximum_frequency(CircuitID, MaxFreq),
    Analysis = timing_analysis(Delays, CriticalPath, MaxFreq).

%% detect_hazards(+CircuitID, -Hazards) is det.
%
%  Detects potential timing hazards in the circuit.
%
detect_hazards(CircuitID, Hazards) :-
    findall(Hazard, find_timing_hazard(CircuitID, Hazard), Hazards).

% Signal propagation engine

propagate_signals(CircuitID) :-
    repeat,
    findall(gate(CircuitID, GateID, Type, Inputs, Output),
            gate(CircuitID, GateID, Type, Inputs, Output),
            Gates),
    (propagate_gate_signals(CircuitID, Gates, Changed) ->
        (Changed = true ->
            fail ;  % Continue propagation
            true    % Stable state reached
        ) ;
        true
    ).

propagate_gate_signals(_, [], false).
propagate_gate_signals(CircuitID, [gate(CircuitID, _, Type, Inputs, Output)|Rest], Changed) :-
    get_input_values(CircuitID, Inputs, InputValues),
    evaluate_gate(Type, InputValues, NewValue),
    signal_value(CircuitID, Output, CurrentValue),
    (NewValue \= CurrentValue ->
        retract(signal_value(CircuitID, Output, CurrentValue)),
        assertz(signal_value(CircuitID, Output, NewValue)),
        Changed = true ;
        propagate_gate_signals(CircuitID, Rest, Changed)
    ).

get_input_values(_, [], []).
get_input_values(CircuitID, [Input|RestInputs], [Value|RestValues]) :-
    signal_value(CircuitID, Input, Value),
    get_input_values(CircuitID, RestInputs, RestValues).

% Logic gate evaluation

evaluate_gate(and, InputValues, Output) :-
    (member(0, InputValues) -> Output = 0 ; Output = 1).

evaluate_gate(or, InputValues, Output) :-
    (member(1, InputValues) -> Output = 1 ; Output = 0).

evaluate_gate(not, [Input], Output) :-
    (Input = 0 -> Output = 1 ; Output = 0).

evaluate_gate(nand, InputValues, Output) :-
    evaluate_gate(and, InputValues, AndResult),
    evaluate_gate(not, [AndResult], Output).

evaluate_gate(nor, InputValues, Output) :-
    evaluate_gate(or, InputValues, OrResult),
    evaluate_gate(not, [OrResult], Output).

evaluate_gate(xor, [A, B], Output) :-
    (A =:= B -> Output = 0 ; Output = 1).

evaluate_gate(xor, [A, B, C|Rest], Output) :-
    evaluate_gate(xor, [A, B], TempResult),
    evaluate_gate(xor, [TempResult, C|Rest], Output).

evaluate_gate(xnor, InputValues, Output) :-
    evaluate_gate(xor, InputValues, XorResult),
    evaluate_gate(not, [XorResult], Output).

evaluate_gate(buffer, [Input], Input).

% Truth table generation helpers

number_to_binary_list(Number, Width, BinaryList) :-
    number_to_binary_list_helper(Number, Width, [], BinaryList).

number_to_binary_list_helper(0, 0, Acc, Acc) :- !.
number_to_binary_list_helper(Number, Width, Acc, Result) :-
    Width > 0,
    Bit is Number mod 2,
    RestNumber is Number // 2,
    Width1 is Width - 1,
    number_to_binary_list_helper(RestNumber, Width1, [Bit|Acc], Result).

set_input_values(_, [], []).
set_input_values(CircuitID, [InputName|RestNames], [Value|RestValues]) :-
    retract(signal_value(CircuitID, InputName, _)),
    assertz(signal_value(CircuitID, InputName, Value)),
    retract(input_node(CircuitID, InputName, _)),
    assertz(input_node(CircuitID, InputName, Value)),
    set_input_values(CircuitID, RestNames, RestValues).

collect_output_values(CircuitID, OutputValues) :-
    findall(output(Name, Value),
            (output_node(CircuitID, Name),
             signal_value(CircuitID, Name, Value)),
            OutputValues).

collect_all_signal_values(CircuitID, AllValues) :-
    findall(signal(Node, Value),
            signal_value(CircuitID, Node, Value),
            AllValues).

% Circuit optimization

find_redundant_gates(CircuitID, RedundantGates) :-
    findall(GateID,
            (gate(CircuitID, GateID, Type, Inputs, Output),
             is_redundant_gate(CircuitID, GateID, Type, Inputs, Output)),
            RedundantGates).

is_redundant_gate(CircuitID, GateID, buffer, [Input], Output) :-
    % Buffer gates that don't affect the output
    \+ gate_affects_output(CircuitID, GateID).

is_redundant_gate(CircuitID, GateID, and, Inputs, Output) :-
    % AND gate with constant 1 input
    member(InputNode, Inputs),
    signal_value(CircuitID, InputNode, 1),
    length(Inputs, 2).

gate_affects_output(CircuitID, GateID) :-
    gate(CircuitID, GateID, _, _, Output),
    (output_node(CircuitID, Output) ;
     connection(CircuitID, Output, _)).

simplify_gate_combinations(CircuitID) :-
    % Simplify double negations
    simplify_double_negations(CircuitID),
    % Combine adjacent gates of same type
    combine_similar_gates(CircuitID).

simplify_double_negations(CircuitID) :-
    forall((gate(CircuitID, Gate1, not, [Input1], Output1),
            gate(CircuitID, Gate2, not, [Output1], Output2)),
           (retract(gate(CircuitID, Gate1, not, [Input1], Output1)),
            retract(gate(CircuitID, Gate2, not, [Output1], Output2)),
            add_gate(CircuitID, buffer, [Input1], Output2))).

combine_similar_gates(CircuitID) :-
    % Combine cascaded AND gates: (A AND B) AND C = A AND B AND C
    forall((gate(CircuitID, Gate1, and, Inputs1, Output1),
            gate(CircuitID, Gate2, and, Inputs2, Output2),
            member(Output1, Inputs2)),
           combine_and_gates(CircuitID, Gate1, Gate2, Inputs1, Inputs2, Output2)).

combine_and_gates(CircuitID, Gate1, Gate2, Inputs1, Inputs2, Output2) :-
    select(Output1, Inputs2, RestInputs2),
    gate(CircuitID, Gate1, and, Inputs1, Output1),
    append(Inputs1, RestInputs2, CombinedInputs),
    retract(gate(CircuitID, Gate1, and, Inputs1, Output1)),
    retract(gate(CircuitID, Gate2, and, Inputs2, Output2)),
    add_gate(CircuitID, and, CombinedInputs, Output2).

% HDL export implementations

write_verilog_header(Stream, CircuitID) :-
    circuit(CircuitID, Name),
    format(Stream, '// Generated Verilog for circuit: ~w~n', [Name]),
    format(Stream, '// Generated by Prolog Digital Logic Simulator~n~n', []).

write_verilog_inputs_outputs(Stream, CircuitID) :-
    findall(InputName, input_node(CircuitID, InputName, _), Inputs),
    findall(OutputName, output_node(CircuitID, OutputName), Outputs),
    format(Stream, 'module circuit(~n', []),
    write_verilog_port_list(Stream, Inputs, Outputs),
    format(Stream, ');~n~n', []),
    write_verilog_declarations(Stream, Inputs, Outputs).

write_verilog_port_list(Stream, Inputs, Outputs) :-
    append(Inputs, Outputs, AllPorts),
    write_comma_separated(Stream, AllPorts, '  ').

write_verilog_declarations(Stream, Inputs, Outputs) :-
    forall(member(Input, Inputs),
           format(Stream, '  input ~w;~n', [Input])),
    forall(member(Output, Outputs),
           format(Stream, '  output ~w;~n', [Output])),
    nl(Stream).

write_verilog_gates(Stream, CircuitID) :-
    forall(gate(CircuitID, GateID, Type, Inputs, Output),
           write_verilog_gate(Stream, GateID, Type, Inputs, Output)).

write_verilog_gate(Stream, GateID, Type, Inputs, Output) :-
    verilog_gate_name(Type, VerilogType),
    format(Stream, '  ~w ~w(~w', [VerilogType, GateID, Output]),
    forall(member(Input, Inputs),
           format(Stream, ', ~w', [Input])),
    format(Stream, ');~n', []).

write_verilog_footer(Stream) :-
    format(Stream, 'endmodule~n', []).

verilog_gate_name(and, 'and').
verilog_gate_name(or, 'or').
verilog_gate_name(not, 'not').
verilog_gate_name(nand, 'nand').
verilog_gate_name(nor, 'nor').
verilog_gate_name(xor, 'xor').
verilog_gate_name(xnor, 'xnor').

% VHDL export

write_vhdl_header(Stream, CircuitID) :-
    circuit(CircuitID, Name),
    format(Stream, '-- Generated VHDL for circuit: ~w~n', [Name]),
    format(Stream, '-- Generated by Prolog Digital Logic Simulator~n~n', []),
    format(Stream, 'library IEEE;~n', []),
    format(Stream, 'use IEEE.STD_LOGIC_1164.ALL;~n~n', []).

write_vhdl_entity(Stream, CircuitID) :-
    circuit(CircuitID, Name),
    format(Stream, 'entity ~w is~n', [Name]),
    format(Stream, '  Port (~n', []),
    write_vhdl_ports(Stream, CircuitID),
    format(Stream, '  );~n', []),
    format(Stream, 'end ~w;~n~n', [Name]).

write_vhdl_ports(Stream, CircuitID) :-
    findall(InputName, input_node(CircuitID, InputName, _), Inputs),
    findall(OutputName, output_node(CircuitID, OutputName), Outputs),
    write_vhdl_input_ports(Stream, Inputs),
    write_vhdl_output_ports(Stream, Outputs).

write_vhdl_input_ports(Stream, Inputs) :-
    forall(member(Input, Inputs),
           format(Stream, '    ~w : in STD_LOGIC;~n', [Input])).

write_vhdl_output_ports(Stream, Outputs) :-
    last(Outputs, LastOutput),
    forall(member(Output, Outputs),
           (Output = LastOutput ->
               format(Stream, '    ~w : out STD_LOGIC~n', [Output]) ;
               format(Stream, '    ~w : out STD_LOGIC;~n', [Output]))).

write_vhdl_architecture(Stream, CircuitID) :-
    circuit(CircuitID, Name),
    format(Stream, 'architecture Behavioral of ~w is~n', [Name]),
    write_vhdl_signals(Stream, CircuitID),
    format(Stream, 'begin~n', []),
    write_vhdl_processes(Stream, CircuitID),
    format(Stream, 'end Behavioral;~n', []).

write_vhdl_signals(Stream, CircuitID) :-
    findall(Node, (gate(CircuitID, _, _, _, Node)), InternalNodes),
    forall(member(Node, InternalNodes),
           format(Stream, '  signal ~w : STD_LOGIC;~n', [Node])),
    nl(Stream).

write_vhdl_processes(Stream, CircuitID) :-
    forall(gate(CircuitID, GateID, Type, Inputs, Output),
           write_vhdl_gate_assignment(Stream, GateID, Type, Inputs, Output)).

write_vhdl_gate_assignment(Stream, GateID, Type, Inputs, Output) :-
    vhdl_gate_operator(Type, Operator),
    format(Stream, '  ~w <= ', [Output]),
    write_vhdl_expression(Stream, Operator, Inputs),
    format(Stream, '; -- ~w~n', [GateID]).

write_vhdl_expression(Stream, not, [Input]) :-
    format(Stream, 'not ~w', [Input]).

write_vhdl_expression(Stream, Operator, [First|Rest]) :-
    write(Stream, First),
    forall(member(Input, Rest),
           format(Stream, ' ~w ~w', [Operator, Input])).

vhdl_gate_operator(and, 'and').
vhdl_gate_operator(or, 'or').
vhdl_gate_operator(not, 'not').
vhdl_gate_operator(nand, 'nand').
vhdl_gate_operator(nor, 'nor').
vhdl_gate_operator(xor, 'xor').
vhdl_gate_operator(xnor, 'xnor').

% Web interface handlers

setup_web_handlers :-
    http_handler('/api/create_circuit', handle_create_circuit, [method(post)]),
    http_handler('/api/add_gate', handle_add_gate, [method(post)]),
    http_handler('/api/simulate', handle_simulate, [method(post)]),
    http_handler('/api/truth_table', handle_truth_table, [method(post)]),
    http_handler('/api/export', handle_export, [method(post)]),
    http_handler('/', serve_web_files, [prefix]).

handle_create_circuit(Request) :-
    create_circuit(CircuitID),
    reply_json_dict(_{circuit_id: CircuitID}).

handle_add_gate(Request) :-
    http_read_json_dict(Request, Data),
    CircuitID = Data.circuit_id,
    GateType = Data.gate_type,
    Inputs = Data.inputs,
    Output = Data.output,
    add_gate(CircuitID, GateType, Inputs, Output),
    reply_json_dict(_{success: true}).

handle_simulate(Request) :-
    http_read_json_dict(Request, Data),
    CircuitID = Data.circuit_id,
    simulate_circuit(CircuitID, Results),
    reply_json_dict(_{results: Results}).

handle_truth_table(Request) :-
    http_read_json_dict(Request, Data),
    CircuitID = Data.circuit_id,
    generate_truth_table(CircuitID, TruthTable),
    reply_json_dict(_{truth_table: TruthTable}).

handle_export(Request) :-
    http_read_json_dict(Request, Data),
    CircuitID = Data.circuit_id,
    Format = Data.format,
    TempFile = '/tmp/circuit_export',
    (Format = verilog ->
        export_verilog(CircuitID, TempFile) ;
        export_vhdl(CircuitID, TempFile)),
    read_file_to_string(TempFile, Content, []),
    reply_json_dict(_{content: Content}).

serve_web_files(Request) :-
    http_reply_from_files('web', [], Request).

% Circuit validation

find_circuit_issue(CircuitID, Issue) :-
    % Unconnected inputs
    gate(CircuitID, GateID, _, Inputs, _),
    member(Input, Inputs),
    \+ signal_value(CircuitID, Input, _),
    Issue = unconnected_input(GateID, Input).

find_circuit_issue(CircuitID, Issue) :-
    % Floating outputs
    output_node(CircuitID, Output),
    \+ gate(CircuitID, _, _, _, Output),
    \+ input_node(CircuitID, Output, _),
    Issue = floating_output(Output).

find_circuit_issue(CircuitID, Issue) :-
    % Combinational loops
    has_combinational_loop(CircuitID, Path),
    Issue = combinational_loop(Path).

% Timing analysis

calculate_propagation_delays(CircuitID, Delays) :-
    findall(delay(GateID, Delay),
            (gate(CircuitID, GateID, Type, _, _),
             gate_delay(Type, Delay)),
            Delays).

gate_delay(and, 2).
gate_delay(or, 2).
gate_delay(not, 1).
gate_delay(nand, 2).
gate_delay(nor, 2).
gate_delay(xor, 3).
gate_delay(xnor, 3).

find_critical_path(CircuitID, CriticalPath) :-
    findall(path(Path, TotalDelay),
            (input_node(CircuitID, Input, _),
             output_node(CircuitID, Output),
             find_path(CircuitID, Input, Output, Path),
             calculate_path_delay(CircuitID, Path, TotalDelay)),
            AllPaths),
    max_member(path(CriticalPath, _), AllPaths).

find_path(CircuitID, Start, End, Path) :-
    find_path_helper(CircuitID, Start, End, [Start], Path).

find_path_helper(_, End, End, Acc, Path) :-
    reverse(Acc, Path).

find_path_helper(CircuitID, Current, End, Acc, Path) :-
    (gate(CircuitID, _, _, Inputs, Current) ->
        member(Next, Inputs) ;
        connection(CircuitID, Current, Next)),
    \+ member(Next, Acc),
    find_path_helper(CircuitID, Next, End, [Next|Acc], Path).

calculate_path_delay(CircuitID, Path, TotalDelay) :-
    findall(Delay,
            (member(Node, Path),
             gate(CircuitID, _, Type, _, Node),
             gate_delay(Type, Delay)),
            Delays),
    sum_list(Delays, TotalDelay).

% Hazard detection

find_timing_hazard(CircuitID, Hazard) :-
    % Static-1 hazard in AND-OR structure
    gate(CircuitID, AndGate, and, AndInputs, AndOutput),
    gate(CircuitID, OrGate, or, OrInputs, _),
    member(AndOutput, OrInputs),
    has_static_hazard_condition(CircuitID, AndInputs, OrInputs),
    Hazard = static_1_hazard(AndGate, OrGate).

has_static_hazard_condition(CircuitID, AndInputs, OrInputs) :-
    % Simplified hazard detection - check for potential race conditions
    member(Input, AndInputs),
    member(Input, OrInputs).

% Utility predicates

write_comma_separated(_, [], _).
write_comma_separated(Stream, [Item], Indent) :-
    format(Stream, '~w~w', [Indent, Item]).
write_comma_separated(Stream, [Item|Rest], Indent) :-
    Rest \= [],
    format(Stream, '~w~w,~n', [Indent, Item]),
    write_comma_separated(Stream, Rest, Indent).

has_combinational_loop(CircuitID, Path) :-
    gate(CircuitID, StartGate, _, _, StartOutput),
    find_loop_path(CircuitID, StartOutput, StartOutput, [StartOutput], Path).

find_loop_path(_, Target, Target, Acc, Path) :-
    length(Acc, Len),
    Len > 1,
    reverse(Acc, Path).

find_loop_path(CircuitID, Current, Target, Acc, Path) :-
    gate(CircuitID, _, _, Inputs, Current),
    member(Next, Inputs),
    \+ member(Next, Acc),
    find_loop_path(CircuitID, Next, Target, [Next|Acc], Path).

copy_inputs_outputs(FromCircuit, ToCircuit) :-
    forall(input_node(FromCircuit, Name, Value),
           add_input(ToCircuit, Name, Value)),
    forall(output_node(FromCircuit, Name),
           add_output(ToCircuit, Name)).

copy_essential_gates(FromCircuit, ToCircuit, RedundantGates) :-
    forall((gate(FromCircuit, GateID, Type, Inputs, Output),
            \+ member(GateID, RedundantGates)),
           add_gate(ToCircuit, Type, Inputs, Output)).

update_single_gate(CircuitID, Node, OldValue, NewValue) :-
    gate(CircuitID, _, Type, Inputs, Node),
    get_input_values(CircuitID, Inputs, InputValues),
    evaluate_gate(Type, InputValues, NewValue),
    signal_value(CircuitID, Node, OldValue),
    NewValue \= OldValue,
    retract(signal_value(CircuitID, Node, OldValue)),
    assertz(signal_value(CircuitID, Node, NewValue)).

% File I/O

load_circuit(File, CircuitID) :-
    open(File, read, Stream),
    read_term(Stream, circuit_data(CircuitID, Gates, Inputs, Outputs, Connections), []),
    close(Stream),
    assertz(circuit(CircuitID, 'Loaded Circuit')),
    load_circuit_components(CircuitID, Gates, Inputs, Outputs, Connections).

save_circuit(CircuitID, File) :-
    collect_circuit_data(CircuitID, Gates, Inputs, Outputs, Connections),
    open(File, write, Stream),
    write_canonical(Stream, circuit_data(CircuitID, Gates, Inputs, Outputs, Connections)),
    write(Stream, '.'),
    close(Stream).

collect_circuit_data(CircuitID, Gates, Inputs, Outputs, Connections) :-
    findall(gate(GateID, Type, GateInputs, Output),
            gate(CircuitID, GateID, Type, GateInputs, Output),
            Gates),
    findall(input(Name, Value),
            input_node(CircuitID, Name, Value),
            Inputs),
    findall(output(Name),
            output_node(CircuitID, Name),
            Outputs),
    findall(connection(From, To),
            connection(CircuitID, From, To),
            Connections).

load_circuit_components(CircuitID, Gates, Inputs, Outputs, Connections) :-
    forall(member(gate(_, Type, GateInputs, Output), Gates),
           add_gate(CircuitID, Type, GateInputs, Output)),
    forall(member(input(Name, Value), Inputs),
           add_input(CircuitID, Name, Value)),
    forall(member(output(Name), Outputs),
           add_output(CircuitID, Name)),
    forall(member(connection(From, To), Connections),
           connect_nodes(CircuitID, From, To)).

% ASCII visualization

generate_ascii_art(CircuitID) :-
    findall(gate(Type, Inputs, Output),
            gate(CircuitID, _, Type, Inputs, Output),
            Gates),
    draw_ascii_circuit(Gates).

draw_ascii_circuit(Gates) :-
    format('~n=== Circuit Diagram ===~n'),
    forall(member(gate(Type, Inputs, Output), Gates),
           draw_ascii_gate(Type, Inputs, Output)),
    format('======================~n~n').

draw_ascii_gate(Type, Inputs, Output) :-
    format('~w: ~w -> [~w] -> ~w~n', [Type, Inputs, Type, Output]).

% SVG visualization

generate_svg_visualization(CircuitID) :-
    format('Generating SVG visualization for circuit ~w~n', [CircuitID]),
    % Placeholder for SVG generation
    true.

generate_dot_graph(CircuitID) :-
    format('Generating DOT graph for circuit ~w~n', [CircuitID]),
    % Placeholder for DOT graph generation
    true.
