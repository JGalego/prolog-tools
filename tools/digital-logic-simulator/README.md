# Digital Logic Simulator

An interactive web-based tool for designing and simulating digital circuits with Prolog backend for logic evaluation and signal propagation.

## Features

- **Drag-and-Drop Circuit Designer**: Visual circuit design interface
- **Real-time Simulation**: Immediate feedback on circuit behavior
- **Step-by-step Debugging**: Trace signal propagation through the circuit
- **Truth Table Generation**: Automatic truth table generation for any circuit
- **Circuit Optimization**: Remove redundant gates and simplify logic
- **HDL Export**: Export to Verilog and VHDL formats
- **Timing Analysis**: Calculate propagation delays and critical paths
- **Hazard Detection**: Identify potential timing hazards
- **Multiple Gate Types**: AND, OR, NOT, NAND, NOR, XOR, XNOR
- **Validation**: Circuit validation and error detection

## Quick Start

### Start the Web Interface

```prolog
% Load the logic simulator
?- use_module(library(logic_simulator)).

% Start the web interface on port 8080
?- start_web_interface(8080).
Digital Logic Simulator started on port 8080
Visit http://localhost:8080 for the circuit designer
```

### Programmatic Circuit Design

```prolog
% Create a new circuit
?- create_circuit(CircuitID).

% Add input nodes
?- add_input(CircuitID, 'A', 0),
   add_input(CircuitID, 'B', 1).

% Add logic gates
?- add_gate(CircuitID, and, ['A', 'B'], 'C'),
   add_gate(CircuitID, not, ['C'], 'Y').

% Add output node
?- add_output(CircuitID, 'Y').

% Simulate the circuit
?- simulate_circuit(CircuitID, Results).
Results = [output('Y', 0)].

% Generate truth table
?- generate_truth_table(CircuitID, TruthTable).
```

## Web Interface

The web interface provides:

### Circuit Designer
- **Gate Palette**: Drag gates from the sidebar onto the canvas
- **Visual Editor**: Click and drag to position components
- **Connection Tool**: Connect gates with wires
- **Properties Panel**: View and edit component properties

### Simulation Controls
- **Input Toggles**: Interactive switches for circuit inputs
- **Output LEDs**: Visual indicators for circuit outputs  
- **Step Simulation**: Debug circuits step by step
- **Truth Table Viewer**: Complete truth table generation

### Export Options
- **Verilog Export**: Generate synthesizable Verilog code
- **VHDL Export**: Generate VHDL descriptions
- **JSON Export**: Save circuit configuration

## Programmatic API

### Circuit Management

```prolog
% Create and manage circuits
create_circuit(CircuitID)
save_circuit(CircuitID, 'my_circuit.pl')
load_circuit('my_circuit.pl', CircuitID)
```

### Component Addition

```prolog
% Add various components
add_gate(CircuitID, and, ['A', 'B'], 'C')
add_gate(CircuitID, or, ['X', 'Y'], 'Z')
add_gate(CircuitID, not, ['Input'], 'Output')
add_input(CircuitID, 'A', 0)
add_output(CircuitID, 'Result')
```

### Analysis Tools

```prolog
% Circuit analysis
validate_circuit(CircuitID, Issues)
analyze_timing(CircuitID, Analysis)
detect_hazards(CircuitID, Hazards)
optimize_circuit(CircuitID, OptimizedCircuit)
```

## Example: Full Adder Circuit

```prolog
% Create a full adder circuit
create_full_adder(CircuitID) :-
    create_circuit(CircuitID),
    
    % Inputs
    add_input(CircuitID, 'A', 0),
    add_input(CircuitID, 'B', 0),
    add_input(CircuitID, 'Cin', 0),
    
    % Sum logic: A XOR B XOR Cin
    add_gate(CircuitID, xor, ['A', 'B'], 'AB_XOR'),
    add_gate(CircuitID, xor, ['AB_XOR', 'Cin'], 'Sum'),
    
    % Carry logic: (A AND B) OR (Cin AND (A XOR B))
    add_gate(CircuitID, and, ['A', 'B'], 'AB_AND'),
    add_gate(CircuitID, and, ['AB_XOR', 'Cin'], 'XOR_CIN_AND'),
    add_gate(CircuitID, or, ['AB_AND', 'XOR_CIN_AND'], 'Cout'),
    
    % Outputs
    add_output(CircuitID, 'Sum'),
    add_output(CircuitID, 'Cout').

% Test the full adder
?- create_full_adder(FA),
   generate_truth_table(FA, TruthTable).
```

## Advanced Features

### Circuit Optimization

```prolog
% Optimize circuit for area and delay
?- optimize_circuit(CircuitID, OptimizedID).

% The optimizer removes:
% - Redundant gates (e.g., double negations)
% - Unused connections
% - Combinable gates of same type
```

### Timing Analysis

```prolog
% Analyze circuit timing
?- analyze_timing(CircuitID, Analysis).
Analysis = timing_analysis(
    [delay(gate1, 2), delay(gate2, 3)],  % Individual gate delays
    critical_path([input_a, gate1, gate2, output_y]),  % Critical path
    max_frequency(125000000)  % Maximum frequency in Hz
).
```

### Hazard Detection

```prolog
% Detect timing hazards
?- detect_hazards(CircuitID, Hazards).
Hazards = [
    static_1_hazard(gate_and_1, gate_or_2),
    race_condition(signal_x, signal_y)
].
```

### HDL Export

```prolog
% Export to Verilog
?- export_verilog(CircuitID, 'circuit.v').

% Export to VHDL  
?- export_vhdl(CircuitID, 'circuit.vhd').
```

## Circuit Validation

The simulator validates circuits for common issues:

- **Unconnected inputs**: Gates with floating inputs
- **Floating outputs**: Outputs not driven by any gate
- **Combinational loops**: Feedback paths without storage elements
- **Multiple drivers**: Signals driven by multiple sources

```prolog
?- validate_circuit(CircuitID, Issues).
Issues = [
    unconnected_input(gate_and_1, input_b),
    floating_output(output_z),
    combinational_loop([gate1, gate2, gate3])
].
```

## Supported Logic Gates

| Gate | Inputs | Function |
|------|--------|----------|
| AND | 2+ | Output = 1 if all inputs = 1 |
| OR | 2+ | Output = 1 if any input = 1 |
| NOT | 1 | Output = opposite of input |
| NAND | 2+ | Output = NOT(AND(inputs)) |
| NOR | 2+ | Output = NOT(OR(inputs)) |
| XOR | 2+ | Output = 1 if odd number of inputs = 1 |
| XNOR | 2+ | Output = NOT(XOR(inputs)) |
| BUFFER | 1 | Output = input (with delay) |

## Testing

Run the test suite:

```bash
cd tools/digital-logic-simulator
./quick_test.sh
```

## Examples

See the `examples/` directory for:
- Basic gate demonstrations
- Complex circuit designs (adders, multiplexers, etc.)
- Timing analysis examples
- Optimization demonstrations

## License

MIT License - see LICENSE file for details.
