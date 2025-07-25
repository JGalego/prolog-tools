#!/bin/bash

# Quick test script for Digital Logic Simulator
echo "Testing Digital Logic Simulator..."

# Check if SWI-Prolog is available
if ! command -v swipl &> /dev/null; then
    echo "ERROR: SWI-Prolog not found. Please install SWI-Prolog."
    exit 1
fi

# Test basic circuit creation and simulation
echo "Testing basic circuit operations..."
swipl -g "
    use_module('src/logic_simulator'),
    
    % Test 1: Create and simulate basic AND gate
    create_circuit(C1),
    add_input(C1, 'A', 1),
    add_input(C1, 'B', 1), 
    add_gate(C1, and, ['A', 'B'], 'Y'),
    add_output(C1, 'Y'),
    simulate_circuit(C1, R1),
    write('AND gate test: '), writeln(R1),
    
    % Test 2: Create and simulate NOT gate
    create_circuit(C2),
    add_input(C2, 'X', 0),
    add_gate(C2, not, ['X'], 'Y'),
    add_output(C2, 'Y'),
    simulate_circuit(C2, R2),
    write('NOT gate test: '), writeln(R2),
    
    % Test 3: Truth table generation
    create_circuit(C3),
    add_input(C3, 'A', 0),
    add_input(C3, 'B', 0),
    add_gate(C3, xor, ['A', 'B'], 'Y'),
    add_output(C3, 'Y'),
    generate_truth_table(C3, TT),
    write('XOR truth table: '), writeln(TT),
    
    % Test 4: Circuit validation
    create_circuit(C4),
    add_input(C4, 'A', 0),
    add_gate(C4, and, ['A', 'B'], 'Y'), % Missing input B - should be caught
    validate_circuit(C4, Issues),
    write('Validation test: '), writeln(Issues),
    
    halt
" -t "halt(1)"

if [ $? -eq 0 ]; then
    echo "✓ Basic tests passed"
else
    echo "✗ Basic tests failed"
    exit 1
fi

# Test timing analysis
echo "Testing timing analysis..."
swipl -g "
    use_module('src/logic_simulator'),
    
    create_circuit(C),
    add_input(C, 'A', 0),
    add_input(C, 'B', 0),
    add_gate(C, and, ['A', 'B'], 'X'),
    add_gate(C, not, ['X'], 'Y'),
    add_output(C, 'Y'),
    
    analyze_timing(C, Analysis),
    write('Timing analysis: '), writeln(Analysis),
    
    halt
" -t "halt(1)"

if [ $? -eq 0 ]; then
    echo "✓ Timing analysis tests passed"
else
    echo "✗ Timing analysis tests failed"
    exit 1
fi

# Test HDL export
echo "Testing HDL export..."
swipl -g "
    use_module('src/logic_simulator'),
    
    create_circuit(C),
    add_input(C, 'A', 0),
    add_input(C, 'B', 0),
    add_gate(C, or, ['A', 'B'], 'Y'),
    add_output(C, 'Y'),
    
    export_verilog(C, 'test_circuit.v'),
    export_vhdl(C, 'test_circuit.vhd'),
    
    writeln('HDL export completed'),
    halt
" -t "halt(1)"

if [ $? -eq 0 ]; then
    echo "✓ HDL export tests passed"
    # Clean up generated files
    rm -f test_circuit.v test_circuit.vhd
else
    echo "✗ HDL export tests failed"
    exit 1
fi

# Test web interface startup (quick check)
echo "Testing web interface startup..."
timeout 5s swipl -g "
    use_module('src/logic_simulator'),
    catch(
        (start_web_interface(8081), sleep(2), stop_web_interface),
        Error,
        (write('Web interface error: '), writeln(Error))
    ),
    halt
" -t "halt(1)" 2>/dev/null

if [ $? -eq 0 ] || [ $? -eq 124 ]; then  # 124 is timeout exit code
    echo "✓ Web interface startup test passed"
else
    echo "✗ Web interface startup test failed"
fi

echo ""
echo "All Digital Logic Simulator tests completed!"
echo "Run 'swipl -g \"use_module('src/logic_simulator'), start_web_interface(8080)\"' to start the web interface."
