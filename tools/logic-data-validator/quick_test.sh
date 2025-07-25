#!/bin/bash

# Quick test script for Logic-Based Data Validator

echo "=== Testing Logic-Based Data Validator ==="

# Check if SWI-Prolog is installed
if ! command -v swipl &> /dev/null; then
    echo "Error: SWI-Prolog is not installed. Please install it first:"
    echo "sudo apt-get install swi-prolog"
    exit 1
fi

echo "Running basic validation tests..."

# Run basic tests
swipl -g "
    use_module(src/data_validator),
    
    % Test 1: Basic validation
    write('Test 1: Basic validation'), nl,
    Data1 = [person(1, 'John', 25), person(2, 'Jane', -5)],
    Rules1 = [rule(age_positive, forall(data_fact(person(_, _, Age)), Age > 0))],
    validate_data(Data1, Rules1, V1),
    format('Violations found: ~w~n', [length(V1)]),
    
    % Test 2: Multiple rules
    write('Test 2: Multiple rules'), nl,
    Rules2 = [
        rule(age_positive, forall(data_fact(person(_, _, Age)), Age > 0)),
        rule(age_reasonable, forall(data_fact(person(_, _, Age)), Age < 150))
    ],
    validate_data(Data1, Rules2, V2),
    format('Violations found: ~w~n', [length(V2)]),
    
    % Test 3: Dynamic rules
    write('Test 3: Dynamic rules'), nl,
    add_rule(test_rule, forall(data_fact(person(_, Name, _)), atom(Name))),
    list_rules,
    remove_rule(test_rule),
    
    write('All tests completed successfully!'), nl,
    halt
" -t halt

echo "Testing completed!"

# Run example if it exists
if [ -f "examples/basic_usage.pl" ]; then
    echo "Running examples..."
    swipl -g "consult('examples/basic_usage'), run_examples, halt" -t halt
fi

echo "=== Logic-Based Data Validator tests completed ==="
