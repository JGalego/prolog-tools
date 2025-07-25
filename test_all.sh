#!/bin/bash

# Quick Test Script for Prolog Tools Collection
# This script tests all available tools in the collection

set -e  # Exit on any error

echo "=== Prolog Tools Collection Test Suite ==="
echo

# Check for SWI-Prolog
if ! command -v swipl &> /dev/null; then
    echo "Error: SWI-Prolog not found. Please install:"
    echo "  sudo apt-get install swi-prolog"
    exit 1
fi

echo "✓ SWI-Prolog found"
echo

# Test each tool
TOOL_DIRS=(tools/*)
PASSED_TOOLS=()
FAILED_TOOLS=()

for tool_dir in "${TOOL_DIRS[@]}"; do
    if [ -d "$tool_dir" ]; then
        tool_name=$(basename "$tool_dir")
        echo "=== Testing $tool_name ==="
        
        if [ -f "$tool_dir/quick_test.sh" ]; then
            cd "$tool_dir"
            if ./quick_test.sh; then
                echo "✓ $tool_name: All tests passed"
                PASSED_TOOLS+=("$tool_name")
            else
                echo "✗ $tool_name: Tests failed"
                FAILED_TOOLS+=("$tool_name")
            fi
            cd - > /dev/null
        else
            echo "⚠ $tool_name: No test script found (quick_test.sh missing)"
        fi
        echo
    fi
done

# Summary
echo "=== Test Summary ==="
echo "Passed: ${#PASSED_TOOLS[@]} tools"
for tool in "${PASSED_TOOLS[@]}"; do
    echo "  ✓ $tool"
done

if [ ${#FAILED_TOOLS[@]} -gt 0 ]; then
    echo "Failed: ${#FAILED_TOOLS[@]} tools"
    for tool in "${FAILED_TOOLS[@]}"; do
        echo "  ✗ $tool"
    done
    exit 1
else
    echo
    echo "🎉 All tools passed their tests!"
    echo
    echo "Available tools:"
    for tool in "${PASSED_TOOLS[@]}"; do
        echo "  📁 tools/$tool/"
    done
    echo
    echo "Each tool has its own README.md with specific documentation."
fi
