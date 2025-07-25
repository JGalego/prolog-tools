# Logic-Based Data Validator

A Prolog-based framework for defining and applying validation rules to check for data inconsistencies, violations, or anomalies.

## Features

- **Flexible Rule Definition**: Define custom validation rules using Prolog syntax
- **Multiple Data Sources**: Validate data from files, databases, or in-memory structures
- **Built-in Validation Types**: Range, type, uniqueness, required fields, cross-field validation
- **Dynamic Rule Management**: Add, remove, and modify rules at runtime
- **Multiple Report Formats**: Generate reports in text, JSON, or CSV format
- **Batch Processing**: Validate multiple datasets efficiently
- **Extensible Architecture**: Easy to add custom validation logic

## Quick Start

### Basic Usage

```prolog
% Load the data validator
?- use_module(library(data_validator)).

% Define some sample data with violations
?- Data = [
    person(1, 'John', 25),
    person(2, 'Jane', -5),    % Invalid age
    person(3, 'Bob', 200)     % Invalid age
].

% Define validation rules
?- Rules = [
    rule(age_positive, forall(data_fact(person(_, _, Age)), Age > 0)),
    rule(age_reasonable, forall(data_fact(person(_, _, Age)), Age < 150))
].

% Validate the data
?- validate_data(Data, Rules, Violations).
Violations = [violation(age_positive, person(2, 'Jane', -5), 'Rule age_positive violated by fact person(2, 'Jane', -5)'),
              violation(age_reasonable, person(3, 'Bob', 200), 'Rule age_reasonable violated by fact person(3, 'Bob', 200)')].

% Generate a report
?- generate_report(Violations, text).
=== DATA VALIDATION REPORT ===
Total violations: 2

VIOLATION: age_positive
  Fact: person(2, 'Jane', -5)
  Reason: Rule age_positive violated by fact person(2, 'Jane', -5)

VIOLATION: age_reasonable
  Fact: person(3, 'Bob', 200)
  Reason: Rule age_reasonable violated by fact person(3, 'Bob', 200)
```

### File-based Validation

```prolog
% Validate data from files
?- validate_file('examples/sample_data.pl', 'examples/custom_rules.pl', Violations).

% Load rules from a file
?- load_rules('examples/custom_rules.pl').
```

### Dynamic Rule Management

```prolog
% Add a new rule at runtime
?- add_rule(salary_check, forall(data_fact(employee(_, _, Salary)), Salary > 0)).

% List all current rules
?- list_rules.

% Remove a rule
?- remove_rule(salary_check).
```

## Built-in Validation Types

### Range Validation
```prolog
% Validate that field 3 (age) is between 18 and 65
validate_range(3, 18, 65)
```

### Type Validation
```prolog
% Validate that field 2 (name) is an atom
validate_type(2, atom)
```

### Uniqueness Validation
```prolog
% Validate that field 1 (ID) values are unique
validate_unique(1)
```

### Required Fields
```prolog
% Validate that field 2 is not empty
validate_required(2)
```

### Cross-field Dependencies
```prolog
% Validate that start_date < end_date
validate_dependency(3, 4, @<)
```

## API Reference

### Core Predicates

- `validate_data(+Data, +Rules, -Violations)` - Validate data against rules
- `validate_file(+DataFile, +RuleFile, -Violations)` - Validate from files
- `validate_fact(+Fact, +Rules)` - Validate a single fact
- `batch_validate(+DataList, +Rules, -AllViolations)` - Batch validation

### Rule Management

- `add_rule(+RuleName, +Rule)` - Add a validation rule
- `remove_rule(+RuleName)` - Remove a validation rule
- `list_rules` - List all current rules
- `load_rules(+RuleFile)` - Load rules from file

### Reporting

- `generate_report(+Violations, +Format)` - Generate validation report
  - Formats: `text`, `json`, `csv`

## Examples

See the `examples/` directory for comprehensive usage examples:

- `basic_usage.pl` - Basic validation scenarios
- `sample_data.pl` - Sample data with various violations
- `custom_rules.pl` - Custom business rule definitions

## Testing

Run the test suite:

```bash
cd tools/logic-data-validator
./quick_test.sh
```

## Rule Definition Guide

### Simple Rules
```prolog
% Age must be positive
rule(positive_age, forall(data_fact(person(_, _, Age)), Age > 0))
```

### Complex Business Rules
```prolog
% Manager salary must be higher than employee salary
rule(manager_salary,
     forall((data_fact(employee(EmpId, _, _, EmpSalary)),
             data_fact(manager(_, EmpId, _, MgrSalary))),
            MgrSalary > EmpSalary))
```

### Cross-table Validation
```prolog
% Referential integrity
rule(order_customer_exists,
     forall(data_fact(order(_, CustomerId, _)),
            data_fact(customer(CustomerId, _, _))))
```

## License

MIT License - see LICENSE file for details.
