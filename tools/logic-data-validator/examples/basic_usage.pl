% Basic usage example for Logic-Based Data Validator

:- use_module('../src/data_validator').

% Example 1: Basic validation
example_basic_validation :-
    write('=== Basic Validation Example ==='), nl,
    
    % Sample data with some violations
    Data = [
        person(1, 'John', 25),
        person(2, 'Jane', -5),    % Invalid age
        person(3, 'Bob', 200)     % Invalid age
    ],
    
    % Define validation rules
    Rules = [
        rule(age_positive, forall(data_fact(person(_, _, Age)), Age > 0)),
        rule(age_reasonable, forall(data_fact(person(_, _, Age)), Age < 150))
    ],
    
    % Validate data
    validate_data(Data, Rules, Violations),
    
    % Display results
    length(Violations, VCount),
    format('Found ~w violations:~n', [VCount]),
    forall(member(V, Violations), format('  ~w~n', [V])).

% Example 2: File-based validation
example_file_validation :-
    write('=== File-based Validation Example ==='), nl,
    validate_file('examples/sample_data.pl', 
                  'examples/custom_rules.pl', 
                  Violations),
    generate_report(Violations, text).

% Example 3: Cross-field validation
example_cross_field :-
    write('=== Cross-field Validation Example ==='), nl,
    
    Data = [
        employee(1, 'John', 50000, date(2020,1,1)),
        employee(2, 'Jane', 60000, date(2019,6,15)),
        employee(3, 'Bob', 40000, date(2025,1,1))  % Future hire date
    ],
    
    Rules = [
        rule(salary_positive, 
             forall(data_fact(employee(_, _, Salary, _)), Salary > 0)),
        rule(hire_date_past,
             forall(data_fact(employee(_, _, _, HireDate)),
                    date_before_today(HireDate)))
    ],
    
    validate_data(Data, Rules, Violations),
    generate_report(Violations, text).

% Example 4: Dynamic rule management
example_dynamic_rules :-
    write('=== Dynamic Rule Management Example ==='), nl,
    
    % Add rules dynamically
    add_rule(min_salary, 
             forall(data_fact(employee(_, _, Salary, _)), Salary >= 30000)),
    add_rule(max_salary,
             forall(data_fact(employee(_, _, Salary, _)), Salary =< 200000)),
    
    % List current rules
    write('Current rules:'), nl,
    list_rules,
    
    % Validate with dynamic rules
    Data = [employee(1, 'John', 25000)],  % Below minimum
    get_rules(Rules),
    validate_data(Data, Rules, Violations),
    
    format('Violations with dynamic rules: ~w~n', [Violations]),
    
    % Clean up
    remove_rule(min_salary),
    remove_rule(max_salary).

% Helper predicate
date_before_today(Date) :-
    get_time(Now),
    format_time(atom(Today), '%Y-%m-%d', Now),
    atom_codes(Today, _TodayCodes),
    % Simplified date comparison for example
    Date @< date(2024,12,31).

% Run all examples
run_examples :-
    example_basic_validation, nl,
    example_cross_field, nl,
    example_dynamic_rules, nl,
    write('=== All examples completed ==='), nl.
