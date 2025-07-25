/**
 * Query Translation Tests
 * 
 * Test suite for Prolog-to-SQL query translation functionality.
 */

:- use_module('../src/query_translator').
:- use_module('../src/type_mapper').
:- use_module(library(plunit)).

:- begin_tests(query_translation).

% Sample table info for testing
sample_table_info(table_info(['id', 'name', 'department', 'salary'], 
                            ['INTEGER', 'VARCHAR', 'VARCHAR', 'DECIMAL'])).

% Test basic query translation
test(basic_translation) :-
    sample_table_info(TableInfo),
    PrologQuery = employee(_, 'John', _, _),
    query_translate_prolog_to_sql(PrologQuery, TableInfo, SQLQuery),
    assertion(sub_string(SQLQuery, _, _, _, 'SELECT')),
    assertion(sub_string(SQLQuery, _, _, _, 'FROM employee')),
    assertion(sub_string(SQLQuery, _, _, _, "name = 'John'")).

% Test query with multiple constants
test(multiple_constants) :-
    sample_table_info(TableInfo),
    PrologQuery = employee(_, 'Alice', 'Engineering', _),
    query_translate_prolog_to_sql(PrologQuery, TableInfo, SQLQuery),
    assertion(sub_string(SQLQuery, _, _, _, "name = 'Alice'")),
    assertion(sub_string(SQLQuery, _, _, _, "department = 'Engineering'")).

% Test query with numeric constant
test(numeric_constant) :-
    sample_table_info(TableInfo),
    PrologQuery = employee(1, _, _, _),
    query_translate_prolog_to_sql(PrologQuery, TableInfo, SQLQuery),
    assertion(sub_string(SQLQuery, _, _, _, 'id = 1')).

% Test query with all variables (no WHERE clause)
test(all_variables) :-
    sample_table_info(TableInfo),
    PrologQuery = employee(_, _, _, _),
    query_translate_prolog_to_sql(PrologQuery, TableInfo, SQLQuery),
    assertion(sub_string(SQLQuery, _, _, _, 'SELECT * FROM employee')),
    assertion(\+ sub_string(SQLQuery, _, _, _, 'WHERE')).

% Test constraint expression translation
test(constraint_translation) :-
    sample_table_info(TableInfo),
    PrologQuery = employee(_, _, _, >(50000)),
    query_translate_prolog_to_sql(PrologQuery, TableInfo, SQLQuery),
    assertion(sub_string(SQLQuery, _, _, _, 'salary > 50000')).

% Test different constraint operators
test(constraint_operators) :-
    sample_table_info(TableInfo),
    
    % Test greater than
    query_translate_prolog_to_sql(employee(_, _, _, >(1000)), TableInfo, SQL1),
    assertion(sub_string(SQL1, _, _, _, 'salary > 1000')),
    
    % Test less than
    query_translate_prolog_to_sql(employee(_, _, _, <(2000)), TableInfo, SQL2),
    assertion(sub_string(SQL2, _, _, _, 'salary < 2000')),
    
    % Test greater than or equal
    query_translate_prolog_to_sql(employee(_, _, _, >=(1500)), TableInfo, SQL3),
    assertion(sub_string(SQL3, _, _, _, 'salary >= 1500')),
    
    % Test less than or equal
    query_translate_prolog_to_sql(employee(_, _, _, =<(3000)), TableInfo, SQL4),
    assertion(sub_string(SQL4, _, _, _, 'salary <= 3000')),
    
    % Test equality
    query_translate_prolog_to_sql(employee(_, _, _, =:=(2500)), TableInfo, SQL5),
    assertion(sub_string(SQL5, _, _, _, 'salary = 2500')),
    
    % Test inequality
    query_translate_prolog_to_sql(employee(_, _, _, =\=(0)), TableInfo, SQL6),
    assertion(sub_string(SQL6, _, _, _, 'salary <> 0')).

% Test query pattern analysis
test(pattern_analysis) :-
    sample_table_info(TableInfo),
    PrologQuery = employee(ID, 'John', 'IT', >(50000)),
    query_analyze_prolog_pattern(PrologQuery, TableInfo, Analysis),
    
    assertion(get_dict(table, Analysis, employee)),
    assertion(get_dict(columns, Analysis, ['id', 'name', 'department', 'salary'])),
    
    % Check that we have the right types of arguments
    get_dict(variables, Analysis, Variables),
    get_dict(constants, Analysis, Constants),
    get_dict(conditions, Analysis, Conditions),
    
    assertion(length(Variables, 1)),  % ID is a variable
    assertion(length(Constants, 2)),  % name and department are constants
    assertion(length(Conditions, 1)). % salary constraint

% Test WHERE clause building
test(where_clause_building) :-
    sample_table_info(TableInfo),
    
    % Test with constants
    Conditions1 = [
        arg_analysis{type: constant, column: name, value: 'John', sql_type: 'VARCHAR'},
        arg_analysis{type: constant, column: department, value: 'IT', sql_type: 'VARCHAR'}
    ],
    query_build_where_clause(Conditions1, TableInfo, Where1),
    assertion(sub_string(Where1, _, _, _, "name = 'John'")),
    assertion(sub_string(Where1, _, _, _, "department = 'IT'")),
    assertion(sub_string(Where1, _, _, _, ' AND ')),
    
    % Test with constraints
    Conditions2 = [
        arg_analysis{type: constraint, column: salary, expression: >(50000), sql_type: 'DECIMAL'}
    ],
    query_build_where_clause(Conditions2, TableInfo, Where2),
    assertion(sub_string(Where2, _, _, _, 'salary > 50000')),
    
    % Test empty conditions
    query_build_where_clause([], TableInfo, Where3),
    assertion(Where3 = '').

:- end_tests(query_translation).

% Optimization tests
:- begin_tests(query_optimization).

% Test basic optimization (currently pass-through)
test(basic_optimization) :-
    RawSQL = "SELECT * FROM employee WHERE salary > 50000",
    query_optimize_sql(RawSQL, OptimizedSQL),
    assertion(OptimizedSQL = RawSQL).  % Currently no optimization

% Test selectivity estimation
test(selectivity_estimation) :-
    sample_table_info(TableInfo),
    Query = "SELECT * FROM employee WHERE department = 'Engineering'",
    query_estimate_selectivity(Query, TableInfo, Selectivity),
    assertion(number(Selectivity)),
    assertion(Selectivity >= 0.0),
    assertion(Selectivity =< 1.0).

:- end_tests(query_optimization).

% Edge case tests
:- begin_tests(edge_cases).

% Test with null values
test(null_handling) :-
    sample_table_info(TableInfo),
    PrologQuery = employee(_, null, _, _),
    query_translate_prolog_to_sql(PrologQuery, TableInfo, SQLQuery),
    assertion(sub_string(SQLQuery, _, _, _, 'name = NULL')).

% Test with complex table names
test(complex_table_name) :-
    TableInfo = table_info(['id', 'value'], ['INTEGER', 'VARCHAR']),
    PrologQuery = 'complex_table_name'(1, _),
    query_translate_prolog_to_sql(PrologQuery, TableInfo, SQLQuery),
    assertion(sub_string(SQLQuery, _, _, _, 'FROM complex_table_name')).

% Test with mixed constraint types
test(mixed_constraints) :-
    sample_table_info(TableInfo),
    PrologQuery = employee(>(10), 'Alice', _, <(100000)),
    query_translate_prolog_to_sql(PrologQuery, TableInfo, SQLQuery),
    assertion(sub_string(SQLQuery, _, _, _, 'id > 10')),
    assertion(sub_string(SQLQuery, _, _, _, "name = 'Alice'")),
    assertion(sub_string(SQLQuery, _, _, _, 'salary < 100000')).

:- end_tests(edge_cases).

% Performance tests
:- begin_tests(performance).

% Test translation performance with large queries
test(large_query_translation) :-
    sample_table_info(TableInfo),
    
    % Create a query with many arguments (simulating a wide table)
    Length = 50,
    length(Args, Length),
    maplist(=(var), Args),  % All variables
    PrologQuery =.. [large_table|Args],
    
    % Should complete reasonably quickly
    get_time(Start),
    query_translate_prolog_to_sql(PrologQuery, TableInfo, _SQLQuery),
    get_time(End),
    Duration is End - Start,
    assertion(Duration < 1.0).  % Should complete in less than 1 second

:- end_tests(performance).

% Helper predicates for testing
test_constraint_expression(Expr) :-
    is_constraint_expression(Expr).

% Run all query translation tests
run_query_tests :-
    run_tests([query_translation, query_optimization, edge_cases, performance]).

% Specific test runners
run_translation_tests :-
    run_tests(query_translation).

run_optimization_tests :-
    run_tests(query_optimization).

run_edge_case_tests :-
    run_tests(edge_cases).

% Instructions:
% Load this file: ?- [tests/test_queries].
% Run all tests: ?- run_query_tests.
% Run specific tests: ?- run_translation_tests.
