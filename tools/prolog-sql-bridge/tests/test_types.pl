/**
 * Type Mapping Tests
 * 
 * Test suite for SQL-Prolog type conversion functionality.
 */

:- use_module('../src/type_mapper').
:- use_module(library(plunit)).

:- begin_tests(type_mapping).

% Test SQL to Prolog conversions
test(sql_to_prolog_integer) :-
    type_map_sql_to_prolog(42, 'INTEGER', PrologValue),
    assertion(PrologValue = 42).

test(sql_to_prolog_string) :-
    type_map_sql_to_prolog('Hello World', 'VARCHAR', PrologValue),
    assertion(PrologValue = "Hello World").

test(sql_to_prolog_null) :-
    type_map_sql_to_prolog(null, 'VARCHAR', PrologValue),
    assertion(PrologValue = null).

test(sql_to_prolog_float) :-
    type_map_sql_to_prolog(3.14159, 'FLOAT', PrologValue),
    assertion(PrologValue = 3.14159).

test(sql_to_prolog_boolean_true) :-
    type_map_sql_to_prolog(1, 'BOOLEAN', PrologValue),
    assertion(PrologValue = true).

test(sql_to_prolog_boolean_false) :-
    type_map_sql_to_prolog(0, 'BOOLEAN', PrologValue),
    assertion(PrologValue = false).

% Test Prolog to SQL conversions
test(prolog_to_sql_integer) :-
    type_map_prolog_to_sql(42, 'INTEGER', SQLValue),
    assertion(SQLValue = '42').

test(prolog_to_sql_string) :-
    type_map_prolog_to_sql("Hello World", 'VARCHAR', SQLValue),
    assertion(SQLValue = "'Hello World'").

test(prolog_to_sql_atom) :-
    type_map_prolog_to_sql(hello, 'VARCHAR', SQLValue),
    assertion(SQLValue = "'hello'").

test(prolog_to_sql_null) :-
    type_map_prolog_to_sql(null, 'VARCHAR', SQLValue),
    assertion(SQLValue = 'NULL').

test(prolog_to_sql_float) :-
    type_map_prolog_to_sql(3.14159, 'FLOAT', SQLValue),
    assertion(SQLValue = '3.14159').

test(prolog_to_sql_boolean_true) :-
    type_map_prolog_to_sql(true, 'BOOLEAN', SQLValue),
    assertion(SQLValue = '1').

test(prolog_to_sql_boolean_false) :-
    type_map_prolog_to_sql(false, 'BOOLEAN', SQLValue),
    assertion(SQLValue = '0').

:- end_tests(type_mapping).

% SQL type categorization tests
:- begin_tests(type_categorization).

test(numeric_types) :-
    assertion(numeric_sql_type('INTEGER')),
    assertion(numeric_sql_type('FLOAT')),
    assertion(numeric_sql_type('DECIMAL')),
    assertion(numeric_sql_type('BIGINT')),
    assertion(\+ numeric_sql_type('VARCHAR')).

test(string_types) :-
    assertion(string_sql_type('VARCHAR')),
    assertion(string_sql_type('CHAR')),
    assertion(string_sql_type('TEXT')),
    assertion(string_sql_type('NVARCHAR')),
    assertion(\+ string_sql_type('INTEGER')).

test(temporal_types) :-
    assertion(temporal_sql_type('DATE')),
    assertion(temporal_sql_type('TIME')),
    assertion(temporal_sql_type('DATETIME')),
    assertion(temporal_sql_type('TIMESTAMP')),
    assertion(\+ temporal_sql_type('VARCHAR')).

test(boolean_types) :-
    assertion(boolean_sql_type('BOOLEAN')),
    assertion(boolean_sql_type('BOOL')),
    assertion(boolean_sql_type('BIT')),
    assertion(\+ boolean_sql_type('INTEGER')).

test(binary_types) :-
    assertion(binary_sql_type('BINARY')),
    assertion(binary_sql_type('VARBINARY')),
    assertion(binary_sql_type('BLOB')),
    assertion(\+ binary_sql_type('VARCHAR')).

:- end_tests(type_categorization).

% Type category mapping tests
:- begin_tests(type_category_mapping).

test(category_numeric) :-
    sql_type_category('INTEGER', Category),
    assertion(Category = numeric).

test(category_string) :-
    sql_type_category('VARCHAR', Category),
    assertion(Category = string).

test(category_temporal) :-
    sql_type_category('DATE', Category),
    assertion(Category = temporal).

test(category_boolean) :-
    sql_type_category('BOOLEAN', Category),
    assertion(Category = boolean).

test(category_binary) :-
    sql_type_category('BLOB', Category),
    assertion(Category = binary).

test(category_unknown) :-
    sql_type_category('UNKNOWN_TYPE', Category),
    assertion(Category = unknown).

:- end_tests(type_category_mapping).

% SQL type to Prolog type mapping tests
:- begin_tests(type_descriptor_mapping).

test(nullable_type) :-
    type_map_sql_type_to_prolog_type('VARCHAR', yes, PrologType),
    assertion(PrologType = nullable(string)).

test(non_nullable_type) :-
    type_map_sql_type_to_prolog_type('INTEGER', no, PrologType),
    assertion(PrologType = numeric).

:- end_tests(type_descriptor_mapping).

% SQL literal generation tests
:- begin_tests(sql_literal_generation).

test(number_literal) :-
    type_get_sql_literal(42, Literal),
    assertion(Literal = '42').

test(string_literal) :-
    type_get_sql_literal("hello", Literal),
    assertion(Literal = "'hello'").

test(atom_literal) :-
    type_get_sql_literal(world, Literal),
    assertion(Literal = "'world'").

test(null_literal) :-
    type_get_sql_literal(null, Literal),
    assertion(Literal = 'NULL').

test(float_literal) :-
    type_get_sql_literal(3.14, Literal),
    assertion(Literal = '3.14').

:- end_tests(sql_literal_generation).

% Type compatibility tests
:- begin_tests(type_compatibility).

test(compatible_numeric) :-
    assertion(type_compatible('INTEGER', 'FLOAT')),
    assertion(type_compatible('DECIMAL', 'BIGINT')).

test(compatible_string) :-
    assertion(type_compatible('VARCHAR', 'CHAR')),
    assertion(type_compatible('TEXT', 'NVARCHAR')).

test(compatible_temporal) :-
    assertion(type_compatible('DATE', 'DATETIME')),
    assertion(type_compatible('TIME', 'TIMESTAMP')).

test(incompatible_types) :-
    assertion(\+ type_compatible('INTEGER', 'VARCHAR')),
    assertion(\+ type_compatible('DATE', 'BOOLEAN')),
    assertion(\+ type_compatible('BLOB', 'TEXT')).

test(same_type_compatible) :-
    assertion(type_compatible('VARCHAR', 'VARCHAR')),
    assertion(type_compatible('INTEGER', 'INTEGER')).

:- end_tests(type_compatibility).

% Edge case tests
:- begin_tests(type_edge_cases).

% Test conversion of string numbers
test(string_number_conversion) :-
    type_map_sql_to_prolog('42', 'INTEGER', PrologValue),
    assertion(PrologValue = 42).

test(string_float_conversion) :-
    type_map_sql_to_prolog('3.14', 'FLOAT', PrologValue),
    assertion(PrologValue = 3.14).

% Test boolean variants
test(boolean_variants) :-
    type_map_sql_to_prolog('TRUE', 'BOOLEAN', Value1),
    assertion(Value1 = true),
    
    type_map_sql_to_prolog('FALSE', 'BOOLEAN', Value2),
    assertion(Value2 = false),
    
    type_map_sql_to_prolog(true, 'BOOLEAN', Value3),
    assertion(Value3 = true),
    
    type_map_sql_to_prolog(false, 'BOOLEAN', Value4),
    assertion(Value4 = false).

% Test large numbers
test(large_numbers) :-
    LargeNumber = 9223372036854775807,  % Max 64-bit signed integer
    type_map_prolog_to_sql(LargeNumber, 'BIGINT', SQLValue),
    atom_number(SQLValue, ConvertedBack),
    assertion(ConvertedBack = LargeNumber).

% Test special float values
test(special_floats) :-
    % Test very small number
    SmallFloat = 1.23e-10,
    type_map_prolog_to_sql(SmallFloat, 'FLOAT', SQLValue1),
    assertion(sub_string(SQLValue1, _, _, _, '1.23e-10')),
    
    % Test very large number
    LargeFloat = 1.23e10,
    type_map_prolog_to_sql(LargeFloat, 'FLOAT', SQLValue2),
    assertion(sub_string(SQLValue2, _, _, _, '1.23e+10')).

% Test empty strings
test(empty_string) :-
    type_map_prolog_to_sql("", 'VARCHAR', SQLValue),
    assertion(SQLValue = "''").

% Test strings with quotes
test(quoted_strings) :-
    type_map_prolog_to_sql("Don't", 'VARCHAR', SQLValue),
    assertion(SQLValue = "'Don't'").  % Should handle apostrophes

:- end_tests(type_edge_cases).

% Performance tests
:- begin_tests(type_performance).

% Test conversion performance
test(conversion_performance) :-
    % Test many conversions
    numlist(1, 1000, Numbers),
    get_time(Start),
    forall(member(N, Numbers),
           type_map_prolog_to_sql(N, 'INTEGER', _)),
    get_time(End),
    Duration is End - Start,
    assertion(Duration < 1.0).  % Should complete quickly

:- end_tests(type_performance).

% Database-specific type tests
:- begin_tests(database_specific_types).

% MySQL-specific types
test(mysql_types) :-
    assertion(string_sql_type('LONGTEXT')),
    assertion(temporal_sql_type('DATETIME2')),
    assertion(binary_sql_type('LONGBLOB')).

% PostgreSQL-specific types
test(postgresql_types) :-
    assertion(numeric_sql_type('BIGINT')),
    assertion(temporal_sql_type('TIMESTAMP')).

% SQLite-specific behavior
test(sqlite_types) :-
    % SQLite is more flexible with types
    assertion(numeric_sql_type('INTEGER')),
    assertion(string_sql_type('TEXT')).

:- end_tests(database_specific_types).

% Run all type mapping tests
run_type_tests :-
    run_tests([
        type_mapping,
        type_categorization,
        type_category_mapping,
        type_descriptor_mapping,
        sql_literal_generation,
        type_compatibility,
        type_edge_cases,
        type_performance,
        database_specific_types
    ]).

% Specific test runners
run_mapping_tests :-
    run_tests(type_mapping).

run_categorization_tests :-
    run_tests(type_categorization).

run_compatibility_tests :-
    run_tests(type_compatibility).

run_edge_case_tests :-
    run_tests(type_edge_cases).

% Instructions:
% Load this file: ?- [tests/test_types].
% Run all tests: ?- run_type_tests.
% Run specific tests: ?- run_mapping_tests.
