/**
 * Type Mapper
 * 
 * This module handles conversion between SQL and Prolog data types,
 * ensuring proper data representation across the bridge.
 */

:- module(type_mapper, [
    type_map_sql_to_prolog/3,        % type_map_sql_to_prolog(+SQLValue, +SQLType, -PrologValue)
    type_map_prolog_to_sql/3,        % type_map_prolog_to_sql(+PrologValue, +SQLType, -SQLValue)
    type_map_sql_type_to_prolog_type/3, % type_map_sql_type_to_prolog_type(+SQLType, +IsNullable, -PrologType)
    type_get_sql_literal/2,          % type_get_sql_literal(+Value, -SQLLiteral)
    type_is_numeric/1,               % type_is_numeric(+Type)
    type_is_string/1,                % type_is_string(+Type)
    type_is_temporal/1,              % type_is_temporal(+Type)
    type_compatible/2,               % type_compatible(+Type1, +Type2)
    sql_type_category/2              % sql_type_category(+SQLType, -Category)
]).

:- use_module(library(error)).
:- use_module(library(debug)).

%! type_map_sql_to_prolog(+SQLValue, +SQLType, -PrologValue) is det.
%
%  Convert a SQL value to an appropriate Prolog representation.
%
%  @param SQLValue    Value from SQL result set
%  @param SQLType     SQL data type information
%  @param PrologValue Converted Prolog value
%
type_map_sql_to_prolog(null, _SQLType, null) :- !.

type_map_sql_to_prolog(SQLValue, SQLType, PrologValue) :-
    debug(type_mapper, 'Converting SQL value ~w of type ~w', [SQLValue, SQLType]),
    
    (   sql_type_category(SQLType, numeric)
    ->  convert_numeric_sql_to_prolog(SQLValue, PrologValue)
    ;   sql_type_category(SQLType, string)
    ->  convert_string_sql_to_prolog(SQLValue, PrologValue)
    ;   sql_type_category(SQLType, temporal)
    ->  convert_temporal_sql_to_prolog(SQLValue, PrologValue)
    ;   sql_type_category(SQLType, boolean)
    ->  convert_boolean_sql_to_prolog(SQLValue, PrologValue)
    ;   sql_type_category(SQLType, binary)
    ->  convert_binary_sql_to_prolog(SQLValue, PrologValue)
    ;   % Default: keep as-is
        PrologValue = SQLValue
    ),
    
    debug(type_mapper, 'Converted to Prolog value: ~w', [PrologValue]).

%! type_map_prolog_to_sql(+PrologValue, +SQLType, -SQLValue) is det.
%
%  Convert a Prolog value to SQL representation.
%
%  @param PrologValue Prolog value
%  @param SQLType     Target SQL type
%  @param SQLValue    SQL representation (usually quoted string)
%
type_map_prolog_to_sql(null, _SQLType, 'NULL') :- !.

type_map_prolog_to_sql(PrologValue, SQLType, SQLValue) :-
    debug(type_mapper, 'Converting Prolog value ~w to SQL type ~w', [PrologValue, SQLType]),
    
    (   sql_type_category(SQLType, numeric)
    ->  convert_numeric_prolog_to_sql(PrologValue, SQLValue)
    ;   sql_type_category(SQLType, string)
    ->  convert_string_prolog_to_sql(PrologValue, SQLValue)
    ;   sql_type_category(SQLType, temporal)
    ->  convert_temporal_prolog_to_sql(PrologValue, SQLValue)
    ;   sql_type_category(SQLType, boolean)
    ->  convert_boolean_prolog_to_sql(PrologValue, SQLValue)
    ;   sql_type_category(SQLType, binary)
    ->  convert_binary_prolog_to_sql(PrologValue, SQLValue)
    ;   % Default: quote as string
        type_get_sql_literal(PrologValue, SQLValue)
    ),
    
    debug(type_mapper, 'Converted to SQL value: ~w', [SQLValue]).

%! type_map_sql_type_to_prolog_type(+SQLType, +IsNullable, -PrologType) is det.
%
%  Map SQL type to Prolog type descriptor.
%
type_map_sql_type_to_prolog_type(SQLType, IsNullable, PrologType) :-
    sql_type_category(SQLType, Category),
    (   IsNullable = yes
    ->  PrologType = nullable(Category)
    ;   PrologType = Category
    ).

% SQL Type Categories

%! sql_type_category(+SQLType, -Category) is det.
%
%  Categorize SQL types into broad categories.
%
sql_type_category(SQLType, Category) :-
    (   numeric_sql_type(SQLType)
    ->  Category = numeric
    ;   string_sql_type(SQLType)
    ->  Category = string
    ;   temporal_sql_type(SQLType)
    ->  Category = temporal
    ;   boolean_sql_type(SQLType)
    ->  Category = boolean
    ;   binary_sql_type(SQLType)
    ->  Category = binary
    ;   Category = unknown
    ).

%! numeric_sql_type(+Type) is semidet.
numeric_sql_type('INTEGER').
numeric_sql_type('INT').
numeric_sql_type('BIGINT').
numeric_sql_type('SMALLINT').
numeric_sql_type('TINYINT').
numeric_sql_type('DECIMAL').
numeric_sql_type('NUMERIC').
numeric_sql_type('FLOAT').
numeric_sql_type('REAL').
numeric_sql_type('DOUBLE').
numeric_sql_type('DOUBLE PRECISION').
numeric_sql_type('MONEY').
numeric_sql_type(Type) :-
    atom_string(Type, TypeStr),
    (   sub_string(TypeStr, _, _, _, 'INT')
    ;   sub_string(TypeStr, _, _, _, 'DECIMAL')
    ;   sub_string(TypeStr, _, _, _, 'NUMERIC')
    ;   sub_string(TypeStr, _, _, _, 'FLOAT')
    ;   sub_string(TypeStr, _, _, _, 'DOUBLE')
    ).

%! string_sql_type(+Type) is semidet.
string_sql_type('VARCHAR').
string_sql_type('CHAR').
string_sql_type('TEXT').
string_sql_type('NVARCHAR').
string_sql_type('NCHAR').
string_sql_type('NTEXT').
string_sql_type('CLOB').
string_sql_type('LONGTEXT').
string_sql_type(Type) :-
    atom_string(Type, TypeStr),
    (   sub_string(TypeStr, _, _, _, 'CHAR')
    ;   sub_string(TypeStr, _, _, _, 'TEXT')
    ;   sub_string(TypeStr, _, _, _, 'STRING')
    ).

%! temporal_sql_type(+Type) is semidet.
temporal_sql_type('DATE').
temporal_sql_type('TIME').
temporal_sql_type('DATETIME').
temporal_sql_type('TIMESTAMP').
temporal_sql_type('DATETIME2').
temporal_sql_type('SMALLDATETIME').
temporal_sql_type(Type) :-
    atom_string(Type, TypeStr),
    (   sub_string(TypeStr, _, _, _, 'DATE')
    ;   sub_string(TypeStr, _, _, _, 'TIME')
    ;   sub_string(TypeStr, _, _, _, 'TIMESTAMP')
    ).

%! boolean_sql_type(+Type) is semidet.
boolean_sql_type('BOOLEAN').
boolean_sql_type('BOOL').
boolean_sql_type('BIT').

%! binary_sql_type(+Type) is semidet.
binary_sql_type('BINARY').
binary_sql_type('VARBINARY').
binary_sql_type('BLOB').
binary_sql_type('LONGBLOB').
binary_sql_type('IMAGE').
binary_sql_type(Type) :-
    atom_string(Type, TypeStr),
    (   sub_string(TypeStr, _, _, _, 'BINARY')
    ;   sub_string(TypeStr, _, _, _, 'BLOB')
    ).

% Type Converters

%! convert_numeric_sql_to_prolog(+SQLValue, -PrologValue) is det.
convert_numeric_sql_to_prolog(SQLValue, PrologValue) :-
    (   number(SQLValue)
    ->  PrologValue = SQLValue
    ;   atom_number(SQLValue, PrologValue)
    ->  true
    ;   atom_string(SQLValue, SQLString),
        number_string(PrologValue, SQLString)
    ->  true
    ;   % If conversion fails, keep as atom
        PrologValue = SQLValue
    ).

%! convert_string_sql_to_prolog(+SQLValue, -PrologValue) is det.
convert_string_sql_to_prolog(SQLValue, PrologValue) :-
    (   string(SQLValue)
    ->  PrologValue = SQLValue
    ;   atom(SQLValue)
    ->  atom_string(SQLValue, PrologValue)
    ;   PrologValue = SQLValue
    ).

%! convert_temporal_sql_to_prolog(+SQLValue, -PrologValue) is det.
convert_temporal_sql_to_prolog(SQLValue, PrologValue) :-
    % For now, keep temporal values as strings
    % Future enhancement: parse into Prolog date/time structures
    (   string(SQLValue)
    ->  PrologValue = SQLValue
    ;   atom_string(SQLValue, PrologValue)
    ).

%! convert_boolean_sql_to_prolog(+SQLValue, -PrologValue) is det.
convert_boolean_sql_to_prolog(SQLValue, PrologValue) :-
    (   SQLValue == 1
    ->  PrologValue = true
    ;   SQLValue == 0
    ->  PrologValue = false
    ;   SQLValue == true
    ->  PrologValue = true
    ;   SQLValue == false
    ->  PrologValue = false
    ;   SQLValue == 'TRUE'
    ->  PrologValue = true
    ;   SQLValue == 'FALSE'
    ->  PrologValue = false
    ;   PrologValue = SQLValue
    ).

%! convert_binary_sql_to_prolog(+SQLValue, -PrologValue) is det.
convert_binary_sql_to_prolog(SQLValue, PrologValue) :-
    % Keep binary data as-is for now
    PrologValue = SQLValue.

%! convert_numeric_prolog_to_sql(+PrologValue, -SQLValue) is det.
convert_numeric_prolog_to_sql(PrologValue, SQLValue) :-
    (   number(PrologValue)
    ->  format(string(SQLValue), '~w', [PrologValue])
    ;   atom_number(PrologValue, Number)
    ->  format(string(SQLValue), '~w', [Number])
    ;   format(string(SQLValue), '~w', [PrologValue])
    ).

%! convert_string_prolog_to_sql(+PrologValue, -SQLValue) is det.
convert_string_prolog_to_sql(PrologValue, SQLValue) :-
    (   string(PrologValue)
    ->  format(string(SQLValue), "'~w'", [PrologValue])
    ;   atom(PrologValue)
    ->  format(string(SQLValue), "'~w'", [PrologValue])
    ;   format(string(SQLValue), "'~w'", [PrologValue])
    ).

%! convert_temporal_prolog_to_sql(+PrologValue, -SQLValue) is det.
convert_temporal_prolog_to_sql(PrologValue, SQLValue) :-
    % For now, treat temporal values as strings
    format(string(SQLValue), "'~w'", [PrologValue]).

%! convert_boolean_prolog_to_sql(+PrologValue, -SQLValue) is det.
convert_boolean_prolog_to_sql(PrologValue, SQLValue) :-
    (   PrologValue == true
    ->  SQLValue = '1'
    ;   PrologValue == false
    ->  SQLValue = '0'
    ;   format(string(SQLValue), "'~w'", [PrologValue])
    ).

%! convert_binary_prolog_to_sql(+PrologValue, -SQLValue) is det.
convert_binary_prolog_to_sql(PrologValue, SQLValue) :-
    % Keep binary data as-is
    format(string(SQLValue), "'~w'", [PrologValue]).

%! type_get_sql_literal(+Value, -SQLLiteral) is det.
%
%  Convert any Prolog value to a SQL literal representation.
%
type_get_sql_literal(Value, SQLLiteral) :-
    (   number(Value)
    ->  format(string(SQLLiteral), '~w', [Value])
    ;   Value == null
    ->  SQLLiteral = 'NULL'
    ;   % Quote strings and atoms
        format(string(SQLLiteral), "'~w'", [Value])
    ).

% Type Predicates

%! type_is_numeric(+Type) is semidet.
type_is_numeric(Type) :-
    sql_type_category(Type, numeric).

%! type_is_string(+Type) is semidet.
type_is_string(Type) :-
    sql_type_category(Type, string).

%! type_is_temporal(+Type) is semidet.
type_is_temporal(Type) :-
    sql_type_category(Type, temporal).

%! type_compatible(+Type1, +Type2) is semidet.
%
%  Check if two types are compatible for operations.
%
type_compatible(Type1, Type2) :-
    sql_type_category(Type1, Category1),
    sql_type_category(Type2, Category2),
    compatible_categories(Category1, Category2).

%! compatible_categories(+Cat1, +Cat2) is semidet.
compatible_categories(numeric, numeric).
compatible_categories(string, string).
compatible_categories(temporal, temporal).
compatible_categories(boolean, boolean).
compatible_categories(Category, Category).

% Debug support
:- debug(type_mapper).
