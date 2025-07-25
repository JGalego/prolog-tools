/**
 * Schema Introspector
 * 
 * This module provides database schema introspection capabilities,
 * allowing automatic discovery of tables, columns, and data types.
 */

:- module(schema_introspector, [
    schema_get_all_tables/2,       % schema_get_all_tables(+Connection, -Tables)
    schema_get_table_info/3,       % schema_get_table_info(+Connection, +TableName, -TableInfo)
    schema_get_column_info/4,      % schema_get_column_info(+Connection, +TableName, +ColumnName, -ColumnInfo)
    schema_get_primary_keys/3,     % schema_get_primary_keys(+Connection, +TableName, -PrimaryKeys)
    schema_get_foreign_keys/3,     % schema_get_foreign_keys(+Connection, +TableName, -ForeignKeys)
    schema_get_indexes/3,          % schema_get_indexes(+Connection, +TableName, -Indexes)
    schema_get_database_info/2,    % schema_get_database_info(+Connection, -DatabaseInfo)
    schema_cache_clear/0,          % schema_cache_clear
    schema_cache_table/3           % schema_cache_table(+Connection, +TableName, +TableInfo)
]).

:- use_module(library(odbc)).
:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(db_connection).
:- use_module(type_mapper).

% Dynamic predicates for schema caching
:- dynamic schema_cache/3.  % schema_cache(ConnectionId, TableName, TableInfo)
:- dynamic database_cache/2. % database_cache(ConnectionId, DatabaseInfo)

%! schema_get_all_tables(+Connection, -Tables) is det.
%
%  Get a list of all table names in the database.
%
%  @param Connection Connection handle
%  @param Tables     List of table names
%
schema_get_all_tables(Connection, Tables) :-
    debug(schema_introspector, 'Getting all tables for connection: ~w', [Connection]),
    
    connection(ConnectionId, Handle) = Connection,
    
    % Try to get from cache first
    (   database_cache(ConnectionId, DatabaseInfo),
        Tables = DatabaseInfo.tables
    ->  debug(schema_introspector, 'Retrieved ~w tables from cache', [Tables])
    ;   % Query the database
        get_tables_from_database(Handle, Tables),
        length(Tables, Count),
        debug(schema_introspector, 'Found ~w tables in database', [Count])
    ).

%! get_tables_from_database(+Handle, -Tables) is det.
%
%  Query the database for table information.
%
get_tables_from_database(Handle, Tables) :-
    % Use ODBC catalog functions to get table information
    findall(
        TableName,
        (
            odbc_current_table(Handle, TableName),
            \+ system_table(TableName)
        ),
        Tables
    ).

%! system_table(+TableName) is semidet.
%
%  Check if a table is a system table that should be ignored.
%
system_table(TableName) :-
    atom_string(TableName, TableStr),
    (   sub_string(TableStr, 0, _, _, 'sys')
    ;   sub_string(TableStr, 0, _, _, 'INFORMATION_SCHEMA')
    ;   sub_string(TableStr, 0, _, _, 'pg_')
    ;   sub_string(TableStr, 0, _, _, 'mysql.')
    ).

%! schema_get_table_info(+Connection, +TableName, -TableInfo) is det.
%
%  Get comprehensive information about a table.
%
%  @param Connection Connection handle
%  @param TableName  Name of the table
%  @param TableInfo  table_info(Columns, Types, Constraints)
%
schema_get_table_info(Connection, TableName, TableInfo) :-
    debug(schema_introspector, 'Getting table info for: ~w', [TableName]),
    
    connection(ConnectionId, Handle) = Connection,
    
    % Try cache first
    (   schema_cache(ConnectionId, TableName, CachedInfo)
    ->  TableInfo = CachedInfo,
        debug(schema_introspector, 'Retrieved table info from cache', [])
    ;   % Query database for table information
        get_table_info_from_database(Handle, TableName, TableInfo),
        % Cache the result
        schema_cache_table(Connection, TableName, TableInfo),
        debug(schema_introspector, 'Cached table info for: ~w', [TableName])
    ).

%! get_table_info_from_database(+Handle, +TableName, -TableInfo) is det.
%
%  Query the database for table schema information.
%
get_table_info_from_database(Handle, TableName, TableInfo) :-
    % Get column information
    get_columns_info(Handle, TableName, Columns, Types),
    
    % Get constraints (primary keys, foreign keys, etc.)
    get_table_constraints(Handle, TableName, Constraints),
    
    TableInfo = table_info(Columns, Types, Constraints).

%! get_columns_info(+Handle, +TableName, -Columns, -Types) is det.
%
%  Get column names and types for a table.
%
get_columns_info(Handle, TableName, Columns, Types) :-
    findall(
        column_info(ColumnName, DataType, IsNullable, DefaultValue),
        odbc_column_property(Handle, TableName, ColumnName, 
                           column(DataType, IsNullable, DefaultValue)),
        ColumnInfos
    ),
    
    % Extract column names and types
    maplist(extract_column_name, ColumnInfos, Columns),
    maplist(extract_column_type, ColumnInfos, Types).

%! extract_column_name(+ColumnInfo, -ColumnName) is det.
extract_column_name(column_info(ColumnName, _, _, _), ColumnName).

%! extract_column_type(+ColumnInfo, -Type) is det.
extract_column_type(column_info(_, DataType, IsNullable, _), Type) :-
    type_map_sql_type_to_prolog_type(DataType, IsNullable, Type).

%! get_table_constraints(+Handle, +TableName, -Constraints) is det.
%
%  Get table constraints (primary keys, foreign keys, etc.).
%
get_table_constraints(Handle, TableName, Constraints) :-
    get_primary_key_constraints(Handle, TableName, PKConstraints),
    get_foreign_key_constraints(Handle, TableName, FKConstraints),
    get_unique_constraints(Handle, TableName, UQConstraints),
    append([PKConstraints, FKConstraints, UQConstraints], Constraints).

%! get_primary_key_constraints(+Handle, +TableName, -PKConstraints) is det.
get_primary_key_constraints(Handle, TableName, PKConstraints) :-
    findall(
        primary_key(ColumnName),
        odbc_primary_key(Handle, TableName, ColumnName),
        PKConstraints
    ).

%! get_foreign_key_constraints(+Handle, +TableName, -FKConstraints) is det.
get_foreign_key_constraints(Handle, TableName, FKConstraints) :-
    findall(
        foreign_key(ColumnName, RefTable, RefColumn),
        odbc_foreign_key(Handle, TableName, ColumnName, RefTable, RefColumn),
        FKConstraints
    ).

%! get_unique_constraints(+Handle, +TableName, -UQConstraints) is det.
get_unique_constraints(_Handle, _TableName, []) :-
    % Placeholder - implement based on database-specific queries
    true.

%! schema_get_column_info(+Connection, +TableName, +ColumnName, -ColumnInfo) is det.
%
%  Get detailed information about a specific column.
%
schema_get_column_info(Connection, TableName, ColumnName, ColumnInfo) :-
    schema_get_table_info(Connection, TableName, TableInfo),
    TableInfo = table_info(Columns, Types, _Constraints),
    
    % Find the column
    nth1(Index, Columns, ColumnName),
    nth1(Index, Types, ColumnType),
    
    ColumnInfo = column_info{
        name: ColumnName,
        type: ColumnType,
        table: TableName
    }.

%! schema_get_primary_keys(+Connection, +TableName, -PrimaryKeys) is det.
%
%  Get the primary key columns for a table.
%
schema_get_primary_keys(Connection, TableName, PrimaryKeys) :-
    schema_get_table_info(Connection, TableName, TableInfo),
    TableInfo = table_info(_Columns, _Types, Constraints),
    
    findall(
        ColumnName,
        member(primary_key(ColumnName), Constraints),
        PrimaryKeys
    ).

%! schema_get_foreign_keys(+Connection, +TableName, -ForeignKeys) is det.
%
%  Get the foreign key relationships for a table.
%
schema_get_foreign_keys(Connection, TableName, ForeignKeys) :-
    schema_get_table_info(Connection, TableName, TableInfo),
    TableInfo = table_info(_Columns, _Types, Constraints),
    
    findall(
        foreign_key(ColumnName, RefTable, RefColumn),
        member(foreign_key(ColumnName, RefTable, RefColumn), Constraints),
        ForeignKeys
    ).

%! schema_get_indexes(+Connection, +TableName, -Indexes) is det.
%
%  Get index information for a table.
%
schema_get_indexes(Connection, TableName, Indexes) :-
    connection(_ConnectionId, Handle) = Connection,
    
    findall(
        index_info(IndexName, ColumnName, IsUnique),
        odbc_statistics(Handle, TableName, IndexName, ColumnName, IsUnique),
        Indexes
    ).

%! schema_get_database_info(+Connection, -DatabaseInfo) is det.
%
%  Get general information about the database.
%
schema_get_database_info(Connection, DatabaseInfo) :-
    connection(ConnectionId, Handle) = Connection,
    
    % Try cache first
    (   database_cache(ConnectionId, CachedInfo)
    ->  DatabaseInfo = CachedInfo
    ;   % Query database
        get_database_info_from_connection(Handle, DatabaseInfo),
        assert(database_cache(ConnectionId, DatabaseInfo))
    ).

%! get_database_info_from_connection(+Handle, -DatabaseInfo) is det.
get_database_info_from_connection(Handle, DatabaseInfo) :-
    % Get database name
    (   catch(odbc_get_connection(Handle, database_name(DatabaseName)), _, 
              DatabaseName = unknown)
    ->  true
    ;   DatabaseName = unknown
    ),
    
    % Get DBMS name and version
    (   catch(odbc_get_connection(Handle, dbms_name(DBMSName)), _, 
              DBMSName = unknown)
    ->  true
    ;   DBMSName = unknown
    ),
    
    (   catch(odbc_get_connection(Handle, dbms_version(DBMSVersion)), _, 
              DBMSVersion = unknown)
    ->  true
    ;   DBMSVersion = unknown
    ),
    
    % Get list of tables
    get_tables_from_database(Handle, Tables),
    
    DatabaseInfo = database_info{
        name: DatabaseName,
        dbms_name: DBMSName,
        dbms_version: DBMSVersion,
        tables: Tables
    }.

%! schema_cache_table(+Connection, +TableName, +TableInfo) is det.
%
%  Cache table information for faster subsequent access.
%
schema_cache_table(connection(ConnectionId, _Handle), TableName, TableInfo) :-
    retractall(schema_cache(ConnectionId, TableName, _)),
    assert(schema_cache(ConnectionId, TableName, TableInfo)).

%! schema_cache_clear is det.
%
%  Clear all cached schema information.
%
schema_cache_clear :-
    retractall(schema_cache(_, _, _)),
    retractall(database_cache(_, _)),
    debug(schema_introspector, 'Schema cache cleared', []).

% Error handling for missing ODBC catalog functions
:- if(\+ current_predicate(odbc_current_table/2)).
odbc_current_table(Handle, TableName) :-
    % Fallback implementation using SQL queries
    Query = "SELECT table_name FROM information_schema.tables WHERE table_type = 'BASE TABLE'",
    odbc_query(Handle, Query, row(TableName)).
:- endif.

:- if(\+ current_predicate(odbc_column_property/4)).
odbc_column_property(Handle, TableName, ColumnName, column(DataType, IsNullable, DefaultValue)) :-
    % Fallback implementation
    format(string(Query), 
           "SELECT column_name, data_type, is_nullable, column_default FROM information_schema.columns WHERE table_name = '~w'",
           [TableName]),
    odbc_query(Handle, Query, row(ColumnName, DataType, IsNullable, DefaultValue)).
:- endif.

:- if(\+ current_predicate(odbc_primary_key/3)).
odbc_primary_key(_Handle, _TableName, _ColumnName) :-
    % Fallback - could be implemented with database-specific queries
    fail.
:- endif.

:- if(\+ current_predicate(odbc_foreign_key/5)).
odbc_foreign_key(_Handle, _TableName, _ColumnName, _RefTable, _RefColumn) :-
    % Fallback - could be implemented with database-specific queries
    fail.
:- endif.

:- if(\+ current_predicate(odbc_statistics/5)).
odbc_statistics(_Handle, _TableName, _IndexName, _ColumnName, _IsUnique) :-
    % Fallback - could be implemented with database-specific queries
    fail.
:- endif.

% Debug support
:- debug(schema_introspector).
