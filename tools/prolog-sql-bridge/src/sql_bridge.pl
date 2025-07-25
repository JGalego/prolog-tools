/**
 * SQL Bridge - Main Interface Module
 * 
 * This module provides the main interface for the Prolog-SQL bridge,
 * allowing users to query SQL databases as if they were Prolog facts.
 */

:- module(sql_bridge, [
    sql_connect/2,           % sql_connect(+ConnectionString, -Connection)
    sql_connect_file/2,      % sql_connect_file(+FilePath, -Connection)
    sql_connect_dsn/2,       % sql_connect_dsn(+DSNName, -Connection)
    sql_disconnect/1,        % sql_disconnect(+Connection)
    sql_register_table/2,    % sql_register_table(+Connection, +TableName)
    sql_register_tables/1,   % sql_register_tables(+Connection)
    sql_query/2,            % sql_query(+Query, -Results)
    sql_fact/1,             % sql_fact(?Fact)
    sql_retract_all/0,      % sql_retract_all
    sql_connection_info/2,  % sql_connection_info(+Connection, -Info)
    sql_diagnose_odbc/0     % sql_diagnose_odbc
]).

:- use_module(library(odbc)).
:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(db_connection).
:- use_module(query_translator).
:- use_module(schema_introspector).
:- use_module(type_mapper).

%! sql_connect(+ConnectionString, -Connection) is det.
%
%  Establish a connection to a SQL database using ODBC.
%  
%  @param ConnectionString ODBC connection string (e.g., 'DSN=mydb;UID=user;PWD=pass')
%  @param Connection       Connection handle for subsequent operations
%
%  @example
%  ?- sql_connect('DSN=employees;UID=admin;PWD=secret', Conn).
%
sql_connect(ConnectionString, Connection) :-
    debug(sql_bridge, 'Connecting to database: ~w', [ConnectionString]),
    db_connect(ConnectionString, Connection),
    db_register_connection(Connection),
    debug(sql_bridge, 'Successfully connected with handle: ~w', [Connection]).

%! sql_connect_file(+FilePath, -Connection) is det.
%
%  Establish a connection to a SQLite database file directly.
%  This predicate tries multiple connection methods to handle
%  various ODBC driver configurations.
%  
%  @param FilePath    Path to SQLite database file
%  @param Connection  Connection handle for subsequent operations
%
%  @example
%  ?- sql_connect_file('examples/sample.db', Conn).
%
sql_connect_file(FilePath, Connection) :-
    debug(sql_bridge, 'Connecting to SQLite file: ~w', [FilePath]),
    (   exists_file(FilePath)
    ->  true
    ;   existence_error(file, FilePath)
    ),
    
    % Try different connection string formats
    (   % Method 1: Try SQLite3 driver
        catch(
            (format(atom(ConnStr1), 'DRIVER=SQLite3;DATABASE=~w', [FilePath]),
             sql_connect(ConnStr1, Connection)),
            _Error1,
            fail
        )
    ->  true
    ;   % Method 2: Try SQLite driver  
        catch(
            (format(atom(ConnStr2), 'DRIVER=SQLite;DATABASE=~w', [FilePath]),
             sql_connect(ConnStr2, Connection)),
            _Error2,
            fail
        )
    ->  true
    ;   % Method 3: Try with absolute path and different format
        absolute_file_name(FilePath, AbsPath),
        catch(
            (format(atom(ConnStr3), 'DRIVER={SQLite3 ODBC Driver};DATABASE=~w', [AbsPath]),
             sql_connect(ConnStr3, Connection)),
            _Error3,
            fail
        )
    ->  true
    ;   % If all methods fail, provide helpful error
        format(atom(ErrorMsg), 
               'Failed to connect to SQLite file ~w. ODBC driver issues detected. Please check ODBC configuration or use sql_connect/2 with a proper DSN.', 
               [FilePath]),
        throw(error(connection_failed, ErrorMsg))
    ).

%! sql_disconnect(+Connection) is det.
%
%  Close a database connection and clean up associated resources.
%
%  @param Connection Connection handle to close
%
sql_disconnect(Connection) :-
    debug(sql_bridge, 'Disconnecting from database: ~w', [Connection]),
    db_cleanup_dynamic_facts(Connection),
    db_unregister_connection(Connection),
    db_disconnect(Connection),
    debug(sql_bridge, 'Successfully disconnected', []).

%! sql_connect_dsn(+DSNName, -Connection) is det.
%
%  Connect to a database using a Data Source Name (DSN).
%  This is a convenience predicate for DSN-based connections.
%
%  @param DSNName     Data Source Name configured in ODBC
%  @param Connection  Connection handle for subsequent operations
%
%  @example
%  ?- sql_connect_dsn(sample_db, Conn).
%
sql_connect_dsn(DSNName, Connection) :-
    format(atom(ConnectionString), 'DSN=~w', [DSNName]),
    sql_connect(ConnectionString, Connection).

%! sql_diagnose_odbc is det.
%
%  Diagnose ODBC configuration and provide helpful information
%  about available drivers and data sources.
%
sql_diagnose_odbc :-
    write('=== ODBC Configuration Diagnosis ==='), nl,
    
    % Check ODBC installation
    (   catch(process_create(path(odbcinst), ['-j'], [stdout(pipe(Stream))]), _, fail),
        read_string(Stream, _, Info),
        close(Stream)
    ->  format('ODBC Installation:~n~w~n', [Info])
    ;   write('Warning: odbcinst not found or not working'), nl
    ),
    
    % Check available drivers
    write('Available ODBC Drivers:'), nl,
    (   catch(process_create(path(odbcinst), ['-q', '-d'], [stdout(pipe(Stream2))]), _, fail),
        read_string(Stream2, _, Drivers),
        close(Stream2)
    ->  format('~w~n', [Drivers])
    ;   write('  Could not retrieve driver information'), nl
    ),
    
    % Check data sources
    write('Available Data Sources:'), nl,
    (   catch(process_create(path(odbcinst), ['-q', '-s'], [stdout(pipe(Stream3))]), _, fail),
        read_string(Stream3, _, Sources),
        close(Stream3)
    ->  format('~w~n', [Sources])
    ;   write('  Could not retrieve data source information'), nl
    ),
    
    % Test SWI-Prolog ODBC support
    write('SWI-Prolog ODBC Module:'), nl,
    (   catch(use_module(library(odbc)), Error, (format('  Error loading: ~w~n', [Error]), fail))
    ->  write('  ✓ ODBC module loaded successfully'), nl
    ;   write('  ✗ ODBC module failed to load'), nl
    ).

%! sql_register_table(+Connection, +TableName) is det.
%
%  Register a single table for Prolog querying. This creates dynamic
%  predicates that can be queried as if they were Prolog facts.
%
%  @param Connection Connection handle
%  @param TableName  Name of the table to register
%
sql_register_table(Connection, TableName) :-
    debug(sql_bridge, 'Registering table: ~w', [TableName]),
    schema_get_table_info(Connection, TableName, TableInfo),
    create_dynamic_predicate(Connection, TableName, TableInfo),
    debug(sql_bridge, 'Successfully registered table: ~w', [TableName]).

%! sql_register_tables(+Connection) is det.
%
%  Register all tables in the connected database for Prolog querying.
%
%  @param Connection Connection handle
%
sql_register_tables(Connection) :-
    debug(sql_bridge, 'Auto-registering all tables', []),
    schema_get_all_tables(Connection, Tables),
    forall(member(Table, Tables), sql_register_table(Connection, Table)),
    length(Tables, Count),
    debug(sql_bridge, 'Registered ~w tables', [Count]).

%! sql_query(+Query, -Results) is det.
%
%  Execute a direct SQL query and return results.
%
%  @param Query   SQL query string
%  @param Results List of result rows
%
sql_query(Query, Results) :-
    db_get_current_connection(Connection),
    db_execute_query(Connection, Query, Results).

%! sql_fact(?Fact) is nondet.
%
%  Query registered tables as Prolog facts. This is the main predicate
%  for transparent SQL-to-Prolog integration.
%
%  @param Fact Prolog term representing a database row
%
sql_fact(Fact) :-
    call(Fact).

%! sql_retract_all is det.
%
%  Remove all dynamically created predicates for SQL tables.
%
sql_retract_all :-
    debug(sql_bridge, 'Retracting all dynamic SQL facts', []),
    db_cleanup_all_dynamic_facts(),
    debug(sql_bridge, 'All dynamic facts retracted', []).

%! sql_connection_info(+Connection, -Info) is det.
%
%  Get information about a database connection.
%
%  @param Connection Connection handle
%  @param Info       Dictionary containing connection information
%
sql_connection_info(Connection, Info) :-
    db_get_connection_info(Connection, Info).

%! create_dynamic_predicate(+Connection, +TableName, +TableInfo) is det.
%
%  Create a dynamic predicate for a SQL table that translates Prolog
%  queries to SQL and executes them transparently.
%
%  @param Connection Connection handle
%  @param TableName  Name of the table
%  @param TableInfo  Schema information for the table
%
create_dynamic_predicate(Connection, TableName, TableInfo) :-
    TableInfo = table_info(Columns, _Types),
    length(Columns, Arity),
    functor(Template, TableName, Arity),
    
    % Create the dynamic predicate clause
    Template =.. [TableName|Args],
    
    % Generate the goal that will execute the SQL query
    Goal = (
        query_translate_prolog_to_sql(Template, TableInfo, SQLQuery),
        db_execute_query(Connection, SQLQuery, Rows),
        member(Row, Rows),
        map_sql_row_to_prolog_args(Row, TableInfo, Args)
    ),
    
    % Assert the dynamic clause
    assertz((Template :- Goal)),
    
    % Make the predicate dynamic
    functor(Template, Functor, Arity),
    dynamic(Functor/Arity).

%! map_sql_row_to_prolog_args(+Row, +TableInfo, -Args) is det.
%
%  Convert a SQL result row to Prolog arguments with proper type mapping.
%
%  @param Row       SQL result row
%  @param TableInfo Table schema information
%  @param Args      Converted Prolog arguments
%
map_sql_row_to_prolog_args(Row, TableInfo, Args) :-
    TableInfo = table_info(_Columns, Types),
    maplist(map_sql_value_to_prolog, Row, Types, Args).

%! map_sql_value_to_prolog(+SQLValue, +Type, -PrologValue) is det.
%
%  Convert individual SQL values to appropriate Prolog terms.
%
map_sql_value_to_prolog(SQLValue, Type, PrologValue) :-
    type_map_sql_to_prolog(SQLValue, Type, PrologValue).

% Debug support
:- debug(sql_bridge).

% Error handling
:- multifile user:exception/3.
user:exception(error(sql_error(Code, Message), _Context), _, _) :-
    format(user_error, 'SQL Error ~w: ~w~n', [Code, Message]),
    fail.

% Initialization
:- initialization(init_sql_bridge).

init_sql_bridge :-
    debug(sql_bridge, 'Initializing SQL Bridge', []),
    db_init_connection_manager().
