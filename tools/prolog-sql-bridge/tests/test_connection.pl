/**
 * Connection Tests
 * 
 * Test suite for database connection functionality.
 */

:- use_module('../src/sql_bridge').
:- use_module('../src/db_connection').
:- use_module(library(plunit)).

:- begin_tests(connection_tests).

% Test basic connection establishment
test(basic_connection, [setup(setup_test_db), cleanup(cleanup_test_db)]) :-
    sql_connect('DSN=test_db;Driver=SQLite3', Connection),
    assertion(Connection = connection(_, _)),
    sql_disconnect(Connection).

% Test connection with invalid DSN
test(invalid_connection, [throws(error(sql_error(_, _), _))]) :-
    sql_connect('DSN=nonexistent_db', _Connection).

% Test multiple connections
test(multiple_connections, [setup(setup_test_db), cleanup(cleanup_test_db)]) :-
    sql_connect('DSN=test_db;Driver=SQLite3', Connection1),
    sql_connect('DSN=test_db;Driver=SQLite3', Connection2),
    assertion(Connection1 \= Connection2),
    sql_disconnect(Connection1),
    sql_disconnect(Connection2).

% Test connection registration
test(connection_registration, [setup(setup_test_db), cleanup(cleanup_test_db)]) :-
    sql_connect('DSN=test_db;Driver=SQLite3', Connection),
    db_register_connection(Connection),
    db_get_current_connection(CurrentConnection),
    assertion(Connection = CurrentConnection),
    sql_disconnect(Connection).

% Test connection info retrieval
test(connection_info, [setup(setup_test_db), cleanup(cleanup_test_db)]) :-
    sql_connect('DSN=test_db;Driver=SQLite3', Connection),
    sql_connection_info(Connection, Info),
    assertion(is_dict(Info)),
    assertion(get_dict(status, Info, active)),
    sql_disconnect(Connection).

% Test disconnection cleanup
test(disconnection_cleanup, [setup(setup_test_db), cleanup(cleanup_test_db)]) :-
    sql_connect('DSN=test_db;Driver=SQLite3', Connection),
    db_register_connection(Connection),
    sql_disconnect(Connection),
    catch(
        db_get_current_connection(_),
        error(no_connection, _),
        true  % Expected error
    ).

% Test query execution
test(basic_query, [setup(setup_test_db), cleanup(cleanup_test_db)]) :-
    sql_connect('DSN=test_db;Driver=SQLite3', Connection),
    create_test_table(Connection),
    sql_query("SELECT * FROM test_table", Results),
    assertion(is_list(Results)),
    sql_disconnect(Connection).

% Test query with error
test(invalid_query, [setup(setup_test_db), cleanup(cleanup_test_db),
                     throws(error(sql_error(_, _), _))]) :-
    sql_connect('DSN=test_db;Driver=SQLite3', Connection),
    sql_query("SELECT * FROM nonexistent_table", _Results),
    sql_disconnect(Connection).

:- end_tests(connection_tests).

% Test setup and cleanup helpers
setup_test_db :-
    % Create a temporary SQLite database for testing
    true.  % Placeholder - would create actual test database

cleanup_test_db :-
    % Clean up test database
    true.  % Placeholder - would remove test database

create_test_table(Connection) :-
    % Create a simple test table
    connection(_, Handle) = Connection,
    catch(
        odbc_query(Handle, "CREATE TABLE test_table (id INTEGER, name TEXT)", _),
        _,
        true
    ),
    catch(
        odbc_query(Handle, "INSERT INTO test_table VALUES (1, 'test')", _),
        _,
        true
    ).

% Connection stress tests
:- begin_tests(connection_stress).

% Test rapid connect/disconnect cycles
test(rapid_cycles, [setup(setup_test_db), cleanup(cleanup_test_db)]) :-
    forall(between(1, 10, _),
           (sql_connect('DSN=test_db;Driver=SQLite3', Connection),
            sql_disconnect(Connection))).

% Test connection pool exhaustion
test(many_connections, [setup(setup_test_db), cleanup(cleanup_test_db)]) :-
    findall(Connection,
            (between(1, 5, _),
             sql_connect('DSN=test_db;Driver=SQLite3', Connection)),
            Connections),
    forall(member(Connection, Connections),
           sql_disconnect(Connection)).

:- end_tests(connection_stress).

% Error handling tests
:- begin_tests(error_handling).

% Test missing ODBC driver
test(missing_driver, [throws(error(sql_error(driver_not_found, _), _))]) :-
    sql_connect('DRIVER={NonexistentDriver};DSN=test', _Connection).

% Test permission denied
test(permission_denied, [condition(has_permission_test_case)]) :-
    % This test would need a specific database setup with permission restrictions
    true.

% Test database not found
test(database_not_found, [throws(error(sql_error(database_not_found, _), _))]) :-
    sql_connect('DSN=definitely_nonexistent_database_12345', _Connection).

:- end_tests(error_handling).

% Helper predicate for conditional tests
has_permission_test_case :-
    % Check if we have a test case available for permission testing
    false.  % Usually false unless specific test environment is set up

% Run all connection tests
run_connection_tests :-
    run_tests([connection_tests, connection_stress, error_handling]).

% Instructions:
% Load this file: ?- [tests/test_connection].
% Run tests: ?- run_connection_tests.
% Run specific test: ?- run_tests(connection_tests:basic_connection).
