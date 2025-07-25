/**
 * Database Connection Manager
 * 
 * This module handles database connections, connection pooling,
 * and low-level database operations.
 */

:- module(db_connection, [
    db_connect/2,              % db_connect(+ConnectionString, -Connection)
    db_disconnect/1,           % db_disconnect(+Connection)
    db_execute_query/3,        % db_execute_query(+Connection, +Query, -Results)
    db_register_connection/1,  % db_register_connection(+Connection)
    db_unregister_connection/1,% db_unregister_connection(+Connection)
    db_get_current_connection/1, % db_get_current_connection(-Connection)
    db_get_connection_info/2,  % db_get_connection_info(+Connection, -Info)
    db_cleanup_dynamic_facts/1, % db_cleanup_dynamic_facts(+Connection)
    db_cleanup_all_dynamic_facts/0, % db_cleanup_all_dynamic_facts
    db_init_connection_manager/0 % db_init_connection_manager
]).

:- use_module(library(odbc)).
:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(thread)).

% Dynamic predicates for connection management
:- dynamic active_connection/2.    % active_connection(ConnectionId, Handle)
:- dynamic connection_info/2.      % connection_info(ConnectionId, InfoDict)
:- dynamic connection_counter/1.   % connection_counter(Counter)
:- dynamic current_connection/1.   % current_connection(ConnectionId)
:- dynamic dynamic_predicate/2.    % dynamic_predicate(ConnectionId, Functor/Arity)

%! db_init_connection_manager is det.
%
%  Initialize the connection manager.
%
db_init_connection_manager :-
    retractall(connection_counter(_)),
    assert(connection_counter(0)),
    debug(db_connection, 'Connection manager initialized', []).

%! db_connect(+ConnectionString, -Connection) is det.
%
%  Establish a database connection using ODBC.
%
%  @param ConnectionString ODBC connection string
%  @param Connection       Connection identifier
%
%  @throws error(sql_error(Code, Message), Context) if connection fails
%
db_connect(ConnectionString, Connection) :-
    % Ensure connection manager is initialized
    (   connection_counter(_)
    ->  true
    ;   db_init_connection_manager
    ),
    
    debug(db_connection, 'Attempting connection with: ~w', [ConnectionString]),
    
    % Try odbc_connect first, then fallback to odbc_driver_connect
    (   catch(
            (odbc_connect(ConnectionString, Handle, []),
             debug(db_connection, 'Connected using odbc_connect', [])),
            _Error1,
            fail
        )
    ;   % Fallback to odbc_driver_connect for direct driver connections
        debug(db_connection, 'Trying odbc_driver_connect as fallback', []),
        catch(
            (odbc_driver_connect(ConnectionString, Handle, []),
             debug(db_connection, 'Connected using odbc_driver_connect', [])),
            Error2,
            handle_connection_error(Error2)
        )
    ),
    
    % Generate unique connection identifier
    next_connection_id(ConnectionId),
    Connection = connection(ConnectionId, Handle),
    
    % Store connection information
    get_time(Timestamp),
    ConnectionInfo = _{
        connection_string: ConnectionString,
        connected_at: Timestamp,
        handle: Handle,
        status: active
    },
    
    assert(active_connection(ConnectionId, Handle)),
    assert(connection_info(ConnectionId, ConnectionInfo)),
    
    debug(db_connection, 'Connected successfully: ~w', [Connection]).

%! db_disconnect(+Connection) is det.
%
%  Close a database connection.
%
%  @param Connection Connection identifier
%
db_disconnect(connection(ConnectionId, Handle)) :-
    debug(db_connection, 'Disconnecting: ~w', [ConnectionId]),
    
    catch(
        odbc_disconnect(Handle),
        Error,
        (debug(db_connection, 'Disconnect error: ~w', [Error]), true)
    ),
    
    retract(active_connection(ConnectionId, Handle)),
    retract(connection_info(ConnectionId, _)),
    
    % Remove from current connection if it's the current one
    (   retract(current_connection(ConnectionId))
    ->  true
    ;   true
    ),
    
    debug(db_connection, 'Disconnected: ~w', [ConnectionId]).

%! db_execute_query(+Connection, +Query, -Results) is det.
%
%  Execute a SQL query and return results.
%
%  @param Connection Connection identifier
%  @param Query      SQL query string
%  @param Results    List of result rows
%
db_execute_query(connection(ConnectionId, Handle), Query, Results) :-
    debug(db_connection, 'Executing query on ~w: ~w', [ConnectionId, Query]),
    
    catch(
        (
            odbc_query(Handle, Query, Row),
            findall(Row, odbc_query(Handle, Query, Row), Results)
        ),
        Error,
        handle_query_error(Error, Query)
    ),
    
    length(Results, Count),
    debug(db_connection, 'Query returned ~w rows', [Count]).

%! db_register_connection(+Connection) is det.
%
%  Register a connection as the current default connection.
%
db_register_connection(connection(ConnectionId, _Handle)) :-
    retractall(current_connection(_)),
    assert(current_connection(ConnectionId)),
    debug(db_connection, 'Registered as current connection: ~w', [ConnectionId]).

%! db_unregister_connection(+Connection) is det.
%
%  Unregister a connection.
%
db_unregister_connection(connection(ConnectionId, _Handle)) :-
    (   retract(current_connection(ConnectionId))
    ->  debug(db_connection, 'Unregistered current connection: ~w', [ConnectionId])
    ;   true
    ).

%! db_get_current_connection(-Connection) is det.
%
%  Get the current default connection.
%
%  @throws error(no_connection, _) if no connection is available
%
db_get_current_connection(Connection) :-
    current_connection(ConnectionId),
    active_connection(ConnectionId, Handle),
    Connection = connection(ConnectionId, Handle),
    !.
db_get_current_connection(_) :-
    throw(error(no_connection, 'No active database connection')).

%! db_get_connection_info(+Connection, -Info) is det.
%
%  Get information about a connection.
%
db_get_connection_info(connection(ConnectionId, _Handle), Info) :-
    connection_info(ConnectionId, Info).

%! db_cleanup_dynamic_facts(+Connection) is det.
%
%  Clean up dynamic predicates associated with a connection.
%
db_cleanup_dynamic_facts(connection(ConnectionId, _Handle)) :-
    debug(db_connection, 'Cleaning up dynamic facts for: ~w', [ConnectionId]),
    forall(
        dynamic_predicate(ConnectionId, Functor/Arity),
        (
            functor(Template, Functor, Arity),
            retractall(Template),
            abolish(Functor/Arity)
        )
    ),
    retractall(dynamic_predicate(ConnectionId, _)).

%! db_cleanup_all_dynamic_facts is det.
%
%  Clean up all dynamic predicates.
%
db_cleanup_all_dynamic_facts :-
    debug(db_connection, 'Cleaning up all dynamic facts', []),
    forall(
        dynamic_predicate(_, Functor/Arity),
        (
            functor(Template, Functor, Arity),
            retractall(Template),
            catch(abolish(Functor/Arity), _, true)
        )
    ),
    retractall(dynamic_predicate(_, _)).

%! next_connection_id(-ConnectionId) is det.
%
%  Generate the next unique connection identifier.
%
next_connection_id(ConnectionId) :-
    with_mutex(connection_counter, (
        retract(connection_counter(Current)),
        Next is Current + 1,
        assert(connection_counter(Next)),
        ConnectionId = Next
    )).

%! handle_connection_error(+Error) is det.
%
%  Handle connection errors with appropriate error messages.
%
handle_connection_error(error(existence_error(odbc_driver, Driver), _)) :-
    !,
    format(string(Msg), 'ODBC driver not found: ~w', [Driver]),
    throw(error(sql_error(driver_not_found, Msg), _)).

handle_connection_error(error(permission_error(connect, database, _), _)) :-
    !,
    throw(error(sql_error(permission_denied, 'Access denied to database'), _)).

handle_connection_error(error(existence_error(database, DB), _)) :-
    !,
    format(string(Msg), 'Database not found: ~w', [DB]),
    throw(error(sql_error(database_not_found, Msg), _)).

handle_connection_error(Error) :-
    format(string(Msg), 'Connection failed: ~w', [Error]),
    throw(error(sql_error(connection_failed, Msg), _)).

%! handle_query_error(+Error, +Query) is det.
%
%  Handle query execution errors.
%
handle_query_error(error(syntax_error(_), _), Query) :-
    !,
    format(string(Msg), 'SQL syntax error in query: ~w', [Query]),
    throw(error(sql_error(syntax_error, Msg), _)).

handle_query_error(error(existence_error(table, Table), _), Query) :-
    !,
    format(string(Msg), 'Table not found: ~w in query: ~w', [Table, Query]),
    throw(error(sql_error(table_not_found, Msg), _)).

handle_query_error(Error, Query) :-
    format(string(Msg), 'Query execution failed: ~w for query: ~w', [Error, Query]),
    throw(error(sql_error(query_failed, Msg), _)).

% Debug support
:- debug(db_connection).
