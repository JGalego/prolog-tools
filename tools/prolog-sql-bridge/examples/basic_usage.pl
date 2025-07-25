/**
 * Basic Usage Examples
 * 
 * This file demonstrates basic usage of the Prolog-SQL bridge.
 */

:- use_module('../src/sql_bridge').
:- use_module('../src/db_connection').

% Example 1: Basic Connection and Query (ODBC DSN)
example_basic_connection :-
    % Connect to a SQLite database using ODBC DSN
    sql_connect('DSN=employees;Driver=SQLite3', Connection),
    
    % Register all tables for querying
    sql_register_tables(Connection),
    
    % Query employees as Prolog facts
    findall(employee(ID, Name, Dept, Salary), 
            employee(ID, Name, Dept, Salary), 
            Employees),
    
    format('Found ~w employees:~n', [length(Employees)]),
    forall(member(employee(ID, Name, Dept, Salary), Employees),
           format('  ~w: ~w (~w) - $~w~n', [ID, Name, Dept, Salary])),
    
    % Disconnect
    sql_disconnect(Connection).

% Example 1b: Direct SQLite Connection (no DSN required)
example_direct_connection :-
    % Connect directly to SQLite database file
    db_connect('DRIVER=SQLite3;DATABASE=../examples/sample.db', Connection),
    
    % Execute direct SQL queries
    db_execute_query(Connection, 'SELECT id, name, department, salary FROM employee', Results),
    
    format('Found ~w employees:~n', [length(Results)]),
    forall(member(row(ID, Name, Dept, Salary), Results),
           format('  ~w: ~w (~w) - $~w~n', [ID, Name, Dept, Salary])),
    
    % Disconnect
    db_disconnect(Connection).

% Example 2: Filtered Queries
example_filtered_queries :-
    sql_connect('DSN=employees;Driver=SQLite3', Connection),
    sql_register_table(Connection, employee),
    
    % Find all employees in Engineering
    format('Engineering employees:~n'),
    forall(employee(ID, Name, 'Engineering', Salary),
           format('  ~w: ~w - $~w~n', [ID, Name, Salary])),
    
    % Find high-salary employees (this would need constraint support)
    format('High-salary employees:~n'),
    forall((employee(ID, Name, Dept, Salary), Salary > 75000),
           format('  ~w: ~w (~w) - $~w~n', [ID, Name, Dept, Salary])),
    
    sql_disconnect(Connection).

% Example 3: Multiple Tables
example_multiple_tables :-
    sql_connect('DSN=company;Driver=SQLite3', Connection),
    
    % Register multiple tables
    sql_register_table(Connection, employee),
    sql_register_table(Connection, department),
    sql_register_table(Connection, project),
    
    % Join-like queries (using Prolog)
    format('Employees with their department managers:~n'),
    forall((employee(EmpID, EmpName, DeptName, _),
            department(DeptName, Manager, _)),
           format('  ~w works in ~w (managed by ~w)~n', [EmpName, DeptName, Manager])),
    
    sql_disconnect(Connection).

% Example 4: Database Introspection
example_introspection :-
    sql_connect('DSN=sample;Driver=SQLite3', Connection),
    
    % Get database information
    sql_connection_info(Connection, Info),
    format('Database info: ~w~n', [Info]),
    
    % Get all tables
    schema_get_all_tables(Connection, Tables),
    format('Available tables: ~w~n', [Tables]),
    
    % Get table schema
    forall(member(Table, Tables),
           (schema_get_table_info(Connection, Table, TableInfo),
            format('Table ~w: ~w~n', [Table, TableInfo]))),
    
    sql_disconnect(Connection).

% Example 5: Error Handling
example_error_handling :-
    catch(
        (sql_connect('DSN=nonexistent', Connection),
         sql_register_tables(Connection)),
        error(sql_error(Code, Message), _),
        format('SQL Error ~w: ~w~n', [Code, Message])
    ).

% Example 6: Dynamic Facts
example_dynamic_facts :-
    sql_connect('DSN=employees;Driver=SQLite3', Connection),
    sql_register_table(Connection, employee),
    
    % Now employee/4 is available as a dynamic predicate
    
    % Count employees by department
    findall(Dept, employee(_, _, Dept, _), Departments),
    sort(Departments, UniqueDepts),
    forall(member(Dept, UniqueDepts),
           (findall(_, employee(_, _, Dept, _), EmpList),
            length(EmpList, Count),
            format('~w: ~w employees~n', [Dept, Count]))),
    
    % Average salary calculation
    findall(Salary, employee(_, _, _, Salary), Salaries),
    sum_list(Salaries, Total),
    length(Salaries, Count),
    Average is Total / Count,
    format('Average salary: $~2f~n', [Average]),
    
    sql_disconnect(Connection).

% Utility predicate for running all examples
run_all_examples :-
    format('=== Basic Connection Example ===~n'),
    catch(example_basic_connection, Error, 
          format('Error: ~w~n', [Error])),
    
    format('~n=== Filtered Queries Example ===~n'),
    catch(example_filtered_queries, Error,
          format('Error: ~w~n', [Error])),
    
    format('~n=== Multiple Tables Example ===~n'),
    catch(example_multiple_tables, Error,
          format('Error: ~w~n', [Error])),
    
    format('~n=== Introspection Example ===~n'),
    catch(example_introspection, Error,
          format('Error: ~w~n', [Error])),
    
    format('~n=== Error Handling Example ===~n'),
    catch(example_error_handling, Error,
          format('Error: ~w~n', [Error])),
    
    format('~n=== Dynamic Facts Example ===~n'),
    catch(example_dynamic_facts, Error,
          format('Error: ~w~n', [Error])).

% Instructions for use:
%
% 1. Ensure you have ODBC configured with appropriate DSNs
% 2. Load this file: ?- [examples/basic_usage].
% 3. Run individual examples: ?- example_basic_connection.
% 4. Or run all examples: ?- run_all_examples.
