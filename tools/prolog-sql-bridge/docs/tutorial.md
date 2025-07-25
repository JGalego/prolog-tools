# Prolog-SQL Bridge Tutorial

This tutorial will guide you through using the Prolog-SQL Bridge to query SQL databases as if they were Prolog facts.

## Table of Contents

1. [Setup and Installation](#setup-and-installation)
2. [Basic Usage](#basic-usage)
3. [Working with Data Types](#working-with-data-types)
4. [Advanced Querying](#advanced-querying)
5. [Schema Introspection](#schema-introspection)
6. [Performance Tips](#performance-tips)
7. [Troubleshooting](#troubleshooting)

## Setup and Installation

### Prerequisites

1. **SWI-Prolog with ODBC support**:
   ```bash
   sudo apt-get install swi-prolog swi-prolog-odbc
   ```

2. **Database and ODBC driver**:
   - For SQLite: `sudo apt-get install libsqlite3-dev unixodbc-dev`
   - For PostgreSQL: `sudo apt-get install odbc-postgresql`
   - For MySQL: `sudo apt-get install libmyodbc`

3. **Configure ODBC DSN**:
   Edit `/etc/odbc.ini` or `~/.odbc.ini`:
   ```ini
   [employees]
   Description = Employee Database
   Driver = SQLite3
   Database = /path/to/employees.db
   ```

### Loading the Library

```prolog
?- use_module(library(sql_bridge)).
```

You can test if the library loads correctly by running:
```bash
cd /path/to/prolog-sql-tools
swipl -g "consult(src/sql_bridge), write('SQL Bridge loaded successfully'), nl, halt"
```

### Quick Test with Sample Data

To quickly test the bridge with sample data:

1. **Create the sample database**:
   ```bash
   # Install SQLite if not already installed
   sudo apt-get install sqlite3
   
   # Create sample database from the provided SQL file
   cd /path/to/prolog-sql-tools
   sqlite3 examples/sample.db < examples/sample_data.sql
   ```

2. **Test with direct SQLite connection** (no ODBC setup required):
   ```bash
   swipl -g "working_directory(_, 'src'), use_module(db_connection), catch((db_connect('DRIVER=SQLite3;DATABASE=/full/path/to/prolog-sql-tools/examples/sample.db', Conn), write('DB connect successful: '), write(Conn), nl), Error, (write('Error: '), write(Error), nl)), halt"
   ```

3. **Or configure ODBC for the sample database**:
   Add to your `~/.odbc.ini` file:
   ```ini
   [sample_db]
   Description = Sample Employee Database
   Driver = SQLite3
   Database = /full/path/to/prolog-sql-tools/examples/sample.db
   ```

4. **Test the bridge**:
   ```bash
   swipl
   ```
   
   Then in Prolog:
   ```prolog
   % Load the SQL bridge
   ?- working_directory(_, '/path/to/prolog-sql-tools/src').
   ?- use_module(sql_bridge).
   
   % Connect to sample database (using ODBC DSN)
   ?- sql_connect('DSN=sample_db', Connection).
   
   % Or connect directly (no DSN required)
   ?- use_module(db_connection).
   ?- db_connect('DRIVER=SQLite3;DATABASE=/full/path/to/sample.db', Connection).
   
   % Register all tables
   ?- sql_register_tables(Connection).
   
   % Test basic queries
   ?- employee(ID, Name, Department, Salary).
   ID = 1, Name = 'Alice Johnson', Department = 'Engineering', Salary = 120000.0 ;
   ID = 2, Name = 'Bob Smith', Department = 'Marketing', Salary = 95000.0 ;
   % ... more results
   
   % Test constraint queries
   ?- employee(_, Name, 'Engineering', _).
   Name = 'Alice Johnson' ;
   Name = 'Frank Miller' ;
   Name = 'Grace Lee' ;
   % ... more results
   
   % Test cross-table queries
   ?- employee(_, Name, Dept, _), department(Dept, Manager, _).
   Name = 'Alice Johnson', Dept = 'Engineering', Manager = 'Alice Johnson' ;
   Name = 'Bob Smith', Dept = 'Marketing', Manager = 'Bob Smith' ;
   % ... more results
   
   % Clean up
   ?- sql_disconnect(Connection).
   ```

4. **Or use the automated test**:
   ```bash
   # Run the provided examples
   make setup-sample-db
   make run-examples
   ```

The sample database contains:
- **23 employees** across 5 departments with realistic salary data
- **5 departments** with managers and budgets
- **10 projects** with different budgets and timelines
- **Employee-project assignments** showing who works on what
- **Manager-employee relationships** for hierarchical queries

This gives you a rich dataset to experiment with all features of the Prolog-SQL bridge!

## Basic Usage

### Step 1: Connect to Database

```prolog
?- sql_connect('DSN=employees', Connection).
Connection = connection(1, <odbc_handle>).
```

### Step 2: Register Tables

Register a single table:
```prolog
?- sql_register_table(Connection, employee).
true.
```

Or register all tables at once:
```prolog
?- sql_register_tables(Connection).
true.
```

### Step 3: Query as Prolog Facts

Now you can query the registered tables as if they were Prolog predicates:

```prolog
% Get all employees
?- employee(ID, Name, Department, Salary).
ID = 1, Name = 'John Doe', Department = 'Engineering', Salary = 75000 ;
ID = 2, Name = 'Jane Smith', Department = 'Marketing', Salary = 65000 ;
ID = 3, Name = 'Bob Johnson', Department = 'Engineering', Salary = 82000.

% Find specific employees
?- employee(_, 'John Doe', Department, Salary).
Department = 'Engineering', Salary = 75000.

% Find employees in specific department
?- employee(ID, Name, 'Engineering', _).
ID = 1, Name = 'John Doe' ;
ID = 3, Name = 'Bob Johnson'.
```

### Step 4: Clean Up

```prolog
?- sql_disconnect(Connection).
true.
```

## Working with Data Types

The bridge automatically handles type conversions between SQL and Prolog:

### Numeric Types

```prolog
% SQL INTEGER, FLOAT, DECIMAL become Prolog numbers
?- employee(1, _, _, Salary), Salary > 70000.
Salary = 75000.

% Arithmetic operations work naturally
?- employee(_, _, _, Salary), TaxableIncome is Salary * 0.8.
```

### String Types

```prolog
% SQL VARCHAR, CHAR, TEXT become Prolog strings/atoms
?- employee(_, Name, _, _), atom_string(Name, NameStr).

% Pattern matching works
?- employee(_, Name, _, _), atom_concat('John', _, Name).
Name = 'John Doe'.
```

### Null Values

```prolog
% SQL NULL becomes Prolog null
?- employee(_, Name, _, null).  % Find employees with no salary data
```

### Date/Time Types

```prolog
% SQL DATE, DATETIME become Prolog strings (for now)
?- employee_with_dates(_, _, _, HireDate), HireDate = '2023-01-15'.
```

## Advanced Querying

### Constraint Expressions

Use constraint expressions for more complex WHERE clauses:

```prolog
% Employees with salary greater than 70000
?- employee(_, _, _, >(70000)).

% Employees with salary between 60000 and 80000
?- employee(ID, Name, Dept, Salary), 
   Salary > 60000, 
   Salary < 80000.

% Using constraint expressions directly
?- employee(_, _, _, >(60000)), employee(_, _, _, <(80000)).
```

### Multiple Table Queries

Query across multiple tables using Prolog's join capabilities:

```prolog
% Register multiple tables
?- sql_register_table(Connection, employee).
?- sql_register_table(Connection, department).

% Find employees with their department budgets
?- employee(_, Name, DeptName, _),
   department(DeptName, Manager, Budget),
   Budget > 1000000.

% Complex joins
?- employee(EmpID, EmpName, DeptName, _),
   department(DeptName, Manager, _),
   employee(MgrID, Manager, _, _),
   format('~w reports to ~w~n', [EmpName, Manager]).
```

### Aggregation and Analysis

Use Prolog's built-in predicates for aggregation:

```prolog
% Count employees by department
?- findall(Dept, employee(_, _, Dept, _), Depts),
   msort(Depts, SortedDepts),
   clumped(SortedDepts, Counts),
   member(Dept-Count, Counts),
   format('~w: ~w employees~n', [Dept, Count]),
   fail.

% Calculate average salary
?- findall(Salary, employee(_, _, _, Salary), Salaries),
   sum_list(Salaries, Total),
   length(Salaries, Count),
   Average is Total / Count,
   format('Average salary: $~2f~n', [Average]).

% Find highest paid employee
?- findall(Salary-Name, employee(_, Name, _, Salary), Pairs),
   max_member(MaxSalary-TopEarner, Pairs),
   format('Highest paid: ~w ($~w)~n', [TopEarner, MaxSalary]).
```

### Direct SQL Queries

For complex queries that don't translate well to Prolog:

```prolog
?- sql_query("SELECT department, AVG(salary) as avg_sal 
              FROM employee 
              GROUP BY department 
              HAVING AVG(salary) > 70000", Results).
Results = [row('Engineering', 78500), row('Sales', 72000)].
```

## Schema Introspection

### Discovering Database Structure

```prolog
% Get all tables
?- schema_get_all_tables(Connection, Tables).
Tables = [employee, department, project].

% Get table schema
?- schema_get_table_info(Connection, employee, TableInfo).
TableInfo = table_info([id, name, department, salary],
                      ['INTEGER', 'VARCHAR', 'VARCHAR', 'DECIMAL'],
                      [primary_key(id)]).

% Get database information
?- schema_get_database_info(Connection, DbInfo).
DbInfo = database_info{name: employees, 
                      dbms_name: 'SQLite', 
                      tables: [employee, department]}.
```

### Working with Constraints

```prolog
% Find primary keys
?- schema_get_primary_keys(Connection, employee, PKs).
PKs = [id].

% Find foreign keys
?- schema_get_foreign_keys(Connection, employee, FKs).
FKs = [foreign_key(department_id, department, id)].
```

## Performance Tips

### 1. Register Tables Once

```prolog
% Good: Register once, query many times
?- sql_register_tables(Connection).
?- employee(_, _, 'Engineering', _).  % Fast
?- employee(_, _, 'Sales', _).        % Fast

% Avoid: Re-registering for each query
```

### 2. Use Constraint Expressions

```prolog
% Better: Use constraints to limit SQL result set
?- employee(_, _, _, >(70000)).

% Slower: Get all records then filter in Prolog
?- employee(_, _, _, Salary), Salary > 70000.
```

### 3. Cache Schema Information

```prolog
% Schema information is cached automatically
% Clear cache only when schema changes
?- schema_cache_clear.
```

### 4. Use Direct SQL for Complex Queries

```prolog
% For complex aggregations, use direct SQL
?- sql_query("SELECT dept, COUNT(*), AVG(salary) 
              FROM employee 
              GROUP BY dept", Results).
```

### 5. Clean Up When Done

```prolog
% Remove dynamic predicates to free memory
?- sql_retract_all.
?- sql_disconnect(Connection).
```

## Troubleshooting

### Common Issues

#### 1. Connection Problems

```prolog
% Enable debugging
?- debug(sql_bridge).

% Check ODBC configuration
?- sql_connect('DSN=test', Connection).
ERROR: sql_error(driver_not_found, 'ODBC driver not found: test')

% Solution: Check /etc/odbc.ini and driver installation
```

#### 2. Table Not Found

```prolog
?- employee(_, _, _, _).
ERROR: existence_error(procedure, employee/4, _)

% Solution: Register the table first
?- sql_register_table(Connection, employee).
```

#### 3. Type Conversion Errors

```prolog
% Check table schema
?- schema_get_table_info(Connection, employee, Info).

% Verify data types
?- sql_query("SELECT * FROM employee LIMIT 1", [Row|_]),
   write(Row).
```

#### 4. Performance Issues

```prolog
% Enable query debugging
?- debug(query_translator).

% Check generated SQL
?- employee(_, _, 'Engineering', _).
% Debug output shows: SELECT * FROM employee WHERE department = 'Engineering'
```

### Error Codes

- `driver_not_found`: Install ODBC driver
- `permission_denied`: Check database permissions
- `database_not_found`: Verify database path/name
- `table_not_found`: Check table name and register it
- `syntax_error`: Check generated SQL query

### Getting Help

1. **Enable debugging**:
   ```prolog
   ?- debug(sql_bridge).
   ?- debug(db_connection).
   ?- debug(query_translator).
   ```

2. **Check connection info**:
   ```prolog
   ?- sql_connection_info(Connection, Info).
   ```

3. **Verify table registration**:
   ```prolog
   ?- current_predicate(employee/4).
   ```

4. **Test with direct SQL**:
   ```prolog
   ?- sql_query("SELECT COUNT(*) FROM employee", Result).
   ```

## Complete Example

Here's a complete example that demonstrates most features:

```prolog
% Load the library
?- use_module(library(sql_bridge)).

% Connect to database
?- sql_connect('DSN=company', Connection).

% Register all tables
?- sql_register_tables(Connection).

% Basic queries
?- employee(_, Name, 'Engineering', _).
Name = 'Alice Johnson' ;
Name = 'Bob Smith'.

% Complex analysis
analyze_departments :-
    findall(Dept-Salaries,
            (department(Dept, _, _),
             findall(S, employee(_, _, Dept, S), Salaries),
             Salaries \= []),
            DeptSalaries),
    
    forall(member(Dept-Salaries, DeptSalaries),
           (sum_list(Salaries, Total),
            length(Salaries, Count),
            Average is Total / Count,
            format('~w: ~w employees, avg salary $~2f~n', 
                   [Dept, Count, Average]))).

% Run the analysis
?- analyze_departments.
Engineering: 5 employees, avg salary $82000.00

## Common Issues and Solutions

### Connection Issues

**Problem**: `ERROR: ... false` when trying to connect
**Solution**: The connection manager may not be initialized. This is now handled automatically, but if you see this error:
1. Make sure you're using the latest version of the db_connection module
2. Try using `db_connect/2` directly instead of `sql_connect/2`
3. Use absolute paths in connection strings

**Example**:
```prolog
% Instead of relative paths
db_connect('DRIVER=SQLite3;DATABASE=../sample.db', Conn).

% Use absolute paths
db_connect('DRIVER=SQLite3;DATABASE=/full/path/to/sample.db', Conn).
```

**Problem**: ODBC driver not found
**Solution**: Install the appropriate ODBC driver:
```bash
# For SQLite
sudo apt-get install libsqliteodbc

# For PostgreSQL  
sudo apt-get install odbc-postgresql

# For MySQL
sudo apt-get install libmyodbc
```

### DSN Configuration Issues

**Problem**: DSN not found errors
**Solution**: Use direct driver connections instead of DSN:
```prolog
% Instead of DSN
db_connect('DSN=mydb', Conn).

% Use direct driver connection
db_connect('DRIVER=SQLite3;DATABASE=/path/to/file.db', Conn).
```

### Query Execution Issues

**Problem**: Singleton variable warnings
**Solution**: These warnings are harmless but can be avoided by using anonymous variables:
```prolog
% Instead of unused variables
employee(ID, Name, Department, Salary).

% Use anonymous variables for unused parameters
employee(_, Name, _, _).
```

**Problem**: Connection handle errors
**Solution**: Always disconnect properly and handle errors:
```prolog
safe_query(Query, Results) :-
    catch(
        (
            db_connect('DRIVER=SQLite3;DATABASE=/path/to/db', Conn),
            db_execute_query(Conn, Query, Results),
            db_disconnect(Conn)
        ),
        Error,
        (
            format('Query failed: ~w~n', [Error]),
            fail
        )
    ).
```
Marketing: 3 employees, avg salary $61666.67
Sales: 4 employees, avg salary $68500.00

% Clean up
?- sql_disconnect(Connection).
```

This tutorial should get you started with the Prolog-SQL Bridge. For more advanced features and detailed API documentation, see the [API Reference](api_reference.md).
