# API Reference

This document provides a comprehensive reference for the Prolog-SQL Bridge API.

## Core Module: sql_bridge

### Connection Management

#### `sql_connect(+ConnectionString, -Connection)`
Establish a connection to a SQL database using ODBC.

**Parameters:**
- `ConnectionString`: ODBC connection string (e.g., `'DSN=mydb;UID=user;PWD=pass'`)
- `Connection`: Connection handle for subsequent operations

**Example:**
```prolog
?- sql_connect('DSN=employees;UID=admin;PWD=secret', Conn).
Conn = connection(1, <odbc_handle>).
```

**Throws:**
- `error(sql_error(driver_not_found, Message), _)` - ODBC driver not found
- `error(sql_error(permission_denied, Message), _)` - Access denied
- `error(sql_error(database_not_found, Message), _)` - Database not found

**Note:** The connection manager initializes automatically on first use.

#### `sql_disconnect(+Connection)`
Close a database connection and clean up associated resources.

**Parameters:**
- `Connection`: Connection handle to close

**Example:**
```prolog
?- sql_disconnect(connection(1, Handle)).
```

#### `sql_connection_info(+Connection, -Info)`
Get information about a database connection.

**Parameters:**
- `Connection`: Connection handle
- `Info`: Dictionary containing connection information

**Example:**
```prolog
?- sql_connection_info(Conn, Info).
Info = _{connection_string:"DSN=test", connected_at:1642581234.5, status:active}.
```

## Low-Level Module: db_connection

For direct database access without the SQL bridge layer:

#### `db_connect(+ConnectionString, -Connection)`
Low-level database connection. Connection manager initializes automatically.

**Parameters:**
- `ConnectionString`: ODBC connection string, including direct driver connections
- `Connection`: Connection identifier

**Example:**
```prolog
% Direct SQLite connection (no DSN required)
?- db_connect('DRIVER=SQLite3;DATABASE=/path/to/db.sqlite', Conn).

% Traditional ODBC DSN connection
?- db_connect('DSN=mydb;UID=user;PWD=pass', Conn).
```

#### `db_execute_query(+Connection, +Query, -Results)`
Execute a SQL query directly.

**Parameters:**
- `Connection`: Connection identifier from `db_connect/2`
- `Query`: SQL query string
- `Results`: List of result rows

**Example:**
```prolog
?- db_execute_query(Conn, 'SELECT * FROM employee LIMIT 5', Results).
Results = [row(1,'Alice Johnson','Engineering',120000.0), ...].
```

### Table Registration

#### `sql_register_table(+Connection, +TableName)`
Register a single table for Prolog querying.

**Parameters:**
- `Connection`: Connection handle
- `TableName`: Name of the table to register

**Example:**
```prolog
?- sql_register_table(Conn, employee).
true.

% Now you can query employee/4 as a Prolog predicate
?- employee(ID, Name, Department, Salary).
```

#### `sql_register_tables(+Connection)`
Register all tables in the connected database for Prolog querying.

**Parameters:**
- `Connection`: Connection handle

**Example:**
```prolog
?- sql_register_tables(Conn).
% All tables are now available as Prolog predicates
```

### Querying

#### `sql_query(+Query, -Results)`
Execute a direct SQL query and return results.

**Parameters:**
- `Query`: SQL query string
- `Results`: List of result rows

**Example:**
```prolog
?- sql_query("SELECT name, salary FROM employee WHERE salary > 50000", Results).
Results = [row('John Doe', 75000), row('Jane Smith', 65000)].
```

#### `sql_fact(?Fact)`
Query registered tables as Prolog facts.

**Parameters:**
- `Fact`: Prolog term representing a database row

**Example:**
```prolog
?- sql_fact(employee(_, Name, 'Engineering', _)).
Name = 'John Doe' ;
Name = 'Alice Johnson' ;
false.
```

### Cleanup

#### `sql_retract_all`
Remove all dynamically created predicates for SQL tables.

**Example:**
```prolog
?- sql_retract_all.
true.
% All dynamic SQL predicates are now removed
```

## Database Connection Module: db_connection

### Low-level Connection Management

#### `db_connect(+ConnectionString, -Connection)`
Low-level database connection establishment.

#### `db_execute_query(+Connection, +Query, -Results)`
Execute a SQL query directly.

#### `db_get_current_connection(-Connection)`
Get the current default connection.

**Throws:**
- `error(no_connection, _)` if no connection is available

## Query Translator Module: query_translator

### Query Translation

#### `query_translate_prolog_to_sql(+PrologQuery, +TableInfo, -SQLQuery)`
Translate a Prolog query pattern to SQL.

**Parameters:**
- `PrologQuery`: Prolog term representing the query
- `TableInfo`: Schema information for the table
- `SQLQuery`: Generated SQL query string

**Example:**
```prolog
?- query_translate_prolog_to_sql(
     employee(_, Name, 'Engineering', >(50000)),
     table_info([id, name, dept, salary], ['INT', 'VARCHAR', 'VARCHAR', 'DECIMAL']),
     SQL
   ).
SQL = "SELECT * FROM employee WHERE dept = 'Engineering' AND salary > 50000".
```

#### `query_optimize_sql(+RawSQL, -OptimizedSQL)`
Optimize a SQL query for better performance.

#### `query_estimate_selectivity(+Query, +TableInfo, -Selectivity)`
Estimate the selectivity of a query (0.0 to 1.0).

## Schema Introspector Module: schema_introspector

### Schema Discovery

#### `schema_get_all_tables(+Connection, -Tables)`
Get a list of all table names in the database.

**Example:**
```prolog
?- schema_get_all_tables(Conn, Tables).
Tables = [employee, department, project].
```

#### `schema_get_table_info(+Connection, +TableName, -TableInfo)`
Get comprehensive information about a table.

**Parameters:**
- `Connection`: Connection handle
- `TableName`: Name of the table
- `TableInfo`: `table_info(Columns, Types, Constraints)` structure

**Example:**
```prolog
?- schema_get_table_info(Conn, employee, Info).
Info = table_info([id, name, dept, salary], 
                  ['INTEGER', 'VARCHAR', 'VARCHAR', 'DECIMAL'],
                  [primary_key(id)]).
```

#### `schema_get_column_info(+Connection, +TableName, +ColumnName, -ColumnInfo)`
Get detailed information about a specific column.

#### `schema_get_primary_keys(+Connection, +TableName, -PrimaryKeys)`
Get the primary key columns for a table.

#### `schema_get_foreign_keys(+Connection, +TableName, -ForeignKeys)`
Get the foreign key relationships for a table.

#### `schema_get_indexes(+Connection, +TableName, -Indexes)`
Get index information for a table.

#### `schema_get_database_info(+Connection, -DatabaseInfo)`
Get general information about the database.

### Schema Caching

#### `schema_cache_table(+Connection, +TableName, +TableInfo)`
Cache table information for faster access.

#### `schema_cache_clear`
Clear all cached schema information.

## Type Mapper Module: type_mapper

### Type Conversion

#### `type_map_sql_to_prolog(+SQLValue, +SQLType, -PrologValue)`
Convert a SQL value to Prolog representation.

**Example:**
```prolog
?- type_map_sql_to_prolog('42', 'INTEGER', Value).
Value = 42.

?- type_map_sql_to_prolog('Hello', 'VARCHAR', Value).
Value = "Hello".
```

#### `type_map_prolog_to_sql(+PrologValue, +SQLType, -SQLValue)`
Convert a Prolog value to SQL representation.

**Example:**
```prolog
?- type_map_prolog_to_sql(42, 'INTEGER', SQL).
SQL = '42'.

?- type_map_prolog_to_sql("Hello", 'VARCHAR', SQL).
SQL = "'Hello'".
```

#### `type_map_sql_type_to_prolog_type(+SQLType, +IsNullable, -PrologType)`
Map SQL type to Prolog type descriptor.

#### `type_get_sql_literal(+Value, -SQLLiteral)`
Convert any Prolog value to SQL literal representation.

### Type Predicates

#### `type_is_numeric(+Type)`
Check if a type is numeric.

#### `type_is_string(+Type)`
Check if a type is string-based.

#### `type_is_temporal(+Type)`
Check if a type is temporal (date/time).

#### `type_compatible(+Type1, +Type2)`
Check if two types are compatible for operations.

## Constraint Expressions

The bridge supports constraint expressions in Prolog queries that are translated to SQL WHERE clauses:

### Supported Operators

- `>(Value)` → `> Value`
- `<(Value)` → `< Value`
- `>=(Value)` → `>= Value`
- `=<(Value)` → `<= Value`
- `=:=(Value)` → `= Value`
- `=\=(Value)` → `<> Value`

### Examples

```prolog
% Find employees with salary greater than 50000
?- employee(_, _, _, >(50000)).

% Find employees with salary between 40000 and 80000
?- employee(_, _, _, >(40000)), employee(_, _, _, <(80000)).

% Find employees not in Engineering
?- employee(_, _, =\=('Engineering'), _).
```

## Error Handling

The bridge uses structured error terms:

### SQL Errors
- `error(sql_error(Code, Message), Context)`

### Common Error Codes
- `driver_not_found` - ODBC driver not available
- `permission_denied` - Access denied to database
- `database_not_found` - Database does not exist
- `connection_failed` - General connection failure
- `syntax_error` - SQL syntax error
- `table_not_found` - Table does not exist
- `query_failed` - General query execution failure

### Connection Errors
- `error(no_connection, Message)` - No active connection available

## Performance Considerations

### Connection Pooling
- Connections are automatically managed and pooled
- Use `sql_register_tables/1` for better performance when querying multiple tables

### Schema Caching
- Table schema information is cached automatically
- Use `schema_cache_clear/0` to refresh cached information

### Query Optimization
- The bridge attempts to optimize generated SQL queries
- Use direct `sql_query/2` for complex queries that don't translate well

### Memory Management
- Use `sql_retract_all/0` to clean up dynamic predicates when done
- Disconnect connections when no longer needed

## Debugging

Enable debug output:
```prolog
?- debug(sql_bridge).
?- debug(db_connection).
?- debug(query_translator).
?- debug(schema_introspector).
?- debug(type_mapper).
```

Disable debug output:
```prolog
?- nodebug(sql_bridge).
```
