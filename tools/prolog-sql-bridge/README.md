# Prolog-SQL Bridge

A Prolog library that allows querying SQL databases as if they were Prolog facts, providing seamless integration between Prolog and relational databases.

## Features

- **Dynamic Fact Generation**: Query SQL tables as Prolog predicates
- **ODBC Support**: Connect to various database systems (PostgreSQL, MySQL, SQLite, etc.)
- **Schema Introspection**: Automatically discover and map database schemas
- **Query Translation**: Convert Prolog queries to optimized SQL
- **Connection Pooling**: Efficient database connection management
- **Type Mapping**: Automatic conversion between SQL and Prolog data types

## Quick Start

```prolog
% Load the SQL bridge
?- use_module(library(sql_bridge)).

% Connect to a database (connection manager initializes automatically)
?- sql_connect('DSN=mydb;UID=user;PWD=pass', Connection).

% Query a table as if it were Prolog facts
?- employee(ID, Name, Department, Salary).
ID = 1, Name = 'John Doe', Department = 'Engineering', Salary = 75000 ;
ID = 2, Name = 'Jane Smith', Department = 'Marketing', Salary = 65000.

% Use complex queries
?- employee(ID, Name, 'Engineering', Salary), Salary > 70000.
```

For testing with SQLite (no DSN setup required):
```prolog
% Connect directly to SQLite database
?- working_directory(_, 'src'), use_module(db_connection).
?- db_connect('DRIVER=SQLite3;DATABASE=/path/to/database.db', Connection).
```

## Installation

1. Ensure you have SWI-Prolog installed with ODBC support:
   ```bash
   sudo apt-get install swi-prolog swi-prolog-odbc
   ```

2. Clone this repository:
   ```bash
   git clone <repository-url>
   cd prolog-sql-tools/tools/prolog-sql-bridge
   ```

3. Load the library in Prolog:
   ```prolog
   ?- use_module(library(sql_bridge)).
   ```

## Project Structure

```
prolog-sql-bridge/
├── src/
│   ├── sql_bridge.pl          # Main SQL bridge interface
│   ├── db_connection.pl       # Database connection management
│   ├── query_translator.pl    # Prolog-to-SQL translation
│   ├── schema_introspector.pl # Database schema discovery
│   └── type_mapper.pl         # Data type conversions
├── examples/
│   ├── basic_usage.pl         # Basic usage examples
│   ├── advanced_queries.pl    # Complex query examples
│   └── sample_data.sql        # Sample database setup
├── tests/
│   ├── test_connection.pl     # Connection tests
│   ├── test_queries.pl        # Query translation tests
│   └── test_types.pl          # Type mapping tests
└── docs/
    ├── api_reference.md       # Complete API documentation
    └── tutorial.md            # Step-by-step tutorial
```

## Documentation

- [API Reference](docs/api_reference.md)
- [Tutorial](docs/tutorial.md)
- [Examples](examples/)

## Testing

Run the quick test to verify your installation:

```bash
./quick_test.sh
```

## License

MIT License - see LICENSE file for details.
