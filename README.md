# Prolog Tools Collection

A collection of useful Prolog tools and libraries for various programming tasks.

## Available Tools

### 🗄️ [Prolog-SQL Bridge](tools/prolog-sql-bridge/)
A library that allows querying SQL databases as if they were Prolog facts, providing seamless integration between Prolog and relational databases.

**Features:**
- Dynamic Fact Generation from SQL tables
- ODBC Support for multiple database systems
- Schema Introspection and automatic mapping
- Query Translation from Prolog to SQL
- Connection Pooling and management
- Type Mapping between SQL and Prolog data types

**Quick Example:**
```prolog
% Connect and query database as Prolog facts
?- use_module(library(sql_bridge)).
?- sql_connect('DSN=mydb', Connection).
?- employee(ID, Name, Department, Salary).
```

## Getting Started

Each tool has its own directory under `tools/` with complete documentation, examples, and tests.

### Quick Installation

1. **Install SWI-Prolog**:
   ```bash
   sudo apt-get install swi-prolog
   ```

2. **Clone this repository**:
   ```bash
   git clone <repository-url>
   cd prolog-sql-tools
   ```

3. **Navigate to the tool you want to use**:
   ```bash
   cd tools/prolog-sql-bridge
   ./quick_test.sh
   ```

## Repository Structure

```
prolog-sql-tools/
├── tools/
│   └── prolog-sql-bridge/      # SQL database integration
│       ├── src/                # Core library modules
│       ├── examples/           # Usage examples
│       ├── tests/              # Test suites
│       ├── docs/               # Documentation
│       └── README.md           # Tool-specific documentation
├── README.md                   # This file
└── LICENSE                     # MIT License
```

## Contributing

We welcome contributions! Each tool maintains its own development guidelines in its respective directory.

## Future Tools (Planned)

- **Prolog-JSON Bridge**: Enhanced JSON parsing and generation
- **Prolog-HTTP Client**: HTTP client library with REST API support  
- **Prolog-Config**: Configuration file management
- **Prolog-Logging**: Structured logging utilities

## License

MIT License - see LICENSE file for details.
