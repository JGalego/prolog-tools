# Prolog Tools Collection - Summary

This repository contains a collection of useful Prolog tools and libraries.

## Repository Structure

```
prolog-sql-tools/
├── tools/                          # Collection of Prolog tools
│   └── prolog-sql-bridge/          # SQL database integration tool
│       ├── src/                    # Core library modules
│       │   ├── sql_bridge.pl       # Main SQL bridge interface
│       │   ├── db_connection.pl    # Database connection management
│       │   ├── query_translator.pl # Prolog-to-SQL translation
│       │   ├── schema_introspector.pl # Database schema discovery
│       │   └── type_mapper.pl      # Data type conversions
│       ├── examples/               # Usage examples and sample data
│       ├── tests/                  # Test suites
│       ├── docs/                   # Documentation
│       └── README.md               # Tool-specific documentation
├── README.md                       # Main project documentation
├── test_all.sh                     # Test runner for all tools
└── LICENSE                         # MIT License
```

## Available Tools

### 🗄️ Prolog-SQL Bridge (`tools/prolog-sql-bridge/`)

A library that enables querying SQL databases as if they were Prolog facts.

**Key Features:**
- Dynamic fact generation from SQL tables
- ODBC support for multiple database systems  
- Automatic schema introspection and mapping
- Query translation from Prolog to optimized SQL
- Connection pooling and management
- Type mapping between SQL and Prolog data types

**Status:** ✅ Fully functional and tested

**Quick Example:**
```prolog
% Connect and query database as Prolog facts
?- use_module(library(sql_bridge)).
?- sql_connect('DSN=mydb', Connection).
?- employee(ID, Name, Department, Salary).
```

**Documentation:**
- [README](tools/prolog-sql-bridge/README.md)
- [API Reference](tools/prolog-sql-bridge/docs/api_reference.md)
- [Tutorial](tools/prolog-sql-bridge/docs/tutorial.md)
- [Examples](tools/prolog-sql-bridge/examples/)

## Quick Start

1. **Test all tools:**
   ```bash
   ./test_all.sh
   ```

2. **Use a specific tool:**
   ```bash
   cd tools/prolog-sql-bridge
   ./quick_test.sh
   ```

3. **Read tool documentation:**
   Each tool has comprehensive documentation in its respective directory.

## Development

- Each tool maintains its own development lifecycle
- All tools follow the same structure: `src/`, `examples/`, `tests/`, `docs/`
- Common testing framework via `test_all.sh`
- Individual tool testing via each tool's `quick_test.sh`

## Future Tools

Planned additions to the collection:
- **Prolog-JSON Bridge**: Enhanced JSON parsing and generation
- **Prolog-HTTP Client**: HTTP client library with REST API support
- **Prolog-Config**: Configuration file management utilities
- **Prolog-Logging**: Structured logging framework

## Installation

1. **Prerequisites:**
   ```bash
   sudo apt-get install swi-prolog
   ```

2. **Clone repository:**
   ```bash
   git clone <repository-url>
   cd prolog-sql-tools
   ```

3. **Test everything:**
   ```bash
   ./test_all.sh
   ```

4. **Use individual tools:**
   ```bash
   cd tools/<tool-name>
   # Follow tool-specific README.md
   ```

## License

MIT License - see LICENSE file for details.