# Makefile for Prolog-SQL Bridge

.PHONY: all test install clean docs examples

# Default target
all: test docs

# Run all tests
test:
	@echo "Running Prolog-SQL Bridge tests..."
	@swipl -g "working_directory(_, 'tests'), [test_connection, test_queries, test_types], run_tests, halt" -t "halt(1)"

# Run specific test suites
test-connection:
	@echo "Running connection tests..."
	@swipl -g "working_directory(_, 'tests'), [test_connection], run_connection_tests, halt" -t "halt(1)"

test-queries:
	@echo "Running query translation tests..."
	@swipl -g "working_directory(_, 'tests'), [test_queries], run_query_tests, halt" -t "halt(1)"

test-types:
	@echo "Running type mapping tests..."
	@swipl -g "working_directory(_, 'tests'), [test_types], run_type_tests, halt" -t "halt(1)"

# Install dependencies (ODBC)
install-deps:
	@echo "Installing ODBC dependencies..."
	@if command -v apt-get >/dev/null 2>&1; then \
		sudo apt-get update && sudo apt-get install -y swi-prolog swi-prolog-odbc unixodbc-dev; \
	elif command -v yum >/dev/null 2>&1; then \
		sudo yum install -y swi-prolog unixODBC-devel; \
	elif command -v brew >/dev/null 2>&1; then \
		brew install swi-prolog unixodbc; \
	else \
		echo "Please install SWI-Prolog with ODBC support manually"; \
	fi

# Install the package
install:
	@echo "Installing Prolog-SQL Bridge..."
	@swipl -g "pack_install('.')" -t halt

# Setup sample database for examples
setup-sample-db:
	@echo "Setting up sample SQLite database..."
	@sqlite3 examples/sample.db < examples/sample_data.sql
	@echo "Sample database created at examples/sample.db"

# Run basic usage examples
run-examples: setup-sample-db
	@echo "Running basic examples..."
	@cd examples && swipl -g "[basic_usage], run_all_examples, halt" -t "halt(1)"

# Run advanced examples
run-advanced-examples: setup-sample-db
	@echo "Running advanced examples..."
	@cd examples && swipl -g "[advanced_queries], run_advanced_examples, halt" -t "halt(1)"

# Generate documentation (if you have a doc generator)
docs:
	@echo "Documentation is available in docs/ directory"
	@echo "- docs/api_reference.md - Complete API reference"
	@echo "- docs/tutorial.md - Step-by-step tutorial"

# Validate Prolog syntax
validate:
	@echo "Validating Prolog syntax..."
	@for file in src/*.pl; do \
		echo "Checking $$file..."; \
		swipl -g "consult('$$file'), halt" -t "halt(1)" || exit 1; \
	done
	@echo "All syntax checks passed"

# Load the library interactively
interactive:
	@echo "Starting interactive session with SQL Bridge loaded..."
	@swipl -g "working_directory(_, 'src'), use_module(sql_bridge)"

# Performance benchmarks
benchmark: setup-sample-db
	@echo "Running performance benchmarks..."
	@swipl -g "working_directory(_, 'tests'), [benchmark], run_benchmarks, halt" -t "halt(1)"

# Clean up generated files
clean:
	@echo "Cleaning up..."
	@rm -f examples/sample.db
	@rm -f *~
	@rm -f src/*~
	@rm -f tests/*~
	@rm -f examples/*~
	@rm -f docs/*~

# Check for ODBC configuration
check-odbc:
	@echo "Checking ODBC configuration..."
	@if command -v odbcinst >/dev/null 2>&1; then \
		echo "Available ODBC drivers:"; \
		odbcinst -q -d; \
		echo "Configured DSNs:"; \
		odbcinst -q -s; \
	else \
		echo "ODBC tools not found. Please install unixODBC."; \
	fi

# Setup development environment
dev-setup: install-deps check-odbc setup-sample-db
	@echo "Development environment setup complete!"
	@echo "Run 'make test' to verify installation"

# Create a simple SQLite DSN for testing
create-test-dsn:
	@echo "Creating test DSN configuration..."
	@mkdir -p ~/.odbc
	@echo "[test_db]" > ~/.odbc/odbc.ini
	@echo "Description = Test SQLite Database" >> ~/.odbc/odbc.ini
	@echo "Driver = SQLite3" >> ~/.odbc/odbc.ini
	@echo "Database = $(PWD)/examples/sample.db" >> ~/.odbc/odbc.ini
	@echo "Test DSN created. Use 'DSN=test_db' to connect."

# Help target
help:
	@echo "Prolog-SQL Bridge Makefile"
	@echo ""
	@echo "Available targets:"
	@echo "  all              - Run tests and prepare documentation"
	@echo "  test             - Run all test suites"
	@echo "  test-connection  - Run connection tests only"
	@echo "  test-queries     - Run query translation tests only"
	@echo "  test-types       - Run type mapping tests only"
	@echo "  install-deps     - Install system dependencies"
	@echo "  install          - Install as SWI-Prolog pack"
	@echo "  setup-sample-db  - Create sample SQLite database"
	@echo "  run-examples     - Run basic usage examples"
	@echo "  run-advanced-examples - Run advanced examples"
	@echo "  docs             - Information about documentation"
	@echo "  validate         - Check Prolog syntax"
	@echo "  interactive      - Start interactive Prolog session"
	@echo "  benchmark        - Run performance benchmarks"
	@echo "  clean            - Remove generated files"
	@echo "  check-odbc       - Check ODBC configuration"
	@echo "  dev-setup        - Setup complete development environment"
	@echo "  create-test-dsn  - Create test ODBC DSN"
	@echo "  help             - Show this help message"

# Check if SQLite3 is available
check-sqlite:
	@if ! command -v sqlite3 >/dev/null 2>&1; then \
		echo "SQLite3 not found. Installing..."; \
		if command -v apt-get >/dev/null 2>&1; then \
			sudo apt-get install -y sqlite3; \
		elif command -v brew >/dev/null 2>&1; then \
			brew install sqlite; \
		else \
			echo "Please install SQLite3 manually"; \
			exit 1; \
		fi; \
	fi

# Override setup-sample-db to check for SQLite first
setup-sample-db: check-sqlite
	@echo "Setting up sample SQLite database..."
	@sqlite3 examples/sample.db < examples/sample_data.sql
	@echo "Sample database created at examples/sample.db"
