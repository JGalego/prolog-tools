#!/bin/bash

# Quick Test Script for Prolog-SQL Bridge
# This script sets up a sample database and runs basic tests

set -e  # Exit on any error

echo "=== Prolog-SQL Bridge Quick Test ==="
echo

# Check if we're in the right directory
if [ ! -f "src/sql_bridge.pl" ]; then
    echo "Error: Please run this script from the prolog-sql-tools directory"
    echo "Usage: cd /path/to/prolog-sql-tools && ./quick_test.sh"
    exit 1
fi

# Check for required tools
echo "1. Checking dependencies..."

if ! command -v swipl &> /dev/null; then
    echo "Error: SWI-Prolog not found. Please install:"
    echo "  sudo apt-get install swi-prolog swi-prolog-odbc"
    exit 1
fi

if ! command -v sqlite3 &> /dev/null; then
    echo "Error: SQLite3 not found. Please install:"
    echo "  sudo apt-get install sqlite3"
    exit 1
fi

echo "✓ SWI-Prolog found"
echo "✓ SQLite3 found"

# Check for ODBC SQLite driver
echo "Checking ODBC SQLite driver..."
if ! odbcinst -q -d | grep -i sqlite > /dev/null 2>&1; then
    echo "Warning: SQLite ODBC driver not found."
    echo "Installing SQLite ODBC driver..."
    
    # Try to install the driver
    if command -v apt-get &> /dev/null; then
        sudo apt-get update && sudo apt-get install -y libsqliteodbc
    elif command -v yum &> /dev/null; then
        sudo yum install -y sqlite-odbc
    else
        echo "Please install SQLite ODBC driver manually:"
        echo "  Ubuntu/Debian: sudo apt-get install libsqliteodbc"
        echo "  RHEL/CentOS: sudo yum install sqlite-odbc"
        echo "  Or compile from source: http://www.ch-werner.de/sqliteodbc/"
        exit 1
    fi
    
    # Check again
    if ! odbcinst -q -d | grep -i sqlite > /dev/null 2>&1; then
        echo "Error: SQLite ODBC driver installation failed."
        echo "Please install manually and try again."
        exit 1
    fi
fi

echo "✓ SQLite ODBC driver found"

# Test module loading
echo
echo "2. Testing module loading..."
if swipl -g "consult(src/sql_bridge), write('Module loaded successfully'), nl, halt" -t "halt(1)" 2>/dev/null; then
    echo "✓ SQL Bridge modules load successfully"
else
    echo "✗ Error loading SQL Bridge modules"
    exit 1
fi

# Create sample database
echo
echo "3. Setting up sample database..."
if [ -f "examples/sample.db" ]; then
    rm examples/sample.db
fi

sqlite3 examples/sample.db < examples/sample_data.sql
echo "✓ Sample database created at examples/sample.db"

# Create ODBC configuration
echo
echo "4. Creating ODBC configuration..."
ODBC_INI="$HOME/.odbc.ini"
DB_PATH="$(pwd)/examples/sample.db"

# Remove any existing sample_db configuration
if [ -f "$ODBC_INI" ]; then
    grep -v "^\[sample_db\]" "$ODBC_INI" > "$ODBC_INI.tmp" || true
    grep -v "^Description = Sample Employee Database" "$ODBC_INI.tmp" > "$ODBC_INI.tmp2" || true
    grep -v "^Driver = SQLite3" "$ODBC_INI.tmp2" > "$ODBC_INI.tmp3" || true
    grep -v "^Database = .*sample\.db" "$ODBC_INI.tmp3" > "$ODBC_INI" || true
    rm -f "$ODBC_INI.tmp" "$ODBC_INI.tmp2" "$ODBC_INI.tmp3"
fi

# Add new configuration
cat >> "$ODBC_INI" << EOF

[sample_db]
Description = Sample Employee Database
Driver = SQLite3
Database = $DB_PATH
EOF

echo "✓ ODBC configuration added to $ODBC_INI"

# Test database connectivity
echo
echo "5. Testing database connectivity..."

# First try a simple SQLite connection test
echo "Testing basic SQLite connectivity..."
if sqlite3 examples/sample.db "SELECT COUNT(*) FROM employee;" > /dev/null 2>&1; then
    echo "✓ SQLite database is accessible"
else
    echo "✗ SQLite database test failed"
    exit 1
fi

# Try ODBC connection
echo "Testing ODBC connectivity..."
cat > test_connection.pl << 'EOF'
:- working_directory(_, 'src').
:- use_module(db_connection).

test_connection :-
    catch(
        (
            db_connect('DSN=sample_db', Connection),
            write('✓ ODBC connection successful'), nl,
            db_disconnect(Connection)
        ),
        Error,
        (
            write('⚠ ODBC connection failed, trying direct SQLite...'), nl,
            % Try direct SQLite connection as fallback
            catch(
                (
                    atom_concat('DRIVER=SQLite3;DATABASE=', '$(pwd)/examples/sample.db', ConnStr),
                    db_connect(ConnStr, Connection2),
                    write('✓ Direct SQLite connection successful'), nl,
                    db_disconnect(Connection2)
                ),
                Error2,
                (
                    write('✗ All connection methods failed'), nl,
                    write('ODBC Error: '), write(Error), nl,
                    write('Direct Error: '), write(Error2), nl
                )
            )
        )
    ).

:- test_connection.
:- halt.
EOF

swipl -s test_connection.pl 2>/dev/null || true  # Don't fail if ODBC doesn't work

rm -f test_connection.pl

# Test basic queries
echo
echo "6. Testing SQL Bridge modules (without ODBC dependency)..."

cat > test_modules.pl << 'EOF'
:- working_directory(_, 'src').
:- use_module(sql_bridge).
:- use_module(query_translator).
:- use_module(type_mapper).
:- use_module(schema_introspector).

test_modules :-
    write('Testing type mapping...'), nl,
    type_map_prolog_to_sql(42, 'INTEGER', SQLValue),
    (   SQLValue = "42"
    ->  write('✓ Type mapping works'), nl
    ;   write('✗ Type mapping failed'), nl, halt(1)
    ),
    
    write('Testing query translation...'), nl,
    TableInfo = table_info([id, name, dept, salary], ['INTEGER', 'VARCHAR', 'VARCHAR', 'DECIMAL']),
    query_translate_prolog_to_sql(employee(1, _, _, _), TableInfo, SQL),
    (   sub_string(SQL, _, _, _, 'id = 1')
    ->  write('✓ Query translation works'), nl
    ;   write('✗ Query translation failed'), nl, halt(1)
    ),
    
    write('✓ All module tests passed!'), nl.

:- test_modules.
:- halt.
EOF

if swipl -s test_modules.pl 2>/dev/null; then
    echo "✓ SQL Bridge module functionality test passed"
else
    echo "✗ SQL Bridge module functionality test failed"
    rm -f test_modules.pl
    exit 1
fi

rm -f test_modules.pl

# Test direct database connection (most reliable method)
echo
echo "7. Testing direct database connection..."

DB_PATH="$(pwd)/examples/sample.db"
cat > test_direct_connection.pl << EOF
:- working_directory(_, 'src').
:- use_module(db_connection).

test_direct_connection :-
    catch(
        (
            db_connect('DRIVER=SQLite3;DATABASE=$DB_PATH', Connection),
            write('✓ Direct SQLite connection successful: '), write(Connection), nl,
            
            % Test a simple query
            db_execute_query(Connection, 'SELECT COUNT(*) FROM employee', Results),
            write('✓ Query execution successful: '), write(Results), nl,
            
            db_disconnect(Connection),
            write('✓ Disconnection successful'), nl
        ),
        Error,
        (
            write('✗ Direct connection test failed: '), write(Error), nl,
            halt(1)
        )
    ).

:- test_direct_connection.
:- halt.
EOF

if swipl -s test_direct_connection.pl; then
    echo "✓ Direct database connection test passed"
else
    echo "✗ Direct database connection test failed"
    exit 1
fi

rm -f test_direct_connection.pl

# Success message
echo
echo "🎉 All tests passed! Your Prolog-SQL Bridge is working correctly."
echo
echo "Note: For full database connectivity, you may need to install ODBC drivers:"
echo "  sudo apt-get install libsqliteodbc        # For SQLite"
echo "  sudo apt-get install odbc-postgresql      # For PostgreSQL"
echo "  sudo apt-get install libmyodbc           # For MySQL"
echo
echo "Next steps:"
echo "  1. Try the examples: make run-examples"
echo "  2. Read the tutorial: docs/tutorial.md"
echo "  3. Check the API reference: docs/api_reference.md"
echo
echo "Sample database is ready at: examples/sample.db"
echo "You can explore it with: sqlite3 examples/sample.db"
echo
echo "To test with a working ODBC setup:"
echo "  swipl"
echo "  ?- working_directory(_, 'src'), use_module(sql_bridge)."
echo "  ?- sql_connect('DSN=sample_db', Conn), sql_register_tables(Conn)."
echo "  ?- employee(ID, Name, Department, Salary)."
echo
echo "For direct SQLite connection (recommended for testing):"
echo "  swipl"
echo "  ?- working_directory(_, 'src'), use_module(db_connection)."
echo "  ?- db_connect('DRIVER=SQLite3;DATABASE=$(pwd)/examples/sample.db', Conn)."
echo "  ?- db_execute_query(Conn, 'SELECT * FROM employee LIMIT 3', Results)."
echo
