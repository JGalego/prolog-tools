name(sql_bridge).
title('Prolog-SQL Bridge').
version('1.0.0').
author('Prolog SQL Tools Project', 'contact@example.com').
maintainer('Prolog SQL Tools Project', 'contact@example.com').
packager('Prolog SQL Tools Project', 'contact@example.com').
home('https://github.com/example/prolog-sql-tools').
download('https://github.com/example/prolog-sql-tools/archive/*.zip').
provides(sql_bridge).
provides(db_connection).
provides(query_translator).
provides(schema_introspector).
provides(type_mapper).
requires(odbc).
autoload(false).
