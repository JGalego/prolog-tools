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

- **Logic-Based Data Validator**: Define Prolog rules to check for inconsistencies or violations in data.
- **Knowledge Graph Extractor**: Convert relational data into Prolog facts to build a reasoning layer
- **Semantic Personal Knowledge Base**: A personal knowledge management system (like Obsidian or Logseq) that stores notes as a knowledge graph and uses Prolog for querying and reasoning.
    - Features: Natural language to Prolog rule generation, timeline reasoning, goal tracking.
    - Cool twist: Add a chatbot interface that answers questions using Prolog queries.
- **Bioinformatics Reasoner**: Build a Prolog-based reasoning engine over biomedical ontologies (e.g., Gene Ontology, SNOMED CT).
    - Use case: Infer gene-disease relationships or drug interactions.
    - Integration: Use SPARQL to pull data from public bio-ontologies.
- **Interactive Digital Logic Simulator with Prolog Backend**: A web-based tool where users can draw digital circuits (AND, OR, NOT, XOR, etc.), and the logic is evaluated using Prolog rules.
    - Frontend: Drag-and-drop circuit builder (e.g., using SVG or canvas).
    - Backend: Prolog engine that evaluates the circuit logic and propagates signals.
    - Features: Step-by-step signal propagation, Truth table generation, Circuit minimization suggestions, Export to Verilog or VHDL

## License

MIT License - see LICENSE file for details.
