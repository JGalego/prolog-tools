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

### ✅ [Logic-Based Data Validator](tools/logic-data-validator/)
Define Prolog rules to check for inconsistencies or violations in complex data structures with customizable validation rules.

**Features:**
- Custom validation rule definitions
- Complex constraint checking
- Hierarchical data validation
- Business logic enforcement
- Report generation and violation tracking

### 🧠 [Knowledge Graph Extractor](tools/knowledge-graph-extractor/)
Convert relational data into Prolog facts to build a reasoning layer for knowledge discovery and inference.

**Features:**
- Automatic knowledge graph generation
- Entity relationship extraction
- Ontology mapping and alignment
- SPARQL endpoint integration
- Semantic reasoning capabilities

### 📚 [Semantic Knowledge Base](tools/semantic-knowledge-base/)
A personal knowledge management system that stores notes as a knowledge graph and uses Prolog for querying and reasoning.

**Features:**
- Natural language to Prolog rule generation
- Timeline reasoning and goal tracking
- Chatbot interface for knowledge queries
- Note linking and semantic search
- Export to various knowledge formats

### 🧬 [Bioinformatics Reasoner](tools/bioinformatics-reasoner/)
A Prolog-based reasoning engine over biomedical ontologies for inferring gene-disease relationships, drug interactions, and other biomedical knowledge.

**Features:**
- Gene Ontology integration
- Drug interaction analysis
- Pathway enrichment analysis
- Variant impact assessment
- SPARQL queries for bio-ontologies

### ⚡ [Digital Logic Simulator](tools/digital-logic-simulator/)
An interactive web-based tool for designing and simulating digital circuits using Prolog for logic evaluation.

**Features:**
- Drag-and-drop circuit builder
- Real-time logic simulation
- Truth table generation
- Circuit optimization suggestions
- Export to hardware description languages

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
│   ├── prolog-sql-bridge/      # SQL database integration
│   ├── logic-data-validator/   # Data validation with Prolog rules
│   ├── knowledge-graph-extractor/ # Knowledge graph generation
│   ├── semantic-knowledge-base/ # Personal knowledge management
│   ├── bioinformatics-reasoner/ # Biomedical knowledge inference
│   └── digital-logic-simulator/ # Digital circuit simulation
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

## Getting Help

Each tool includes:
- Comprehensive documentation in `docs/`
- Working examples in `examples/`
- Test suites in `tests/`
- Quick start scripts for immediate testing

Run `./test_all.sh` from the root directory to test all tools at once.

## License

MIT License - see LICENSE file for details.
