# Knowledge Graph Extractor

Converts relational data into Prolog facts to build a reasoning layer, enabling semantic queries and knowledge discovery.

## Features

- **Multi-Source Data Loading**: CSV, JSON, databases, Prolog files
- **Semantic Mapping**: Convert relational schemas to semantic concepts
- **Relationship Inference**: Automatically discover relationships from foreign keys
- **Multiple Export Formats**: Turtle, RDF/XML, JSON-LD, GraphML, Prolog
- **Reasoning Engine**: Add custom rules for knowledge inference
- **Graph Analysis**: Structural analysis and centrality measures
- **Visualization Support**: Generate graph visualizations
- **Query Interface**: Semantic query processing

## Quick Start

### Basic Usage

```prolog
% Load the knowledge graph extractor
?- use_module(library(kg_extractor)).

% Extract knowledge graph from CSV data
?- extract_knowledge_graph(
    csv_file('data/employees.csv'),
    [mapping(employees, id, person_id),
     mapping(employees, department_id, department)],
    KG
).

% Query the knowledge graph
?- query_knowledge_graph(find_entities(person), People).

% Export to different formats
?- export_knowledge_graph(KG, turtle, 'output/knowledge_graph.ttl').
?- export_knowledge_graph(KG, json_ld, 'output/knowledge_graph.jsonld').
```

### Advanced Example with Reasoning

```prolog
% Load data and add reasoning rules
?- extract_knowledge_graph(
    database(connection_id),
    [mapping(employees, id, person),
     mapping(departments, id, organization),
     mapping(projects, id, project)],
    KG
).

% Add custom reasoning rules
?- add_reasoning_rules([
    rule(colleague_relation,
         (entity(P1, person, Props1),
          entity(P2, person, Props2),
          member(department-Dept, Props1),
          member(department-Dept, Props2),
          P1 \= P2,
          assertz(relationship(_, P1, colleague_of, P2)))),
    
    rule(project_team,
         (relationship(_, Person, works_on, Project),
          relationship(_, Manager, manages, Project),
          assertz(relationship(_, Person, reports_to, Manager))))
]).

% Query inferred relationships
?- query_knowledge_graph(find_relationships(john_doe, colleague_of), Colleagues).
```

## Data Source Configuration

### CSV Files
```prolog
% Load from CSV with custom mapping
extract_knowledge_graph(
    csv_file('data.csv'),
    [mapping(data, id, entity_id),
     mapping(data, name, label),
     mapping(data, category, type)],
    KG
)
```

### Database Integration
```prolog
% Use with SQL Bridge
use_module(library(sql_bridge)),
sql_connect('DSN=mydb', Conn),
extract_knowledge_graph(
    database(Conn),
    [mapping(users, user_id, person),
     mapping(orders, order_id, transaction)],
    KG
)
```

### JSON Data
```prolog
% Load structured JSON data
extract_knowledge_graph(
    json_file('data.json'),
    [mapping(root, id, identifier)],
    KG
)
```

## Schema Mapping

Define how relational data maps to semantic concepts:

```prolog
Schema = [
    mapping(table_name, column_name, semantic_type),
    mapping(employees, emp_id, person_id),
    mapping(employees, dept_id, department),
    mapping(departments, dept_id, organization_id),
    mapping(projects, proj_id, project_id)
]
```

## Reasoning Rules

Add domain-specific inference rules:

```prolog
Rules = [
    % Transitivity: if A manages B and B manages C, then A manages C (indirectly)
    rule(management_hierarchy,
         (relationship(_, A, manages, B),
          relationship(_, B, manages, C),
          assertz(relationship(_, A, indirectly_manages, C)))),
    
    % Similarity: entities with same properties are similar
    rule(similarity,
         (entity(E1, Type, Props),
          entity(E2, Type, Props),
          E1 \= E2,
          assertz(relationship(_, E1, similar_to, E2)))),
    
    % Domain rules: employees in same department are colleagues
    rule(colleagues,
         (entity(P1, person, Props1),
          entity(P2, person, Props2),
          member(department-D, Props1),
          member(department-D, Props2),
          P1 \= P2,
          assertz(relationship(_, P1, colleague_of, P2))))
]
```

## Export Formats

### Turtle (RDF)
```turtle
@prefix : <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

:john_doe rdf:type :person .
:john_doe :name "John Doe" .
:john_doe :department "Engineering" .
:john_doe :colleague_of :jane_smith .
```

### JSON-LD
```json
{
  "@context": {"@vocab": "http://example.org/"},
  "@graph": [
    {
      "@id": "john_doe",
      "@type": "person",
      "name": "John Doe",
      "department": "Engineering",
      "colleague_of": "jane_smith"
    }
  ]
}
```

### Prolog Facts
```prolog
entity(john_doe, person, [name-'John Doe', department-engineering]).
relationship(rel1, john_doe, colleague_of, jane_smith).
```

## Semantic Queries

Query the knowledge graph using semantic patterns:

```prolog
% Find all people
?- query_knowledge_graph(find_entities(person), People).

% Find relationships of a specific type
?- query_knowledge_graph(find_relationships(_, colleague_of), Colleagues).

% Find paths between entities
?- query_knowledge_graph(path(john_doe, jane_smith), Path).

% Complex queries with custom patterns
?- entity(Person, person, Props),
   member(department-engineering, Props),
   relationship(_, Person, works_on, Project).
```

## Graph Analysis

Analyze knowledge graph structure:

```prolog
% Get structural analysis
?- analyze_graph_structure(KG, Analysis).
Analysis = analysis(
    num_entities(150),
    num_relationships(300),
    degree_distribution([freq(john_doe, 15), freq(jane_smith, 12), ...]),
    connected_components([[john_doe, jane_smith, ...], [...]]),
    central_entities([john_doe, manager_smith])
).

% Find central entities
?- identify_central_entities(Relationships, Central).

% Detect communities/clusters
?- find_connected_components(Facts, Relationships, Components).
```

## Visualization

Generate graph visualizations:

```prolog
% Generate DOT format for Graphviz
?- visualize_graph(KG, dot).

% Generate SVG visualization
?- visualize_graph(KG, svg).

% Generate Cytoscape JSON
?- visualize_graph(KG, cytoscape).
```

## Merging Knowledge Graphs

Combine multiple knowledge graphs:

```prolog
% Extract from different sources
?- extract_knowledge_graph(csv_file('employees.csv'), Schema1, KG1),
   extract_knowledge_graph(json_file('projects.json'), Schema2, KG2).

% Merge graphs
?- merge_knowledge_graphs(KG1, KG2, MergedKG).

% The merged graph contains entities and relationships from both sources
```

## API Reference

### Core Predicates

- `extract_knowledge_graph(+Source, +Schema, -KG)` - Main extraction predicate
- `load_relational_data(+Source, -Data)` - Load data from various sources
- `generate_prolog_facts(+Data, -Facts)` - Convert to semantic facts
- `infer_relationships(+Facts, -Relationships)` - Discover relationships

### Query Interface

- `query_knowledge_graph(+Query, -Results)` - Execute semantic queries
- `find_entities(+Type)` - Find entities of specific type
- `find_relationships(+Subject, +Predicate)` - Find specific relationships
- `path(+Start, +End)` - Find paths between entities

### Export & Visualization

- `export_knowledge_graph(+KG, +Format, +File)` - Export in various formats
- `visualize_graph(+KG, +Format)` - Generate visualizations
- `merge_knowledge_graphs(+KG1, +KG2, -Merged)` - Merge graphs

### Analysis

- `analyze_graph_structure(+KG, -Analysis)` - Structural analysis
- `identify_central_entities(+Relationships, -Central)` - Find central nodes
- `find_connected_components(+Facts, +Rels, -Components)` - Community detection

## Examples

See the `examples/` directory for comprehensive usage examples:

- `basic_extraction.pl` - Basic knowledge graph extraction
- `reasoning_example.pl` - Custom reasoning rules
- `multi_source.pl` - Combining data from multiple sources
- `analysis_example.pl` - Graph analysis and visualization

## Testing

Run the test suite:

```bash
cd tools/knowledge-graph-extractor
./quick_test.sh
```

## License

MIT License - see LICENSE file for details.
