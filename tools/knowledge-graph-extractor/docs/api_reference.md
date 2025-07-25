# API Reference - Knowledge Graph Extractor

## Core Predicates

### Text Processing

#### `extract_entities/2`
```prolog
extract_entities(+Text, -Entities) is det.
```
Extracts named entities from input text.

**Parameters:**
- `Text`: Input text string
- `Entities`: List of entity/2 terms

**Example:**
```prolog
?- extract_entities("Apple Inc. released iPhone 14", Entities).
Entities = [entity("Apple Inc.", organization), entity("iPhone 14", product)].
```

#### `extract_relationships/2`
```prolog
extract_relationships(+Text, -Relationships) is det.
```
Extracts semantic relationships between entities.

**Parameters:**
- `Text`: Input text string  
- `Relationships`: List of relationship/3 terms

**Example:**
```prolog
?- extract_relationships("Apple released iPhone", Relations).
Relations = [relationship("Apple", "released", "iPhone")].
```

#### `extract_keywords/2`
```prolog
extract_keywords(+Text, -Keywords) is det.
```
Extracts important keywords from text.

**Parameters:**
- `Text`: Input text string
- `Keywords`: List of keyword atoms

### Knowledge Graph Construction

#### `build_knowledge_graph/2`
```prolog
build_knowledge_graph(+Text, -KnowledgeGraph) is det.
```
Constructs a complete knowledge graph from text.

**Parameters:**
- `Text`: Input text string
- `KnowledgeGraph`: kg/2 term containing entities and relationships

**Example:**
```prolog
?- build_knowledge_graph("AI improves healthcare", KG).
KG = kg([entity("AI", technology), entity("healthcare", domain)], 
        [relationship("AI", "improves", "healthcare")]).
```

#### `merge_knowledge_graphs/3`
```prolog
merge_knowledge_graphs(+KG1, +KG2, -MergedKG) is det.
```
Combines two knowledge graphs into one.

**Parameters:**
- `KG1`, `KG2`: Input knowledge graphs
- `MergedKG`: Combined knowledge graph

### Document Processing

#### `process_document/3`
```prolog
process_document(+Document, +Format, -KnowledgeGraph) is det.
```
Processes various document formats to extract knowledge.

**Parameters:**
- `Document`: Document content or file path
- `Format`: Document format (text, pdf, html, json)
- `KnowledgeGraph`: Extracted knowledge graph

**Supported Formats:**
- `text`: Plain text documents
- `html`: HTML web pages
- `pdf`: PDF documents (requires external tool)
- `json`: Structured JSON data
- `xml`: XML documents

#### `extract_from_url/2`
```prolog
extract_from_url(+URL, -KnowledgeGraph) is det.
```
Extracts knowledge graph from web page.

**Parameters:**
- `URL`: Web page URL
- `KnowledgeGraph`: Extracted knowledge graph

### Named Entity Recognition

#### `extract_named_entities/2`
```prolog
extract_named_entities(+Text, -NamedEntities) is det.
```
Extracts categorized named entities.

**Parameters:**
- `Text`: Input text string
- `NamedEntities`: List of categorized entities

**Entity Categories:**
- `person`: People names
- `organization`: Companies, institutions
- `location`: Places, addresses
- `date`: Temporal expressions
- `money`: Monetary values
- `product`: Products, services

#### `classify_entity/2`
```prolog
classify_entity(+Entity, -Category) is det.
```
Classifies an entity into a category.

**Parameters:**
- `Entity`: Entity string
- `Category`: Entity category atom

### Semantic Analysis

#### `extract_semantic_roles/2`
```prolog
extract_semantic_roles(+Sentence, -Roles) is det.
```
Extracts semantic roles from sentence.

**Parameters:**
- `Sentence`: Input sentence string
- `Roles`: List of semantic role assignments

**Role Types:**
- `agent`: Entity performing action
- `patient`: Entity affected by action  
- `instrument`: Tool/method used
- `location`: Where action occurs
- `time`: When action occurs

#### `find_coreferences/2`
```prolog
find_coreferences(+Text, -Coreferences) is det.
```
Finds coreference chains in text.

**Parameters:**
- `Text`: Input text string
- `Coreferences`: List of coreference chains

### Domain-Specific Extraction

#### `extract_domain_concepts/3`
```prolog
extract_domain_concepts(+Text, +Domain, -Concepts) is det.
```
Extracts concepts specific to a domain.

**Parameters:**
- `Text`: Input text string
- `Domain`: Domain identifier (medical, legal, technical, etc.)
- `Concepts`: List of domain-specific concepts

**Supported Domains:**
- `medical`: Healthcare and biomedical terms
- `legal`: Legal terminology and concepts
- `technical`: Technology and engineering terms
- `financial`: Financial and business terms
- `scientific`: General scientific concepts

#### `apply_domain_rules/3`
```prolog
apply_domain_rules(+KnowledgeGraph, +Domain, -EnhancedKG) is det.
```
Applies domain-specific rules to enhance knowledge graph.

### Graph Analysis

#### `find_entity_connections/3`
```prolog
find_entity_connections(+KnowledgeGraph, +Entity, -Connections) is det.
```
Finds all connections for a specific entity.

**Parameters:**
- `KnowledgeGraph`: Input knowledge graph
- `Entity`: Target entity
- `Connections`: List of connected entities and relationships

#### `calculate_centrality/3`
```prolog
calculate_centrality(+KnowledgeGraph, +Entity, -Centrality) is det.
```
Calculates centrality score for an entity.

**Parameters:**
- `KnowledgeGraph`: Input knowledge graph
- `Entity`: Target entity
- `Centrality`: Numeric centrality score

#### `find_communities/2`
```prolog
find_communities(+KnowledgeGraph, -Communities) is det.
```
Identifies communities/clusters in knowledge graph.

### Export and Visualization

#### `export_knowledge_graph/3`
```prolog
export_knowledge_graph(+KnowledgeGraph, +Format, +Output) is det.
```
Exports knowledge graph to various formats.

**Parameters:**
- `KnowledgeGraph`: Input knowledge graph
- `Format`: Output format (json, rdf, graphml, cypher)
- `Output`: Output file path or stream

**Export Formats:**
- `json`: JSON representation
- `rdf`: RDF/Turtle format
- `graphml`: GraphML XML format
- `cypher`: Neo4j Cypher statements
- `dot`: Graphviz DOT format

#### `visualize_graph/2`
```prolog
visualize_graph(+KnowledgeGraph, +Options) is det.
```
Creates visual representation of knowledge graph.

**Parameters:**
- `KnowledgeGraph`: Input knowledge graph
- `Options`: Visualization options list

**Options:**
- `layout(Type)`: Layout algorithm (force, circular, hierarchical)
- `node_size(Size)`: Node size specification
- `edge_style(Style)`: Edge styling options
- `output(File)`: Output image file

### Batch Processing

#### `process_corpus/3`
```prolog
process_corpus(+DocumentList, +Options, -CombinedKG) is det.
```
Processes multiple documents to create combined knowledge graph.

**Parameters:**
- `DocumentList`: List of documents to process
- `Options`: Processing options
- `CombinedKG`: Combined knowledge graph

#### `extract_parallel/3`
```prolog
extract_parallel(+DocumentList, +Workers, -Results) is det.
```
Parallel processing of multiple documents.

**Parameters:**
- `DocumentList`: List of documents
- `Workers`: Number of worker threads
- `Results`: List of extracted knowledge graphs

### Quality Assessment

#### `validate_knowledge_graph/2`
```prolog
validate_knowledge_graph(+KnowledgeGraph, -ValidationReport) is det.
```
Validates knowledge graph for consistency and quality.

**Parameters:**
- `KnowledgeGraph`: Input knowledge graph
- `ValidationReport`: List of validation issues

#### `calculate_confidence/3`
```prolog
calculate_confidence(+KnowledgeGraph, +Element, -Confidence) is det.
```
Calculates confidence score for graph elements.

**Parameters:**
- `KnowledgeGraph`: Input knowledge graph
- `Element`: Entity or relationship to assess
- `Confidence`: Confidence score (0.0-1.0)

### Configuration

#### `set_extraction_parameters/1`
```prolog
set_extraction_parameters(+Parameters) is det.
```
Configures extraction parameters.

**Parameters:**
- `Parameters`: List of parameter settings

**Available Parameters:**
- `min_entity_length(N)`: Minimum entity name length
- `confidence_threshold(T)`: Minimum confidence for inclusion
- `max_relationships_per_entity(N)`: Limit relationships per entity
- `enable_coreference_resolution(Bool)`: Enable/disable coreference
- `language(Lang)`: Set processing language

#### `load_domain_ontology/2`
```prolog
load_domain_ontology(+Domain, +OntologyFile) is det.
```
Loads domain-specific ontology for enhanced extraction.

## Data Structures

### Knowledge Graph
```prolog
kg(Entities, Relationships)
```
- `Entities`: List of entity/2 terms
- `Relationships`: List of relationship/3 terms

### Entity
```prolog
entity(Name, Type)
entity(Name, Type, Attributes)
```
- `Name`: Entity name string
- `Type`: Entity type atom
- `Attributes`: Optional attribute list

### Relationship
```prolog
relationship(Subject, Predicate, Object)
relationship(Subject, Predicate, Object, Attributes)
```
- `Subject`: Source entity
- `Predicate`: Relationship type
- `Object`: Target entity
- `Attributes`: Optional relationship attributes

### Document
```prolog
document(Content, Metadata)
```
- `Content`: Document content
- `Metadata`: Document metadata (title, author, date, etc.)

## Error Handling

The system provides comprehensive error handling:

- `extraction_error(Type, Message)`: Extraction-specific errors
- `format_error(Format, Message)`: Format-related errors
- `validation_error(Element, Issue)`: Validation errors
- `resource_error(Resource, Problem)`: Resource access errors

## Performance Considerations

- Use `extract_parallel/3` for large document collections
- Set appropriate confidence thresholds to filter low-quality extractions
- Consider domain-specific optimizations for specialized corpora
- Monitor memory usage with large knowledge graphs

## Integration

The Knowledge Graph Extractor integrates well with:
- Semantic Knowledge Base for storage
- Logic Data Validator for quality assurance
- External NLP libraries for enhanced processing
- Graph databases for persistence

## Examples

See the `examples/` directory for comprehensive usage examples:
- Document analysis workflows
- Multi-format processing
- Domain-specific extraction
- Large-scale batch processing
