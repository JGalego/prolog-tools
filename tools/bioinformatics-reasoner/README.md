# Bioinformatics Reasoner

A Prolog-based reasoning engine for biomedical knowledge discovery, ontology querying, and bioinformatics analysis.

## Features

- **Ontology Integration**: Load and query biomedical ontologies (Gene Ontology, OMIM, etc.)
- **Gene-Disease Association**: Infer relationships between genes and diseases
- **Drug Interaction Analysis**: Analyze potential drug-drug interactions  
- **Pathway Analysis**: Perform pathway enrichment and gene set analysis
- **Phenotype Inference**: Predict phenotypes from genotypic data
- **Sequence Analysis**: Analyze DNA, RNA, and protein sequences
- **Protein Function Prediction**: Predict protein functions from structure/sequence
- **Variant Impact Analysis**: Assess the impact of genetic variants
- **SPARQL Integration**: Query linked biomedical data sources
- **Comparative Genomics**: Compare genomes across species

## Quick Start

```prolog
?- use_module(library(bio_reasoner)).
?- load_ontology('http://purl.obolibrary.org/obo/go.owl', owl).
?- query_gene_ontology(molecular_function(gene('BRCA1')), Functions).
?- infer_gene_disease('BRCA1', breast_cancer, Evidence).
```

## Installation

1. Ensure SWI-Prolog is installed with semantic web libraries:
   ```bash
   swipl -g "pack_install(semweb)" -t halt
   ```

2. Install additional dependencies:
   ```bash
   # For HTTP access to ontologies
   swipl -g "pack_install(http)" -t halt
   
   # For RDF/OWL processing
   swipl -g "pack_install(rdf_db)" -t halt
   ```

## Core Concepts

### Ontology Integration

The system can load various biomedical ontologies:

```prolog
% Load Gene Ontology
?- load_ontology('http://purl.obolibrary.org/obo/go.owl', owl).

% Load from local file
?- load_ontology('data/omim.ttl', turtle).

% Query loaded ontologies
?- query_gene_ontology(cellular_component(X), Components).
```

### Knowledge Representation

Biomedical entities are represented as structured facts:

```prolog
% Gene representation
gene(gene_id, symbol, description).
gene('ENSG00000012048', 'BRCA1', 'BRCA1 DNA repair associated').

% Disease representation  
disease(disease_id, name, category).
disease('OMIM:114480', 'Breast cancer', 'Neoplasm').

% Gene-disease associations
gene_disease_association('BRCA1', 'breast_cancer', literature_evidence, 0.95).

% Drug interactions
drug_interaction('warfarin', 'aspirin', 'potentiation', 'high').

% Protein information
protein('P38398', 'BRCA1', 'DNA_repair', 'tumor_suppressor').
```

## Usage Examples

### Gene-Disease Analysis

```prolog
% Find diseases associated with a gene
?- infer_gene_disease('BRCA1', Disease, Evidence).
Disease = breast_cancer,
Evidence = [literature_evidence, functional_evidence].

% Find genes associated with a disease
?- infer_gene_disease(Gene, diabetes, Evidence).
Gene = 'INS',
Evidence = [genetic_association].

% Query with confidence thresholds
?- gene_disease_association(Gene, Disease, Evidence, Confidence),
   Confidence > 0.8.
```

### Drug Interaction Analysis

```prolog
% Analyze drug interaction risks
?- analyze_drug_interactions([warfarin, aspirin, ibuprofen], Interactions).
Interactions = [
    interaction(warfarin, aspirin, potentiation, high),
    interaction(aspirin, ibuprofen, synergistic, medium)
].

% Check specific drug pair
?- drug_interaction('drug_a', 'drug_b', Type, Severity).
```

### Pathway Analysis

```prolog
% Perform pathway enrichment analysis
?- pathway_analysis(['BRCA1', 'TP53', 'ATM'], dna_repair_pathway, Enrichment).
Enrichment = [
    enrichment_score(0.85),
    p_value(0.001),
    genes_in_pathway(['BRCA1', 'TP53', 'ATM']),
    pathway_size(45)
].

% Find pathways for gene set
?- pathway_analysis(GeneSet, Pathway, Enrichment),
   member(enrichment_score(Score), Enrichment),
   Score > 0.7.
```

### Phenotype Prediction

```prolog
% Predict phenotypes from genotype
?- phenotype_inference(
     [variant('BRCA1', 'c.185delAG'), variant('TP53', 'p.R273H')],
     Phenotypes
   ).
Phenotypes = [
    phenotype(breast_cancer_susceptibility, 0.9),
    phenotype(li_fraumeni_syndrome, 0.7)
].
```

### Sequence Analysis

```prolog
% Analyze DNA sequence
?- sequence_analysis('ATGCGATCGATCG', dna, Analysis).
Analysis = [
    length(13),
    gc_content(0.54),
    codons(['ATG', 'CGA', 'TCG', 'ATC']),
    reading_frames(3)
].

% Protein sequence analysis
?- sequence_analysis('MVLSPADKTNVKAAW', protein, Analysis).
Analysis = [
    length(15),
    molecular_weight(1654.8),
    hydrophobicity(0.3),
    secondary_structure_prediction([helix, loop, sheet])
].
```

### Variant Impact Assessment

```prolog
% Assess variant pathogenicity
?- variant_impact_analysis(
     variant('BRCA1', 'c.68_69delAG', 'frameshift'),
     Impact
   ).
Impact = [
    pathogenicity(pathogenic),
    confidence(0.95),
    functional_effect(loss_of_function),
    clinical_significance(pathogenic)
].
```

### SPARQL Queries

```prolog
% Query external SPARQL endpoints
?- sparql_query(
     'https://sparql.uniprot.org/sparql',
     'SELECT ?protein WHERE { ?protein a up:Protein . } LIMIT 10',
     Results
   ).
```

## API Reference

### Core Predicates

- `load_ontology/2` - Load biomedical ontologies
- `query_gene_ontology/2` - Query Gene Ontology terms
- `infer_gene_disease/3` - Infer gene-disease relationships
- `analyze_drug_interactions/2` - Analyze drug interaction risks
- `pathway_analysis/3` - Perform pathway enrichment analysis
- `phenotype_inference/2` - Predict phenotypes from genotypes
- `sequence_analysis/3` - Analyze biological sequences
- `protein_function_prediction/2` - Predict protein functions
- `variant_impact_analysis/2` - Assess variant pathogenicity

### Data Management

- `export_inferences/2` - Export results to various formats
- `validate_biodata/2` - Validate biological data consistency
- `sparql_query/2` - Query external SPARQL endpoints

### Advanced Analysis

- `comparative_genomics/3` - Compare genomes across species
- `drug_repurposing/3` - Identify drug repurposing opportunities

## Configuration

### Ontology Sources

```prolog
% Configure default ontology sources
:- dynamic ontology_source/2.

ontology_source(go, 'http://purl.obolibrary.org/obo/go.owl').
ontology_source(omim, 'http://purl.obolibrary.org/obo/omim.owl').
ontology_source(hp, 'http://purl.obolibrary.org/obo/hp.owl').
```

### Evidence Weights

```prolog
% Configure evidence type weights
evidence_weight(literature_evidence, 0.8).
evidence_weight(functional_evidence, 0.9).
evidence_weight(genetic_association, 0.7).
evidence_weight(protein_interaction, 0.6).
```

## Data Sources

The system can integrate with various biomedical databases:

- **Gene Ontology** (GO): Functional gene annotations
- **OMIM**: Human genetic disorders
- **UniProt**: Protein sequence and functional information
- **STRING**: Protein-protein interaction networks
- **PharmGKB**: Pharmacogenomics data
- **ClinVar**: Clinical variant interpretations

## Testing

Run the test suite:

```bash
./quick_test.sh
```

Individual test modules:
```prolog
?- run_tests(ontology_loading).
?- run_tests(gene_disease_inference).
?- run_tests(drug_interactions).
```

## Examples

See the `examples/` directory for:
- Basic ontology querying
- Gene-disease association analysis
- Drug interaction screening
- Pathway enrichment workflows
- Variant interpretation pipelines

## Performance Notes

- Large ontologies may take time to load initially
- Consider using local ontology files for better performance
- Some SPARQL endpoints may have rate limits
- Cache frequently used query results

## License

MIT License - see LICENSE file for details.

## Citation

If you use this tool in research, please cite:
```
Bioinformatics Reasoner: A Prolog-based System for Biomedical Knowledge Discovery
Prolog Tools Collection, 2025
```
