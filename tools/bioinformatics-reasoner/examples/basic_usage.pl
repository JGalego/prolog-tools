:- use_module('../src/bio_reasoner').

/** <module> Basic Usage Examples for Bioinformatics Reasoner

Examples demonstrating the main functionality of the bio_reasoner module.

@author Prolog Tools Collection
@version 1.0.0
*/

% Example 1: Query Gene Ontology
example_query_go :-
    query_gene_ontology(biological_process('DNA'), Results),
    format('GO biological processes related to DNA:~n'),
    forall(member(Result, Results), 
           format('  ~w~n', [Result])).

% Example 2: Infer gene-disease associations
example_gene_disease_inference :-
    infer_gene_disease('BRCA1', 'OMIM:114480', Evidence),
    format('Evidence for BRCA1-breast cancer association: ~w~n', [Evidence]).

% Example 3: Analyze drug interactions
example_drug_interactions :-
    analyze_drug_interactions(['DB01234', 'DB05679'], Interactions),
    format('Drug interactions found:~n'),
    forall(member(Interaction, Interactions),
           format('  ~w~n', [Interaction])).

% Example 4: Pathway enrichment analysis
example_pathway_analysis :-
    pathway_analysis(['BRCA1', 'BRCA2', 'TP53'], 'hsa05200', Enrichment),
    format('Pathway enrichment: ~w~n', [Enrichment]).

% Example 5: Sequence analysis
example_sequence_analysis :-
    Sequence = 'ATGCGATCGATCGTAGCTAGCTAG',
    sequence_analysis(Sequence, dna, Analysis),
    format('DNA sequence analysis: ~w~n', [Analysis]).

% Example 6: Protein function prediction
example_protein_prediction :-
    assertz(protein(test_protein, 'BRCA1', unknown, kinase_structure)),
    protein_function_prediction(test_protein, Functions),
    format('Predicted protein functions: ~w~n', [Functions]).

% Example 7: Variant impact analysis
example_variant_analysis :-
    variant_impact_analysis(variant('BRCA1', 1000, snp, missense), Impact),
    format('Variant impact: ~w~n', [Impact]).

% Example 8: Export results
example_export :-
    export_inferences(csv, 'results.csv'),
    format('Results exported to results.csv~n').

% Example 9: Validate biological data
example_validation :-
    TestData = [gene('BRCA1', 'BRCA1', 'Test gene'), variant(test, 25, snp, missense, effect)],
    validate_biodata(TestData, Violations),
    format('Data validation violations: ~w~n', [Violations]).

% Example 10: Comparative genomics
example_comparative_genomics :-
    comparative_genomics(homo_sapiens, mus_musculus, Comparison),
    format('Comparative genomics results: ~w~n', [Comparison]).

% Run all examples
run_all_examples :-
    format('=== Bioinformatics Reasoner Examples ===~n~n'),
    
    format('1. Gene Ontology Query:~n'),
    example_query_go,
    nl,
    
    format('2. Gene-Disease Inference:~n'),
    example_gene_disease_inference,
    nl,
    
    format('3. Drug Interactions:~n'),
    example_drug_interactions,
    nl,
    
    format('4. Pathway Analysis:~n'),
    example_pathway_analysis,
    nl,
    
    format('5. Sequence Analysis:~n'),
    example_sequence_analysis,
    nl,
    
    format('6. Protein Function Prediction:~n'),
    example_protein_prediction,
    nl,
    
    format('7. Variant Impact Analysis:~n'),
    example_variant_analysis,
    nl,
    
    format('8. Export Results:~n'),
    example_export,
    nl,
    
    format('9. Data Validation:~n'),
    example_validation,
    nl,
    
    format('10. Comparative Genomics:~n'),
    example_comparative_genomics,
    nl.

% Interactive query examples
demo_queries :-
    format('=== Interactive Demo Queries ===~n'),
    
    % Demo 1: Find all genes associated with cancer
    format('1. Genes associated with cancer:~n'),
    forall((gene_disease_association(Gene, Disease, Evidence, Confidence),
            atom_concat(_, 'cancer', Disease)),
           format('  ~w -> ~w (~w, confidence: ~w)~n', [Gene, Disease, Evidence, Confidence])),
    nl,
    
    % Demo 2: Find all drugs and their mechanisms
    format('2. Drugs and their mechanisms:~n'),
    forall(drug(ID, Name, Type, Mechanism),
           format('  ~w (~w): ~w mechanism - ~w~n', [Name, ID, Type, Mechanism])),
    nl,
    
    % Demo 3: Show pathway genes
    format('3. Pathway gene memberships:~n'),
    forall(pathway(ID, Name, Genes),
           format('  ~w (~w): ~w genes~n', [Name, ID, Genes])),
    nl.
