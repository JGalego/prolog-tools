#!/usr/bin/env swipl
/**
 * Advanced Gene-Disease Analysis Example
 * 
 * This example demonstrates advanced gene-disease relationship inference
 * and pathway analysis using the bioinformatics reasoner.
 */

:- use_module('../src/bio_reasoner').

% Example: Cancer gene analysis
demo_cancer_gene_analysis :-
    format('=== Cancer Gene Analysis Demo ===~n'),
    
    % Load sample cancer gene data
    load_cancer_gene_data,
    
    % Analyze BRCA1 gene
    format('~n--- BRCA1 Gene Analysis ---~n'),
    infer_gene_disease('BRCA1', Disease, Evidence),
    format('BRCA1 associated with: ~w (Evidence: ~w)~n', [Disease, Evidence]),
    fail.  % Force backtracking to show all results
demo_cancer_gene_analysis :-
    
    % Pathway analysis for DNA repair genes
    format('~n--- DNA Repair Pathway Analysis ---~n'),
    pathway_analysis(['BRCA1', 'BRCA2', 'TP53', 'ATM', 'CHEK2'], 
                     dna_repair, Enrichment),
    format('DNA repair pathway enrichment: ~w~n', [Enrichment]),
    
    % Predict cancer susceptibility
    format('~n--- Cancer Susceptibility Prediction ---~n'),
    phenotype_inference([
        variant('BRCA1', 'c.185delAG', frameshift),
        variant('TP53', 'p.R273H', missense)
    ], Phenotypes),
    format('Predicted phenotypes: ~w~n', [Phenotypes]).

% Example: Drug interaction analysis
demo_drug_interaction_analysis :-
    format('~n=== Drug Interaction Analysis Demo ===~n'),
    
    % Load sample drug data
    load_drug_interaction_data,
    
    % Analyze drug combination safety
    DrugList = [warfarin, aspirin, simvastatin, metformin],
    format('Analyzing drug combination: ~w~n', [DrugList]),
    analyze_drug_interactions(DrugList, Interactions),
    format('~n--- Detected Interactions ---~n'),
    print_drug_interactions(Interactions),
    
    % Drug repurposing analysis
    format('~n--- Drug Repurposing Analysis ---~n'),
    drug_repurposing(aspirin, alzheimer_disease, Potential),
    format('Aspirin repurposing potential for Alzheimer\'s: ~w~n', [Potential]).

% Example: Protein function prediction
demo_protein_analysis :-
    format('~n=== Protein Function Analysis Demo ===~n'),
    
    % Load protein data
    load_protein_data,
    
    % Predict protein function from sequence
    protein_function_prediction('BRCA1_protein', Functions),
    format('BRCA1 predicted functions: ~w~n', [Functions]),
    
    % Sequence analysis
    sequence_analysis('MVLSPADKTNVKAAW', protein, Analysis),
    format('Protein sequence analysis: ~w~n', [Analysis]),
    
    % Variant impact assessment
    variant_impact_analysis(
        variant('BRCA1', 'c.68_69delAG', frameshift),
        Impact
    ),
    format('Variant impact: ~w~n', [Impact]).

% Example: Comparative genomics
demo_comparative_genomics :-
    format('~n=== Comparative Genomics Demo ===~n'),
    
    % Compare human and mouse genomes
    comparative_genomics(human, mouse, Comparison),
    format('Human-Mouse comparison: ~w~n', [Comparison]),
    
    % Ortholog analysis
    find_orthologs('BRCA1', human, mouse, Orthologs),
    format('BRCA1 orthologs: ~w~n', [Orthologs]).

% Load sample data for demonstrations
load_cancer_gene_data :-
    % Gene definitions
    assertz(gene('ENSG00000012048', 'BRCA1', 'BRCA1 DNA repair associated')),
    assertz(gene('ENSG00000139618', 'BRCA2', 'BRCA2 DNA repair associated')),
    assertz(gene('ENSG00000141510', 'TP53', 'tumor protein p53')),
    assertz(gene('ENSG00000149311', 'ATM', 'ATM serine/threonine kinase')),
    assertz(gene('ENSG00000183765', 'CHEK2', 'checkpoint kinase 2')),
    
    % Disease definitions
    assertz(disease('OMIM:114480', 'breast_cancer', 'neoplasm')),
    assertz(disease('OMIM:612555', 'ovarian_cancer', 'neoplasm')),
    assertz(disease('OMIM:151623', 'li_fraumeni_syndrome', 'cancer_syndrome')),
    
    % Gene-disease associations
    assertz(gene_disease_association('BRCA1', breast_cancer, 
                                   [literature_evidence, functional_evidence], 0.95)),
    assertz(gene_disease_association('BRCA1', ovarian_cancer,
                                   [genetic_association], 0.85)),
    assertz(gene_disease_association('BRCA2', breast_cancer,
                                   [literature_evidence], 0.90)),
    assertz(gene_disease_association('TP53', li_fraumeni_syndrome,
                                   [functional_evidence], 0.98)),
    
    % Pathway definitions
    assertz(pathway('GO:0006281', 'DNA repair', 
                   ['BRCA1', 'BRCA2', 'TP53', 'ATM', 'CHEK2'])).

load_drug_interaction_data :-
    % Drug definitions
    assertz(drug('DB00682', warfarin, anticoagulant, 'vitamin K antagonist')),
    assertz(drug('DB00945', aspirin, nsaid, 'COX inhibitor')),
    assertz(drug('DB00641', simvastatin, statin, 'HMG-CoA reductase inhibitor')),
    assertz(drug('DB00331', metformin, antidiabetic, 'biguanide')),
    
    % Drug interactions
    assertz(drug_interaction(warfarin, aspirin, potentiation, high)),
    assertz(drug_interaction(warfarin, simvastatin, interaction, medium)),
    assertz(drug_interaction(aspirin, metformin, minimal, low)).

load_protein_data :-
    % Protein definitions
    assertz(protein('P38398', 'BRCA1', 'DNA_repair', 'tumor_suppressor')),
    assertz(protein('P51587', 'BRCA2', 'DNA_repair', 'tumor_suppressor')),
    assertz(protein('P04637', 'TP53', 'transcription_regulation', 'tumor_suppressor')),
    
    % Variant definitions
    assertz(variant('rs80357906', 'BRCA1', 'c.68_69delAG', frameshift, pathogenic)),
    assertz(variant('rs28897696', 'BRCA1', 'c.185delAG', frameshift, pathogenic)),
    assertz(variant('rs11571833', 'BRCA2', 'c.9976A>T', missense, benign)).

% Helper predicates
print_drug_interactions([]).
print_drug_interactions([interaction(Drug1, Drug2, Type, Severity)|Rest]) :-
    format('  ~w + ~w: ~w (~w severity)~n', [Drug1, Drug2, Type, Severity]),
    print_drug_interactions(Rest).

find_orthologs(Gene, Species1, Species2, Orthologs) :-
    % Simplified ortholog finding
    Orthologs = [ortholog(Gene, Species1, mouse_gene_equivalent, Species2, 0.95)].

% Enhanced analysis predicates (these would integrate with the main module)
advanced_pathway_analysis(Genes, Pathway, DetailedEnrichment) :-
    pathway_analysis(Genes, Pathway, BasicEnrichment),
    % Add additional statistical analysis
    DetailedEnrichment = [
        BasicEnrichment,
        statistical_significance(p_value(0.001)),
        effect_size(large),
        gene_overlap_ratio(0.8),
        functional_coherence(high)
    ].

clinical_interpretation(GeneVariants, ClinicalSignificance) :-
    % Aggregate variant impacts for clinical interpretation
    findall(Impact, 
            (member(Variant, GeneVariants),
             variant_impact_analysis(Variant, Impact)),
            Impacts),
    aggregate_clinical_significance(Impacts, ClinicalSignificance).

aggregate_clinical_significance(Impacts, ClinicalSignificance) :-
    % Simplified aggregation logic
    (   member([pathogenicity(pathogenic)|_], Impacts)
    ->  ClinicalSignificance = high_risk
    ;   member([pathogenicity(likely_pathogenic)|_], Impacts)
    ->  ClinicalSignificance = moderate_risk
    ;   ClinicalSignificance = low_risk
    ).

% Pharmacogenomics analysis
pharmacogenomic_analysis(Drug, PatientGenotype, Recommendation) :-
    % Analyze how patient's genotype affects drug response
    drug_metabolizing_enzymes(Drug, Enzymes),
    patient_enzyme_variants(PatientGenotype, Enzymes, Variants),
    predict_drug_response(Drug, Variants, Recommendation).

drug_metabolizing_enzymes(warfarin, ['CYP2C9', 'VKORC1']).
drug_metabolizing_enzymes(simvastatin, ['CYP3A4']).

patient_enzyme_variants(PatientGenotype, Enzymes, Variants) :-
    % Extract relevant enzyme variants from patient genotype
    findall(EnzymeVariant,
            (member(Enzyme, Enzymes),
             member(variant(Enzyme, _, _), PatientGenotype),
             EnzymeVariant = variant(Enzyme, _, _)),
            Variants).

predict_drug_response(Drug, Variants, Recommendation) :-
    % Simplified drug response prediction
    (   member(variant('CYP2C9', _, reduced_function), Variants)
    ->  Recommendation = reduce_dose(Drug, 0.5)
    ;   Recommendation = standard_dose(Drug)
    ).

% Run all demos
run_bio_demo :-
    demo_cancer_gene_analysis,
    demo_drug_interaction_analysis,
    demo_protein_analysis,
    demo_comparative_genomics,
    format('~n=== Bioinformatics analysis demo completed ===~n').

% Query interface
?- write('Loading advanced bioinformatics analysis demo...'), nl.
?- write('Type "run_bio_demo." to see all examples'), nl.
?- write('Available demos: demo_cancer_gene_analysis, demo_drug_interaction_analysis, demo_protein_analysis, demo_comparative_genomics'), nl.
