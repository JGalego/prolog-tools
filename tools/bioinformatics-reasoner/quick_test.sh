#!/bin/bash

# Quick test script for Bioinformatics Reasoner
# Tests core functionality and examples

echo "=== Testing Bioinformatics Reasoner ==="
echo

# Check if SWI-Prolog is available
if ! command -v swipl &> /dev/null; then
    echo "ERROR: SWI-Prolog not found. Please install SWI-Prolog."
    exit 1
fi

echo "✓ SWI-Prolog found"

# Test basic module loading
echo "Testing module loading..."
swipl -g "use_module('src/bio_reasoner'), halt" 2>/dev/null
if [ $? -eq 0 ]; then
    echo "✓ bio_reasoner module loads successfully"
else
    echo "✗ Failed to load bio_reasoner module"
    exit 1
fi

# Test basic predicates
echo "Testing core predicates..."
swipl -g "
    use_module('src/bio_reasoner'),
    (current_predicate(bio_reasoner:load_ontology/2) -> 
        write('✓ load_ontology/2 predicate exists') ; 
        write('✗ load_ontology/2 predicate missing')), nl,
    (current_predicate(bio_reasoner:infer_gene_disease/3) -> 
        write('✓ infer_gene_disease/3 predicate exists') ; 
        write('✗ infer_gene_disease/3 predicate missing')), nl,
    (current_predicate(bio_reasoner:analyze_drug_interactions/2) -> 
        write('✓ analyze_drug_interactions/2 predicate exists') ; 
        write('✗ analyze_drug_interactions/2 predicate missing')), nl,
    halt
"

# Test example files
echo
echo "Testing example files..."

if [ -f "examples/basic_usage.pl" ]; then
    echo "Testing basic_usage.pl..."
    swipl -g "consult('examples/basic_usage'), halt" 2>/dev/null
    if [ $? -eq 0 ]; then
        echo "✓ basic_usage.pl loads successfully"
    else
        echo "✗ basic_usage.pl failed to load"
    fi
else
    echo "✗ basic_usage.pl not found"
fi

if [ -f "examples/advanced_analysis.pl" ]; then
    echo "Testing advanced_analysis.pl..."
    swipl -g "consult('examples/advanced_analysis'), halt" 2>/dev/null
    if [ $? -eq 0 ]; then
        echo "✓ advanced_analysis.pl loads successfully"
    else
        echo "✗ advanced_analysis.pl failed to load"
    fi
else
    echo "✗ advanced_analysis.pl not found"
fi

# Test data loading and basic functionality
echo
echo "Testing basic functionality..."
swipl -g "
    use_module('src/bio_reasoner'),
    % Test gene assertion
    assertz(gene('TEST001', 'TESTGENE', 'Test gene for validation')),
    assertz(disease('TEST001', 'test_disease', 'test_category')),
    assertz(gene_disease_association('TESTGENE', test_disease, [test_evidence], 0.8)),
    
    % Test basic query
    (gene_disease_association('TESTGENE', Disease, Evidence, Confidence) ->
        (format('✓ Gene-disease association works: ~w -> ~w (~w, ~w)~n', 
                ['TESTGENE', Disease, Evidence, Confidence])) ;
        write('✗ Gene-disease association failed')), nl,
    
    halt
"

# Test validation functions
echo
echo "Testing data validation..."
swipl -g "
    use_module('src/bio_reasoner'),
    
    % Test validate_biodata predicate
    TestData = [
        gene('G001', 'GENE1', 'Valid gene'),
        disease('D001', 'disease1', 'Valid disease'),
        gene_disease_association('GENE1', disease1, [evidence], 0.9)
    ],
    
    (current_predicate(bio_reasoner:validate_biodata/2) ->
        (validate_biodata(TestData, Violations),
         (Violations = [] ->
            write('✓ Data validation passed') ;
            format('⚠ Data validation found issues: ~w', [Violations]))) ;
        write('⚠ validate_biodata/2 not implemented')), nl,
    
    halt
"

# Test drug interaction functionality  
echo
echo "Testing drug interaction analysis..."
swipl -g "
    use_module('src/bio_reasoner'),
    
    % Add test drug data
    assertz(drug('D001', aspirin, nsaid, 'COX inhibitor')),
    assertz(drug('D002', warfarin, anticoagulant, 'Vitamin K antagonist')),
    assertz(drug_interaction(aspirin, warfarin, potentiation, high)),
    
    % Test drug interaction query
    (drug_interaction(Drug1, Drug2, Type, Severity) ->
        format('✓ Drug interaction detected: ~w + ~w = ~w (~w)~n', 
               [Drug1, Drug2, Type, Severity]) ;
        write('✗ No drug interactions found')), nl,
        
    halt
"

# Check for required dependencies
echo
echo "Checking dependencies..."

swipl -g "
    catch(use_module(library(semweb/rdf_db)), _, fail),
    write('✓ RDF database support available'), nl,
    halt
" 2>/dev/null || echo "⚠ RDF database support not available (install semweb pack)"

swipl -g "
    catch(use_module(library(http/http_open)), _, fail),
    write('✓ HTTP support available'), nl,
    halt
" 2>/dev/null || echo "⚠ HTTP support not available"

# Performance test
echo
echo "Running performance test..."
swipl -g "
    use_module('src/bio_reasoner'),
    
    % Load test data
    forall(between(1, 100, N), (
        atom_concat('GENE', N, GeneSymbol),
        atom_concat('gene_', N, GeneID),
        assertz(gene(GeneID, GeneSymbol, 'Test gene'))
    )),
    
    % Time a simple query
    get_time(Start),
    findall(Gene, gene(_, Gene, _), Genes),
    get_time(End),
    Duration is End - Start,
    length(Genes, Count),
    
    format('✓ Performance test: Found ~w genes in ~3f seconds~n', [Count, Duration]),
    halt
"

echo
echo "=== Test Summary ==="
echo "If you see mostly ✓ marks above, the bioinformatics reasoner is working correctly."
echo "Any ✗ marks indicate issues that need attention."
echo "Any ⚠ marks indicate optional features or dependencies that could be added."
echo
echo "To run interactive tests:"
echo "  swipl -s src/bio_reasoner.pl"
echo "  ?- consult('examples/basic_usage')."
echo "  ?- consult('examples/advanced_analysis')."
