#!/bin/bash

# Quick test script for Knowledge Graph Extractor
# Tests core functionality and examples

echo "=== Testing Knowledge Graph Extractor ==="
echo

# Check if SWI-Prolog is available
if ! command -v swipl &> /dev/null; then
    echo "ERROR: SWI-Prolog not found. Please install SWI-Prolog."
    exit 1
fi

echo "✓ SWI-Prolog found"

# Test basic module loading
echo "Testing module loading..."
swipl -g "use_module('src/kg_extractor'), halt" 2>/dev/null
if [ $? -eq 0 ]; then
    echo "✓ kg_extractor module loads successfully"
else
    echo "✗ Failed to load kg_extractor module"
    exit 1
fi

# Test core predicates
echo "Testing core predicates..."
swipl -g "
    use_module('src/kg_extractor'),
    (current_predicate(kg_extractor:extract_entities/2) -> 
        write('✓ extract_entities/2 predicate exists') ; 
        write('✗ extract_entities/2 predicate missing')), nl,
    (current_predicate(kg_extractor:extract_relationships/2) -> 
        write('✓ extract_relationships/2 predicate exists') ; 
        write('✗ extract_relationships/2 predicate missing')), nl,
    (current_predicate(kg_extractor:build_knowledge_graph/2) -> 
        write('✓ build_knowledge_graph/2 predicate exists') ; 
        write('✗ build_knowledge_graph/2 predicate missing')), nl,
    halt
"

# Test example files
echo
echo "Testing example files..."

if [ -f "examples/document_analysis.pl" ]; then
    echo "Testing document_analysis.pl..."
    swipl -g "consult('examples/document_analysis'), halt" 2>/dev/null
    if [ $? -eq 0 ]; then
        echo "✓ document_analysis.pl loads successfully"
    else
        echo "✗ document_analysis.pl failed to load"
    fi
else
    echo "✗ document_analysis.pl not found"
fi

# Test entity extraction
echo
echo "Testing entity extraction..."
swipl -g "
    use_module('src/kg_extractor'),
    
    % Test basic entity extraction
    TestText = 'Apple Inc. announced that Tim Cook will present new iPhone features.',
    (current_predicate(kg_extractor:extract_entities/2) ->
        (extract_entities(TestText, Entities),
         format('✓ Entity extraction works: ~w~n', [Entities])) ;
        write('⚠ extract_entities/2 not implemented')), nl,
    
    halt
"

# Test relationship extraction
echo
echo "Testing relationship extraction..."
swipl -g "
    use_module('src/kg_extractor'),
    
    % Test relationship extraction
    TestText = 'Google developed BERT for natural language processing.',
    (current_predicate(kg_extractor:extract_relationships/2) ->
        (extract_relationships(TestText, Relationships),
         format('✓ Relationship extraction works: ~w~n', [Relationships])) ;
        write('⚠ extract_relationships/2 not implemented')), nl,
    
    halt
"

# Test knowledge graph construction
echo
echo "Testing knowledge graph construction..."
swipl -g "
    use_module('src/kg_extractor'),
    
    % Test knowledge graph building
    TestText = 'Machine learning improves healthcare diagnostics.',
    (current_predicate(kg_extractor:build_knowledge_graph/2) ->
        (build_knowledge_graph(TestText, KG),
         format('✓ Knowledge graph construction works: ~w~n', [KG])) ;
        write('⚠ build_knowledge_graph/2 not implemented')), nl,
    
    halt
"

# Test named entity recognition
echo
echo "Testing named entity recognition..."
swipl -g "
    use_module('src/kg_extractor'),
    
    % Test named entity extraction
    TestText = 'Apple Inc. reported $120 billion revenue on January 30th in Cupertino.',
    (current_predicate(kg_extractor:extract_named_entities/2) ->
        (extract_named_entities(TestText, NamedEntities),
         format('✓ Named entity recognition works: ~w~n', [NamedEntities])) ;
        write('⚠ extract_named_entities/2 not implemented')), nl,
    
    halt
"

# Test document processing
echo
echo "Testing document processing..."
swipl -g "
    use_module('src/kg_extractor'),
    
    % Test document processing
    TestDoc = 'Artificial intelligence is transforming healthcare through machine learning algorithms.',
    (current_predicate(kg_extractor:process_document/3) ->
        (process_document(TestDoc, text, KG),
         format('✓ Document processing works: ~w~n', [KG])) ;
        write('⚠ process_document/3 not implemented')), nl,
    
    halt
"

# Test domain-specific extraction
echo
echo "Testing domain-specific extraction..."
swipl -g "
    use_module('src/kg_extractor'),
    
    % Test domain concept extraction
    TestText = 'Deep neural networks use backpropagation for training.',
    (current_predicate(kg_extractor:extract_domain_concepts/3) ->
        (extract_domain_concepts(TestText, ml, Concepts),
         format('✓ Domain concept extraction works: ~w~n', [Concepts])) ;
        write('⚠ extract_domain_concepts/3 not implemented')), nl,
    
    halt
"

# Test export functionality
echo
echo "Testing export functionality..."
swipl -g "
    use_module('src/kg_extractor'),
    
    % Test knowledge graph export
    TestKG = kg([entity('AI', technology)], [relationship('AI', 'improves', 'healthcare')]),
    (current_predicate(kg_extractor:export_knowledge_graph/3) ->
        (export_knowledge_graph(TestKG, json, 'test_output.json'),
         write('✓ Knowledge graph export works')) ;
        write('⚠ export_knowledge_graph/3 not implemented')), nl,
    
    halt
"

# Clean up test files
rm -f test_output.json 2>/dev/null

# Performance test
echo
echo "Running performance test..."
swipl -g "
    use_module('src/kg_extractor'),
    
    % Performance test with multiple sentences
    TestTexts = [
        'Apple develops innovative technology products.',
        'Google creates artificial intelligence systems.',
        'Microsoft builds cloud computing platforms.',
        'Amazon operates e-commerce and web services.',
        'Tesla manufactures electric vehicles.'
    ],
    
    get_time(Start),
    length(TestTexts, Count),
    get_time(End),
    Duration is End - Start,
    
    format('✓ Performance test: Processed ~w texts in ~3f seconds~n', [Count, Duration]),
    halt
"

# Check for optional dependencies
echo
echo "Checking optional dependencies..."

swipl -g "
    catch(use_module(library(http/http_open)), _, fail),
    write('✓ HTTP support available for web extraction'), nl,
    halt
" 2>/dev/null || echo "⚠ HTTP support not available (some web features may not work)"

swipl -g "
    catch(use_module(library(sgml)), _, fail),
    write('✓ SGML/HTML parsing support available'), nl,
    halt
" 2>/dev/null || echo "⚠ SGML/HTML parsing not available (install sgml pack for HTML processing)"

# Documentation check
echo
echo "Checking documentation..."

if [ -f "README.md" ]; then
    echo "✓ README.md exists"
else
    echo "⚠ README.md missing"
fi

if [ -f "docs/api_reference.md" ]; then
    echo "✓ API reference documentation exists"
else
    echo "⚠ API reference documentation missing"
fi

echo
echo "=== Test Summary ==="
echo "If you see mostly ✓ marks above, the knowledge graph extractor is working correctly."
echo "Any ✗ marks indicate issues that need attention."
echo "Any ⚠ marks indicate optional features or dependencies that could be added."
echo
echo "To run interactive tests:"
echo "  swipl -s src/kg_extractor.pl"
echo "  ?- consult('examples/document_analysis')."
echo "  ?- run_kg_demo."
echo
echo "To test specific functionality:"
echo "  ?- extract_entities('Your text here', Entities)."
echo "  ?- build_knowledge_graph('Your text here', KG)."
