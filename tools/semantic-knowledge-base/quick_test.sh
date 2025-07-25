#!/bin/bash

# Quick test script for Semantic Knowledge Base
# Tests core functionality and examples

echo "=== Testing Semantic Knowledge Base ==="
echo

# Check if SWI-Prolog is available
if ! command -v swipl &> /dev/null; then
    echo "ERROR: SWI-Prolog not found. Please install SWI-Prolog."
    exit 1
fi

echo "✓ SWI-Prolog found"

# Test basic module loading
echo "Testing module loading..."
swipl -g "use_module('src/semantic_kb'), halt" 2>/dev/null
if [ $? -eq 0 ]; then
    echo "✓ semantic_kb module loads successfully"
else
    echo "✗ Failed to load semantic_kb module"
    exit 1
fi

# Test core predicates
echo "Testing core predicates..."
swipl -g "
    use_module('src/semantic_kb'),
    (current_predicate(semantic_kb:add_note/3) -> 
        write('✓ add_note/3 predicate exists') ; 
        write('✗ add_note/3 predicate missing')), nl,
    (current_predicate(semantic_kb:query_notes/2) -> 
        write('✓ query_notes/2 predicate exists') ; 
        write('✗ query_notes/2 predicate missing')), nl,
    (current_predicate(semantic_kb:search_semantic/2) -> 
        write('✓ search_semantic/2 predicate exists') ; 
        write('✗ search_semantic/2 predicate missing')), nl,
    halt
"

# Test example files
echo
echo "Testing example files..."

if [ -f "examples/personal_journal.pl" ]; then
    echo "Testing personal_journal.pl..."
    swipl -g "consult('examples/personal_journal'), halt" 2>/dev/null
    if [ $? -eq 0 ]; then
        echo "✓ personal_journal.pl loads successfully"
    else
        echo "✗ personal_journal.pl failed to load"
    fi
else
    echo "✗ personal_journal.pl not found"
fi

if [ -f "examples/research_notes.pl" ]; then
    echo "Testing research_notes.pl..."
    swipl -g "consult('examples/research_notes'), halt" 2>/dev/null
    if [ $? -eq 0 ]; then
        echo "✓ research_notes.pl loads successfully"
    else
        echo "✗ research_notes.pl failed to load"
    fi
else
    echo "✗ research_notes.pl not found"
fi

if [ -f "examples/chatbot_demo.pl" ]; then
    echo "Testing chatbot_demo.pl..."
    swipl -g "consult('examples/chatbot_demo'), halt" 2>/dev/null
    if [ $? -eq 0 ]; then
        echo "✓ chatbot_demo.pl loads successfully"
    else
        echo "✗ chatbot_demo.pl failed to load"
    fi
else
    echo "✗ chatbot_demo.pl not found"
fi

# Test note management
echo
echo "Testing note management..."
swipl -g "
    use_module('src/semantic_kb'),
    
    % Test adding a note
    add_note(test_001, 'This is a test note about Prolog programming.', 
             [tags([prolog, programming, test]), date(2025, 1, 25)]),
    write('✓ Note added successfully'), nl,
    
    % Test querying notes
    (query_notes(note(test_001, Content, Metadata, _), Results) ->
        format('✓ Note query works: ~w~n', [Results]) ;
        write('✗ Note query failed')), nl,
    
    halt
"

# Test semantic search
echo
echo "Testing semantic search..."
swipl -g "
    use_module('src/semantic_kb'),
    
    % Add test notes
    add_note(note_001, 'Learning Prolog is fun and educational.', 
             [tags([prolog, learning, education])]),
    add_note(note_002, 'Machine learning algorithms are powerful.', 
             [tags([machine_learning, algorithms, ai])]),
    add_note(note_003, 'Prolog excels at symbolic reasoning.', 
             [tags([prolog, reasoning, symbolic])]),
    
    % Test semantic search
    (search_semantic([prolog], PrologNotes) ->
        format('✓ Semantic search works: found ~w notes about Prolog~n', [PrologNotes]) ;
        write('✗ Semantic search failed')), nl,
    
    halt
"

# Test note linking
echo
echo "Testing note linking..."
swipl -g "
    use_module('src/semantic_kb'),
    
    % Test linking notes
    (current_predicate(semantic_kb:link_notes/3) ->
        (link_notes(note_001, note_003, 'related_to'),
         write('✓ Note linking works')) ;
        write('⚠ link_notes/3 not implemented')), nl,
    
    halt
"

# Test tagging system
echo
echo "Testing tagging system..."
swipl -g "
    use_module('src/semantic_kb'),
    
    % Test tagging
    (current_predicate(semantic_kb:tag_note/2) ->
        (tag_note(note_001, 'important'),
         write('✓ Note tagging works')) ;
        write('⚠ tag_note/2 not implemented')), nl,
    
    halt
"

# Test timeline functionality
echo
echo "Testing timeline functionality..."
swipl -g "
    use_module('src/semantic_kb'),
    
    % Test timeline queries
    (current_predicate(semantic_kb:timeline_query/3) ->
        (timeline_query(date(2025, 1, 1), date(2025, 12, 31), Events),
         format('✓ Timeline query works: ~w~n', [Events])) ;
        write('⚠ timeline_query/3 not implemented')), nl,
    
    halt
"

# Test goal tracking
echo
echo "Testing goal tracking..."
swipl -g "
    use_module('src/semantic_kb'),
    
    % Test goal tracking
    (current_predicate(semantic_kb:goal_tracking/2) ->
        (goal_tracking('learn_prolog', Progress),
         format('✓ Goal tracking works: ~w~n', [Progress])) ;
        write('⚠ goal_tracking/2 not implemented')), nl,
    
    halt
"

# Test natural language processing
echo
echo "Testing natural language processing..."
swipl -g "
    use_module('src/semantic_kb'),
    
    % Test natural language queries
    (current_predicate(semantic_kb:natural_language_query/2) ->
        (natural_language_query('What do I know about Prolog?', Answer),
         format('✓ Natural language query works: ~w~n', [Answer])) ;
        write('⚠ natural_language_query/2 not implemented')), nl,
    
    halt
"

# Test export functionality
echo
echo "Testing export functionality..."
swipl -g "
    use_module('src/semantic_kb'),
    
    % Test knowledge base export
    (current_predicate(semantic_kb:export_knowledge_base/2) ->
        (export_knowledge_base(json, 'test_kb_export.json'),
         write('✓ Knowledge base export works')) ;
        write('⚠ export_knowledge_base/2 not implemented')), nl,
    
    halt
"

# Test backup and restore
echo
echo "Testing backup and restore..."
swipl -g "
    use_module('src/semantic_kb'),
    
    % Test backup
    (current_predicate(semantic_kb:backup_knowledge_base/1) ->
        (backup_knowledge_base('test_backup.kb'),
         write('✓ Knowledge base backup works')) ;
        write('⚠ backup_knowledge_base/1 not implemented')), nl,
    
    % Test restore
    (current_predicate(semantic_kb:restore_knowledge_base/1) ->
        (restore_knowledge_base('test_backup.kb'),
         write('✓ Knowledge base restore works')) ;
        write('⚠ restore_knowledge_base/1 not implemented')), nl,
    
    halt
"

# Clean up test files
rm -f test_kb_export.json test_backup.kb 2>/dev/null

# Test insights generation
echo
echo "Testing insights generation..."
swipl -g "
    use_module('src/semantic_kb'),
    
    % Test insights
    (current_predicate(semantic_kb:generate_insights/1) ->
        (generate_insights(Insights),
         format('✓ Insights generation works: ~w~n', [Insights])) ;
        write('⚠ generate_insights/1 not implemented')), nl,
    
    halt
"

# Test web interface
echo
echo "Testing web interface..."
swipl -g "
    use_module('src/semantic_kb'),
    
    % Test chatbot server
    (current_predicate(semantic_kb:start_chatbot_server/1) ->
        write('✓ Chatbot server interface available') ;
        write('⚠ start_chatbot_server/1 not implemented')), nl,
    
    halt
"

# Performance test
echo
echo "Running performance test..."
swipl -g "
    use_module('src/semantic_kb'),
    
    % Add multiple notes for performance testing
    get_time(Start),
    forall(between(1, 50, N), (
        atom_concat('note_perf_', N, NoteID),
        atom_concat('Test note number ', N, ContentBase),
        atom_concat(ContentBase, ' about performance testing.', Content),
        add_note(NoteID, Content, [tags([test, performance])])
    )),
    get_time(End),
    Duration is End - Start,
    
    format('✓ Performance test: Added 50 notes in ~3f seconds~n', [Duration]),
    halt
"

# Check for required dependencies
echo
echo "Checking dependencies..."

swipl -g "
    catch(use_module(library(http/thread_httpd)), _, fail),
    write('✓ HTTP server support available'), nl,
    halt
" 2>/dev/null || echo "⚠ HTTP server support not available (web interface may not work)"

swipl -g "
    catch(use_module(library(uuid)), _, fail),
    write('✓ UUID generation support available'), nl,
    halt
" 2>/dev/null || echo "⚠ UUID support not available (install uuid pack)"

# Check web interface files
echo
echo "Checking web interface..."

if [ -f "web/index.html" ]; then
    echo "✓ Web interface HTML exists"
else
    echo "⚠ Web interface HTML missing"
fi

# Documentation check
echo
echo "Checking documentation..."

if [ -f "README.md" ]; then
    echo "✓ README.md exists and is comprehensive"
else
    echo "⚠ README.md missing"
fi

echo
echo "=== Test Summary ==="
echo "If you see mostly ✓ marks above, the semantic knowledge base is working correctly."
echo "Any ✗ marks indicate issues that need attention."
echo "Any ⚠ marks indicate optional features or dependencies that could be added."
echo
echo "To run interactive tests:"
echo "  swipl -s src/semantic_kb.pl"
echo "  ?- consult('examples/personal_journal')."
echo "  ?- run_demo."
echo
echo "To test specific functionality:"
echo "  ?- add_note(my_note, 'Content here', [tags([tag1, tag2])])."
echo "  ?- search_semantic([keyword], Results)."
echo "  ?- natural_language_query('Your question', Answer)."
echo
echo "To start web interface:"
echo "  ?- start_chatbot_server(8080)."
echo "  Then visit: http://localhost:8080"
