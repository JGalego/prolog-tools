#!/usr/bin/env swipl
/**
 * Research Notes Management Example
 * 
 * This example demonstrates using the semantic knowledge base
 * for academic research and literature management.
 */

:- use_module('../src/semantic_kb').

% Example: Research paper management
demo_research_management :-
    format('=== Research Management Demo ===~n'),
    
    % Add research papers
    add_note(paper_001,
             "Deep Learning for Natural Language Processing - Comprehensive survey of transformer architectures",
             [tags([deep_learning, nlp, transformers, survey]),
              authors(['Smith, J.', 'Doe, A.']),
              year(2023),
              venue('Nature Machine Intelligence'),
              category(research_paper),
              impact_factor(25.3)]),
    
    add_note(paper_002,
             "BERT: Pre-training of Deep Bidirectional Transformers for Language Understanding",
             [tags([bert, transformers, nlp, pretraining]),
              authors(['Devlin, J.', 'Chang, M.']),
              year(2019),
              venue('NAACL'),
              category(research_paper),
              citations(15000)]),
    
    add_note(paper_003,
             "Attention Is All You Need - Introduced the Transformer architecture",
             [tags([transformers, attention, architecture]),
              authors(['Vaswani, A.', 'Shazeer, N.']),
              year(2017),
              venue('NIPS'),
              category(research_paper),
              citations(50000)]),
    
    % Add research ideas/notes
    add_note(idea_001,
             "Could transformer attention mechanisms be applied to graph neural networks?",
             [tags([research_idea, transformers, gnn, attention]),
              date(2025, 1, 18),
              category(research_idea),
              priority(high)]),
    
    add_note(todo_001,
             "Implement BERT fine-tuning for sentiment analysis dataset",
             [tags([todo, bert, sentiment_analysis, implementation]),
              date(2025, 1, 19),
              category(todo),
              deadline(date(2025, 2, 1))]),
    
    % Create relationships between papers
    link_notes(paper_003, paper_002, influences),  % Transformer -> BERT
    link_notes(paper_002, paper_001, cited_in),    % BERT cited in survey
    link_notes(paper_002, idea_001, inspires),     % BERT inspires idea
    link_notes(idea_001, todo_001, leads_to),      % Idea leads to todo
    
    % Tag notes for organization
    tag_note(paper_001, must_read),
    tag_note(paper_002, implemented),
    tag_note(paper_003, foundational),
    tag_note(idea_001, future_work),
    
    format('Added research papers and notes.~n').

% Query research database
demo_research_queries :-
    format('~n--- Research Query Examples ---~n'),
    
    % Find papers by topic
    format('1. Papers about transformers:~n'),
    search_semantic([transformers], TransformerPapers),
    print_research_results(TransformerPapers),
    
    % Find highly cited papers
    format('~n2. High-impact papers:~n'),
    query_notes(note(ID, Content, Metadata, _), HighImpactPapers),
    filter_high_impact(HighImpactPapers, FilteredPapers),
    print_research_results(FilteredPapers),
    
    % Find research ideas
    format('~n3. Research ideas to explore:~n'),
    query_notes(note(ID, Content, Metadata, _), AllNotes),
    filter_by_category(AllNotes, research_idea, Ideas),
    print_research_results(Ideas),
    
    % Citation network analysis
    format('~n4. Citation relationships:~n'),
    analyze_citation_network.

% Literature review helper
demo_literature_review :-
    format('~n=== Literature Review Generation ===~n'),
    
    % Generate thematic clusters
    find_research_themes(Themes),
    format('Research themes identified:~n'),
    print_themes(Themes),
    
    % Generate bibliography
    format('~n--- Bibliography ---~n'),
    generate_bibliography(Bibliography),
    print_bibliography(Bibliography),
    
    % Identify research gaps
    format('~n--- Research Gaps ---~n'),
    identify_research_gaps(Gaps),
    print_gaps(Gaps).

% Research collaboration features
demo_collaboration :-
    format('~n=== Research Collaboration ===~n'),
    
    % Add collaborator notes
    add_note(collab_001,
             "Meeting with Dr. Smith about transformer efficiency research",
             [tags([meeting, collaboration, efficiency]),
              date(2025, 1, 20),
              participants(['Dr. Smith', 'Prof. Johnson']),
              category(meeting_notes)]),
    
    % Track shared resources
    add_note(resource_001,
             "Shared dataset: Common Crawl preprocessed for transformer training",
             [tags([dataset, shared_resource, transformers]),
              shared_by('Dr. Smith'),
              location('/shared/datasets/common_crawl_processed'),
              category(shared_resource)]),
    
    format('Added collaboration information.~n').

% Helper predicates
print_research_results([]).
print_research_results([note(ID, Content, Metadata, _)|Rest]) :-
    format('  [~w] ~w~n', [ID, Content]),
    extract_key_metadata(Metadata, KeyInfo),
    format('    ~w~n', [KeyInfo]),
    print_research_results(Rest).

extract_key_metadata(Metadata, KeyInfo) :-
    (   member(authors(Authors), Metadata)
    ->  KeyInfo = authors(Authors)
    ;   member(tags(Tags), Metadata)
    ->  KeyInfo = tags(Tags)
    ;   KeyInfo = 'No key metadata'
    ).

filter_high_impact([], []).
filter_high_impact([note(ID, Content, Metadata, Time)|Rest], [note(ID, Content, Metadata, Time)|Filtered]) :-
    (   member(citations(Citations), Metadata), Citations > 10000
    ;   member(impact_factor(IF), Metadata), IF > 20
    ), !,
    filter_high_impact(Rest, Filtered).
filter_high_impact([_|Rest], Filtered) :-
    filter_high_impact(Rest, Filtered).

filter_by_category([], _, []).
filter_by_category([note(ID, Content, Metadata, Time)|Rest], Category, [note(ID, Content, Metadata, Time)|Filtered]) :-
    member(category(Category), Metadata), !,
    filter_by_category(Rest, Category, Filtered).
filter_by_category([_|Rest], Category, Filtered) :-
    filter_by_category(Rest, Category, Filtered).

analyze_citation_network :-
    findall(link(From, To, Type), note_link(From, To, Type), Links),
    format('Citation network:~n'),
    print_links(Links).

print_links([]).
print_links([link(From, To, Type)|Rest]) :-
    format('  ~w -[~w]-> ~w~n', [From, Type, To]),
    print_links(Rest).

find_research_themes(themes([
    theme('Transformer Architectures', [transformers, attention, architecture]),
    theme('Natural Language Processing', [nlp, bert, language_understanding]),
    theme('Deep Learning Applications', [deep_learning, applications, survey])
])).

print_themes(themes([])).
print_themes(themes([theme(Name, Keywords)|Rest])) :-
    format('  Theme: ~w~n    Keywords: ~w~n', [Name, Keywords]),
    print_themes(themes(Rest)).

generate_bibliography([
    citation('Vaswani et al.', 2017, 'Attention Is All You Need', 'NIPS'),
    citation('Devlin et al.', 2019, 'BERT: Pre-training of Deep Bidirectional Transformers', 'NAACL'),
    citation('Smith & Doe', 2023, 'Deep Learning for Natural Language Processing', 'Nature MI')
]).

print_bibliography([]).
print_bibliography([citation(Authors, Year, Title, Venue)|Rest]) :-
    format('  ~w (~w). ~w. ~w.~n', [Authors, Year, Title, Venue]),
    print_bibliography(Rest).

identify_research_gaps([
    gap('Limited work on transformer efficiency for mobile devices'),
    gap('Few studies on cross-lingual transformer adaptation'),
    gap('Insufficient research on transformer interpretability')
]).

print_gaps([]).
print_gaps([gap(Description)|Rest]) :-
    format('  - ~w~n', [Description]),
    print_gaps(Rest).

% Run all demos
run_research_demo :-
    demo_research_management,
    demo_research_queries,
    demo_literature_review,
    demo_collaboration,
    format('~n=== Research demo completed ===~n').

% Query interface
?- write('Loading research management demo...'), nl.
?- write('Type "run_research_demo." to see all examples'), nl.
?- write('Available demos: demo_research_management, demo_research_queries, demo_literature_review, demo_collaboration'), nl.
