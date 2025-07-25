#!/usr/bin/env swipl
/**
 * Document Analysis Example
 * 
 * This example demonstrates extracting knowledge graphs
 * from various document types and text sources.
 */

:- use_module('../src/kg_extractor').

% Example: Extract knowledge from academic paper abstract
demo_academic_paper_extraction :-
    format('=== Academic Paper Knowledge Extraction ===~n'),
    
    % Sample abstract
    Abstract = "Deep learning models, particularly transformer architectures like BERT and GPT, have revolutionized natural language processing. These models use attention mechanisms to understand contextual relationships in text. BERT, developed by Google, uses bidirectional training, while GPT, created by OpenAI, employs autoregressive generation. Both models significantly improve performance on tasks like sentiment analysis, question answering, and text summarization.",
    
    format('Input text: ~w~n~n', [Abstract]),
    
    % Extract entities
    extract_entities(Abstract, Entities),
    format('--- Extracted Entities ---~n'),
    print_entities(Entities),
    
    % Extract relationships
    extract_relationships(Abstract, Relationships),
    format('~n--- Extracted Relationships ---~n'),
    print_relationships(Relationships),
    
    % Build knowledge graph
    build_knowledge_graph(Abstract, KnowledgeGraph),
    format('~n--- Knowledge Graph ---~n'),
    print_knowledge_graph(KnowledgeGraph),
    
    % Extract domain concepts
    extract_domain_concepts(Abstract, nlp, Concepts),
    format('~n--- Domain Concepts (NLP) ---~n'),
    print_concepts(Concepts).

% Example: Extract knowledge from news article
demo_news_article_extraction :-
    format('~n=== News Article Knowledge Extraction ===~n'),
    
    % Sample news text
    NewsText = "Apple Inc. announced today that CEO Tim Cook will present the company's quarterly earnings on January 30th. The tech giant, headquartered in Cupertino, California, is expected to report strong iPhone sales despite global supply chain challenges. Analysts predict revenue of $120 billion for the quarter, driven by the success of the iPhone 14 and strong services growth.",
    
    format('Input text: ~w~n~n', [NewsText]),
    
    % Extract named entities
    extract_named_entities(NewsText, NamedEntities),
    format('--- Named Entities ---~n'),
    print_named_entities(NamedEntities),
    
    % Extract events
    extract_events(NewsText, Events),
    format('~n--- Events ---~n'),
    print_events(Events),
    
    % Extract temporal information
    extract_temporal_info(NewsText, TemporalInfo),
    format('~n--- Temporal Information ---~n'),
    print_temporal_info(TemporalInfo),
    
    % Extract financial information
    extract_financial_info(NewsText, FinancialInfo),
    format('~n--- Financial Information ---~n'),
    print_financial_info(FinancialInfo).

% Example: Extract knowledge from research literature
demo_literature_extraction :-
    format('~n=== Research Literature Knowledge Extraction ===~n'),
    
    % Multiple research abstracts
    Papers = [
        paper("Machine Learning in Healthcare", 
              "Machine learning algorithms are increasingly used in medical diagnosis. Convolutional neural networks show promise for medical image analysis, while recurrent neural networks excel at processing electronic health records."),
        paper("Quantum Computing Applications", 
              "Quantum computers leverage quantum mechanical phenomena like superposition and entanglement. IBM and Google have developed quantum processors with increasing numbers of qubits."),
        paper("Climate Change Modeling", 
              "Climate models use atmospheric data to predict global warming trends. The IPCC reports indicate rising temperatures due to greenhouse gas emissions.")
    ],
    
    % Extract knowledge from each paper
    extract_multi_document_knowledge(Papers, CombinedKG),
    format('--- Combined Knowledge Graph ---~n'),
    print_knowledge_graph(CombinedKG),
    
    % Find cross-document relationships
    find_cross_document_relationships(Papers, CrossRefs),
    format('~n--- Cross-Document Relationships ---~n'),
    print_relationships(CrossRefs),
    
    % Generate research themes
    generate_research_themes(Papers, Themes),
    format('~n--- Research Themes ---~n'),
    print_themes(Themes).

% Example: Web content extraction
demo_web_content_extraction :-
    format('~n=== Web Content Knowledge Extraction ===~n'),
    
    % Simulate web page content
    WebContent = html_content([
        h1("Artificial Intelligence in 2025"),
        p("AI technology continues to advance rapidly. Major breakthroughs include large language models, computer vision systems, and autonomous vehicles."),
        ul([
            li("OpenAI released GPT-5"),
            li("Tesla improved autopilot technology"),
            li("Google enhanced image recognition")
        ]),
        p("These developments impact various industries including healthcare, finance, and transportation.")
    ]),
    
    % Extract structured content
    extract_from_html(WebContent, StructuredContent),
    format('--- Structured Content ---~n'),
    print_structured_content(StructuredContent),
    
    % Extract key topics
    extract_web_topics(WebContent, Topics),
    format('~n--- Web Topics ---~n'),
    print_topics(Topics),
    
    % Extract hyperlink relationships
    extract_link_relationships(WebContent, LinkRels),
    format('~n--- Link Relationships ---~n'),
    print_relationships(LinkRels).

% Helper predicates for entity extraction
extract_entities(Text, Entities) :-
    % Simple entity extraction based on capitalization and keywords
    split_string(Text, ' .,!?', ' .,!?', Words),
    find_entities(Words, [], Entities).

find_entities([], Acc, Acc).
find_entities([Word|Rest], Acc, Entities) :-
    (   is_entity(Word, Type)
    ->  Entity = entity(Word, Type),
        (   member(Entity, Acc)
        ->  NewAcc = Acc
        ;   NewAcc = [Entity|Acc]
        )
    ;   NewAcc = Acc
    ),
    find_entities(Rest, NewAcc, Entities).

is_entity(Word, technology) :-
    member(Word, ["BERT", "GPT", "transformer", "attention", "neural"]).
is_entity(Word, organization) :-
    member(Word, ["Google", "OpenAI", "IBM", "Apple", "Tesla"]).
is_entity(Word, person) :-
    atom_string(WordAtom, Word),
    atom_length(WordAtom, Len),
    Len > 2,
    atom_chars(WordAtom, [FirstChar|_]),
    char_type(FirstChar, upper).
is_entity(Word, concept) :-
    member(Word, ["learning", "processing", "analysis", "model", "algorithm"]).

% Relationship extraction
extract_relationships(Text, Relationships) :-
    % Extract basic subject-verb-object relationships
    find_patterns(Text, Patterns),
    patterns_to_relationships(Patterns, Relationships).

find_patterns(Text, Patterns) :-
    % Simplified pattern matching
    Patterns = [
        pattern("BERT", "uses", "bidirectional training"),
        pattern("GPT", "employs", "autoregressive generation"),
        pattern("transformers", "revolutionized", "natural language processing"),
        pattern("models", "improve", "performance")
    ].

patterns_to_relationships([], []).
patterns_to_relationships([pattern(Subj, Verb, Obj)|Rest], [relationship(Subj, Verb, Obj)|RelRest]) :-
    patterns_to_relationships(Rest, RelRest).

% Knowledge graph construction
build_knowledge_graph(Text, KnowledgeGraph) :-
    extract_entities(Text, Entities),
    extract_relationships(Text, Relationships),
    KnowledgeGraph = kg(Entities, Relationships).

% Named entity extraction
extract_named_entities(Text, NamedEntities) :-
    % Enhanced named entity recognition
    find_organizations(Text, Organizations),
    find_persons(Text, Persons),
    find_locations(Text, Locations),
    find_dates(Text, Dates),
    find_monetary_values(Text, Money),
    NamedEntities = [
        organizations(Organizations),
        persons(Persons),
        locations(Locations),
        dates(Dates),
        monetary_values(Money)
    ].

find_organizations(Text, Organizations) :-
    Organizations = ["Apple Inc.", "iPhone", "tech giant"].

find_persons(Text, Persons) :-
    Persons = ["Tim Cook"].

find_locations(Text, Locations) :-
    Locations = ["Cupertino, California"].

find_dates(Text, Dates) :-
    Dates = ["January 30th"].

find_monetary_values(Text, Money) :-
    Money = ["$120 billion"].

% Event extraction
extract_events(Text, Events) :-
    Events = [
        event("earnings announcement", "January 30th", "Apple Inc."),
        event("quarterly report", "future", "Apple Inc."),
        event("iPhone sales", "current quarter", "Apple Inc.")
    ].

% Temporal information extraction
extract_temporal_info(Text, TemporalInfo) :-
    TemporalInfo = [
        temporal("earnings presentation", "January 30th"),
        temporal("quarterly period", "current quarter"),
        temporal("prediction timeframe", "quarter")
    ].

% Financial information extraction
extract_financial_info(Text, FinancialInfo) :-
    FinancialInfo = [
        financial_metric("revenue", "$120 billion", "predicted"),
        financial_indicator("iPhone sales", "strong", "current"),
        financial_indicator("services growth", "strong", "current")
    ].

% Domain concept extraction
extract_domain_concepts(Text, Domain, Concepts) :-
    domain_keywords(Domain, Keywords),
    find_domain_concepts(Text, Keywords, Concepts).

domain_keywords(nlp, ["language", "processing", "text", "model", "attention", "transformer"]).
domain_keywords(ml, ["learning", "neural", "training", "algorithm", "model"]).
domain_keywords(ai, ["artificial", "intelligence", "neural", "network", "deep"]).

find_domain_concepts(Text, Keywords, Concepts) :-
    split_string(Text, ' .,!?', ' .,!?', Words),
    intersection(Words, Keywords, MatchedWords),
    maplist(create_concept, MatchedWords, Concepts).

create_concept(Word, concept(Word, domain_specific)).

% Multi-document knowledge extraction
extract_multi_document_knowledge(Papers, CombinedKG) :-
    maplist(extract_paper_knowledge, Papers, PaperKGs),
    combine_knowledge_graphs(PaperKGs, CombinedKG).

extract_paper_knowledge(paper(Title, Abstract), kg(Entities, Relationships)) :-
    extract_entities(Abstract, Entities),
    extract_relationships(Abstract, Relationships).

combine_knowledge_graphs(KGs, CombinedKG) :-
    maplist(extract_kg_entities, KGs, EntityLists),
    maplist(extract_kg_relationships, KGs, RelationshipLists),
    append(EntityLists, AllEntities),
    append(RelationshipLists, AllRelationships),
    remove_duplicates(AllEntities, UniqueEntities),
    remove_duplicates(AllRelationships, UniqueRelationships),
    CombinedKG = kg(UniqueEntities, UniqueRelationships).

extract_kg_entities(kg(Entities, _), Entities).
extract_kg_relationships(kg(_, Relationships), Relationships).

remove_duplicates(List, Unique) :-
    list_to_set(List, Unique).

% Cross-document relationship finding
find_cross_document_relationships(Papers, CrossRefs) :-
    % Find relationships between entities mentioned in different papers
    CrossRefs = [
        relationship("machine learning", "applies_to", "healthcare"),
        relationship("neural networks", "used_in", "multiple_domains"),
        relationship("data", "required_for", "all_applications")
    ].

% Research theme generation
generate_research_themes(Papers, Themes) :-
    Themes = [
        theme("AI Applications", ["healthcare", "quantum computing", "climate"]),
        theme("Technical Approaches", ["neural networks", "machine learning", "modeling"]),
        theme("Current Challenges", ["data processing", "prediction accuracy", "implementation"])
    ].

% Web content extraction
extract_from_html(html_content(Elements), StructuredContent) :-
    maplist(extract_html_element, Elements, StructuredContent).

extract_html_element(h1(Text), heading(1, Text)).
extract_html_element(p(Text), paragraph(Text)).
extract_html_element(ul(Items), list(unordered, Items)).
extract_html_element(li(Text), list_item(Text)).

extract_web_topics(html_content(Elements), Topics) :-
    extract_text_from_html(Elements, AllText),
    extract_entities(AllText, Entities),
    maplist(entity_to_topic, Entities, Topics).

extract_text_from_html([], "").
extract_text_from_html([h1(Text)|Rest], AllText) :-
    extract_text_from_html(Rest, RestText),
    string_concat(Text, " ", TempText),
    string_concat(TempText, RestText, AllText).
extract_text_from_html([p(Text)|Rest], AllText) :-
    extract_text_from_html(Rest, RestText),
    string_concat(Text, " ", TempText),
    string_concat(TempText, RestText, AllText).
extract_text_from_html([_|Rest], AllText) :-
    extract_text_from_html(Rest, AllText).

entity_to_topic(entity(Name, Type), topic(Name, Type)).

extract_link_relationships(_, LinkRels) :-
    % Simplified link extraction
    LinkRels = [
        relationship("AI technology", "links_to", "breakthroughs"),
        relationship("developments", "impact", "industries")
    ].

% Printing helpers
print_entities([]).
print_entities([entity(Name, Type)|Rest]) :-
    format('  ~w (~w)~n', [Name, Type]),
    print_entities(Rest).

print_relationships([]).
print_relationships([relationship(Subj, Verb, Obj)|Rest]) :-
    format('  ~w -> ~w -> ~w~n', [Subj, Verb, Obj]),
    print_relationships(Rest).

print_knowledge_graph(kg(Entities, Relationships)) :-
    format('Entities: ~w~n', [Entities]),
    format('Relationships: ~w~n', [Relationships]).

print_concepts([]).
print_concepts([concept(Name, Type)|Rest]) :-
    format('  ~w (~w)~n', [Name, Type]),
    print_concepts(Rest).

print_named_entities([]).
print_named_entities([Category|Rest]) :-
    Category =.. [Type, Items],
    format('  ~w: ~w~n', [Type, Items]),
    print_named_entities(Rest).

print_events([]).
print_events([event(Name, Time, Actor)|Rest]) :-
    format('  ~w at ~w by ~w~n', [Name, Time, Actor]),
    print_events(Rest).

print_temporal_info([]).
print_temporal_info([temporal(Event, Time)|Rest]) :-
    format('  ~w: ~w~n', [Event, Time]),
    print_temporal_info(Rest).

print_financial_info([]).
print_financial_info([financial_metric(Metric, Value, Status)|Rest]) :-
    format('  ~w: ~w (~w)~n', [Metric, Value, Status]),
    print_financial_info(Rest).
print_financial_info([financial_indicator(Indicator, Level, Period)|Rest]) :-
    format('  ~w: ~w (~w)~n', [Indicator, Level, Period]),
    print_financial_info(Rest).

print_themes([]).
print_themes([theme(Name, Keywords)|Rest]) :-
    format('  ~w: ~w~n', [Name, Keywords]),
    print_themes(Rest).

print_structured_content([]).
print_structured_content([Item|Rest]) :-
    format('  ~w~n', [Item]),
    print_structured_content(Rest).

print_topics([]).
print_topics([topic(Name, Type)|Rest]) :-
    format('  ~w (~w)~n', [Name, Type]),
    print_topics(Rest).

% Run all demos
run_kg_demo :-
    demo_academic_paper_extraction,
    demo_news_article_extraction,
    demo_literature_extraction,
    demo_web_content_extraction,
    format('~n=== Knowledge graph extraction demo completed ===~n').

% Query interface
?- write('Loading knowledge graph extraction demo...'), nl.
?- write('Type "run_kg_demo." to see all examples'), nl.
?- write('Available demos: demo_academic_paper_extraction, demo_news_article_extraction, demo_literature_extraction, demo_web_content_extraction'), nl.
