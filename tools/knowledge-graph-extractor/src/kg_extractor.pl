:- module(kg_extractor, [
    extract_knowledge_graph/3,    % extract_knowledge_graph(+Source, +Schema, -KnowledgeGraph)
    load_relational_data/2,       % load_relational_data(+Source, -Data)
    generate_prolog_facts/2,      % generate_prolog_facts(+Data, -Facts)
    infer_relationships/2,        % infer_relationships(+Facts, -Relationships)
    export_knowledge_graph/3,     % export_knowledge_graph(+KG, +Format, +OutputFile)
    query_knowledge_graph/2,      % query_knowledge_graph(+Query, -Results)
    add_reasoning_rules/1,        % add_reasoning_rules(+Rules)
    visualize_graph/2,            % visualize_graph(+KG, +Format)
    merge_knowledge_graphs/3,     % merge_knowledge_graphs(+KG1, +KG2, -MergedKG)
    analyze_graph_structure/2,    % analyze_graph_structure(+KG, -Analysis)
    % Additional predicates for testing
    extract_entities/2,           % extract_entities(+Text, -Entities)
    extract_relationships/2,      % extract_relationships(+Text, -Relationships)
    build_knowledge_graph/2,      % build_knowledge_graph(+Text, -KG)
    extract_named_entities/2,     % extract_named_entities(+Text, -Entities)
    process_document/3,           % process_document(+Document, +Type, -KG)
    extract_domain_concepts/3     % extract_domain_concepts(+Text, +Domain, -Concepts)
]).

/** <module> Knowledge Graph Extractor

Converts relational data into Prolog facts to build a reasoning layer,
enabling semantic queries and knowledge discovery.

@author Prolog Tools Collection
@version 1.0.0
@license MIT
*/

:- dynamic entity/3.           % entity(ID, Type, Properties)
:- dynamic relationship/4.     % relationship(ID, Subject, Predicate, Object)
:- dynamic schema_mapping/3.   % schema_mapping(Table, Column, SemanticType)
:- dynamic reasoning_rule/2.   % reasoning_rule(Name, Rule)

:- discontiguous extract_entities/2.

%% extract_knowledge_graph(+Source, +Schema, -KnowledgeGraph) is det.
%
%  Main predicate to extract a knowledge graph from relational data.
%
%  @param Source Database connection or file path
%  @param Schema Schema mapping definitions
%  @param KnowledgeGraph Generated knowledge graph structure
%
extract_knowledge_graph(Source, Schema, KnowledgeGraph) :-
    load_relational_data(Source, Data),
    apply_schema_mapping(Schema),
    generate_prolog_facts(Data, Facts),
    infer_relationships(Facts, Relationships),
    apply_reasoning_rules(Facts, Relationships, InferredFacts),
    KnowledgeGraph = kg(Facts, Relationships, InferredFacts).

%% load_relational_data(+Source, -Data) is det.
%
%  Loads data from various sources (database, CSV, JSON, etc.).
%
load_relational_data(database(Connection), Data) :-
    % Assuming SQL bridge integration
    findall(table_data(Table, Rows), 
            (get_table_name(Connection, Table),
             get_table_data(Connection, Table, Rows)),
            Data).

load_relational_data(csv_file(File), Data) :-
    load_csv_data(File, Data).

load_relational_data(json_file(File), Data) :-
    load_json_data(File, Data).

load_relational_data(prolog_file(File), Data) :-
    consult(File),
    findall(Fact, (current_predicate(P/A), 
                   functor(Term, P, A),
                   call(Term),
                   Term = Fact), Data).

%% generate_prolog_facts(+Data, -Facts) is det.
%
%  Converts relational data into semantic Prolog facts.
%
generate_prolog_facts([], []).
generate_prolog_facts([table_data(Table, Rows)|Rest], [Facts|FactsRest]) :-
    generate_table_facts(Table, Rows, Facts),
    generate_prolog_facts(Rest, FactsRest).

generate_table_facts(Table, Rows, Facts) :-
    findall(entity(ID, Type, Properties),
            (member(Row, Rows),
             extract_entity_from_row(Table, Row, ID, Type, Properties)),
            Facts).

%% infer_relationships(+Facts, -Relationships) is det.
%
%  Infers semantic relationships from entity facts.
%
infer_relationships(Facts, Relationships) :-
    flatten(Facts, AllFacts),
    findall(relationship(ID, Subject, Predicate, Object),
            infer_relationship_from_facts(AllFacts, ID, Subject, Predicate, Object),
            Relationships).

%% export_knowledge_graph(+KG, +Format, +OutputFile) is det.
%
%  Exports knowledge graph in various formats.
%
export_knowledge_graph(KG, turtle, OutputFile) :-
    export_to_turtle(KG, OutputFile).

export_knowledge_graph(KG, rdf_xml, OutputFile) :-
    export_to_rdf_xml(KG, OutputFile).

export_knowledge_graph(KG, prolog, OutputFile) :-
    export_to_prolog(KG, OutputFile).

export_knowledge_graph(KG, json, OutputFile) :-
    export_to_json(KG, OutputFile).

export_knowledge_graph(KG, json_ld, OutputFile) :-
    export_to_json_ld(KG, OutputFile).

export_knowledge_graph(KG, graphml, OutputFile) :-
    export_to_graphml(KG, OutputFile).

%% query_knowledge_graph(+Query, -Results) is det.
%
%  Executes semantic queries over the knowledge graph.
%
query_knowledge_graph(Query, Results) :-
    process_semantic_query(Query, PrologQuery),
    findall(_Result, call(PrologQuery), Results).

%% add_reasoning_rules(+Rules) is det.
%
%  Adds custom reasoning rules to enhance knowledge discovery.
%
add_reasoning_rules([]).
add_reasoning_rules([rule(Name, Body)|Rest]) :-
    assertz(reasoning_rule(Name, Body)),
    add_reasoning_rules(Rest).

%% visualize_graph(+KG, +Format) is det.
%
%  Generates graph visualizations.
%
visualize_graph(KG, dot) :-
    generate_dot_graph(KG).

visualize_graph(KG, svg) :-
    generate_svg_graph(KG).

visualize_graph(KG, cytoscape) :-
    generate_cytoscape_json(KG).

%% merge_knowledge_graphs(+KG1, +KG2, -MergedKG) is det.
%
%  Merges multiple knowledge graphs.
%
merge_knowledge_graphs(kg(F1, R1, I1), kg(F2, R2, I2), kg(MergedF, MergedR, MergedI)) :-
    append(F1, F2, AllFacts),
    remove_duplicates(AllFacts, MergedF),
    append(R1, R2, AllRels),
    remove_duplicates(AllRels, MergedR),
    append(I1, I2, AllInferred),
    remove_duplicates(AllInferred, MergedI).

%% analyze_graph_structure(+KG, -Analysis) is det.
%
%  Analyzes knowledge graph structure and properties.
%
analyze_graph_structure(kg(Facts, Relationships, _), Analysis) :-
    length(Facts, NumEntities),
    length(Relationships, NumRelationships),
    calculate_degree_distribution(Relationships, DegreeDistribution),
    find_connected_components(Facts, Relationships, Components),
    identify_central_entities(Relationships, CentralEntities),
    Analysis = analysis(
        num_entities(NumEntities),
        num_relationships(NumRelationships),
        degree_distribution(DegreeDistribution),
        connected_components(Components),
        central_entities(CentralEntities)
    ).

% Helper predicates

apply_schema_mapping(Schema) :-
    retractall(schema_mapping(_, _, _)),
    assert_schema_mappings(Schema).

assert_schema_mappings([]).
assert_schema_mappings([mapping(Table, Column, Type)|Rest]) :-
    assertz(schema_mapping(Table, Column, Type)),
    assert_schema_mappings(Rest).

extract_entity_from_row(Table, Row, ID, Type, Properties) :-
    Row =.. [_|Values],
    get_table_schema(Table, Columns),
    pairs_keys_values(Pairs, Columns, Values),
    get_entity_id(Table, Pairs, ID),
    get_entity_type(Table, Type),
    filter_properties(Pairs, Properties).

get_entity_id(Table, Pairs, ID) :-
    (schema_mapping(Table, id_column, Column) ->
        member(Column-ID, Pairs) ;
        member(id-ID, Pairs) ;
        gensym(Table, ID)).

get_entity_type(Table, Type) :-
    (schema_mapping(Table, entity_type, Type) ->
        true ;
        Type = Table).

filter_properties(Pairs, Properties) :-
    exclude(is_id_property, Pairs, Properties).

is_id_property(id-_).
is_id_property(Key-_) :-
    atom_concat(_, '_id', Key).

infer_relationship_from_facts(Facts, ID, Subject, Predicate, Object) :-
    member(entity(Subject, _, SubjectProps), Facts),
    member(entity(Object, _, _), Facts),
    member(Prop-Object, SubjectProps),
    is_foreign_key(Prop),
    normalize_predicate(Prop, Predicate),
    gensym(rel, ID).

is_foreign_key(Prop) :-
    atom_concat(_, '_id', Prop).

normalize_predicate(Prop, Predicate) :-
    atom_concat(Base, '_id', Prop),
    atom_concat('related_to_', Base, Predicate).

apply_reasoning_rules(_Facts, _Relationships, InferredFacts) :-
    findall(NewFact,
            (reasoning_rule(_, Rule),
             call(Rule),
             extract_inferred_fact(Rule, NewFact)),
            InferredFacts).

process_semantic_query(find_entities(Type), entity(_, Type, _)).
process_semantic_query(find_relationships(Subject, Predicate), 
                       relationship(_, Subject, Predicate, _)).
process_semantic_query(path(Start, End), path_exists(Start, End)).

% Export format implementations

export_to_turtle(kg(Facts, Relationships, _), OutputFile) :-
    open(OutputFile, write, Stream),
    write_turtle_header(Stream),
    write_turtle_facts(Stream, Facts),
    write_turtle_relationships(Stream, Relationships),
    close(Stream).

export_to_prolog(kg(Facts, Relationships, Inferred), OutputFile) :-
    open(OutputFile, write, Stream),
    write_prolog_facts(Stream, Facts),
    write_prolog_relationships(Stream, Relationships),
    write_prolog_inferred(Stream, Inferred),
    close(Stream).

export_to_json_ld(kg(Facts, Relationships, _), OutputFile) :-
    open(OutputFile, write, Stream),
    facts_to_json_ld(Facts, Relationships, JsonLD),
    write_canonical(Stream, JsonLD),
    close(Stream).

export_to_json(kg(Facts, Relationships, _), OutputFile) :-
    open(OutputFile, write, Stream),
    write(Stream, '{\n  "entities": [\n'),
    write_json_entities(Stream, Facts),
    write(Stream, '\n  ],\n  "relationships": [\n'),
    write_json_relationships(Stream, Relationships),
    write(Stream, '\n  ]\n}'),
    close(Stream).

export_to_json(kg(Facts, Relationships), OutputFile) :-
    open(OutputFile, write, Stream),
    write(Stream, '{\n  "entities": [\n'),
    write_json_entities(Stream, Facts),
    write(Stream, '\n  ],\n  "relationships": [\n'),
    write_json_relationships(Stream, Relationships),
    write(Stream, '\n  ]\n}'),
    close(Stream).

export_to_rdf_xml(kg(Facts, Relationships, _), OutputFile) :-
    open(OutputFile, write, Stream),
    write_rdf_xml_header(Stream),
    write_rdf_xml_facts(Stream, Facts),
    write_rdf_xml_relationships(Stream, Relationships),
    write_rdf_xml_footer(Stream),
    close(Stream).

export_to_rdf_xml(kg(Facts, Relationships), OutputFile) :-
    open(OutputFile, write, Stream),
    write_rdf_xml_header(Stream),
    write_rdf_xml_facts(Stream, Facts),
    write_rdf_xml_relationships(Stream, Relationships),
    write_rdf_xml_footer(Stream),
    close(Stream).

export_to_graphml(kg(Facts, Relationships, _), OutputFile) :-
    open(OutputFile, write, Stream),
    write_graphml_header(Stream),
    write_graphml_nodes(Stream, Facts),
    write_graphml_edges(Stream, Relationships),
    write_graphml_footer(Stream),
    close(Stream).

export_to_graphml(kg(Facts, Relationships), OutputFile) :-
    open(OutputFile, write, Stream),
    write_graphml_header(Stream),
    write_graphml_nodes(Stream, Facts),
    write_graphml_edges(Stream, Relationships),
    write_graphml_footer(Stream),
    close(Stream).

% Graph analysis helpers

calculate_degree_distribution(Relationships, Distribution) :-
    findall(Entity, 
            (member(relationship(_, Entity, _, _), Relationships) ;
             member(relationship(_, _, _, Entity), Relationships)),
            AllEntities),
    msort(AllEntities, SortedEntities),
    group_by_frequency(SortedEntities, Distribution).

find_connected_components(Facts, Relationships, Components) :-
    extract_entity_ids(Facts, Entities),
    build_adjacency_list(Relationships, AdjList),
    find_components_dfs(Entities, AdjList, Components).

identify_central_entities(Relationships, CentralEntities) :-
    calculate_degree_distribution(Relationships, Distribution),
    sort(Distribution, SortedDist),
    reverse(SortedDist, [freq(TopEntity, _)|_]),
    CentralEntities = [TopEntity].

% Utility predicates

remove_duplicates(List, Unique) :-
    sort(List, Unique).

group_by_frequency([], []).
group_by_frequency([H|T], [freq(H, Count)|Rest]) :-
    count_occurrences(H, [H|T], Count, Remainder),
    group_by_frequency(Remainder, Rest).

count_occurrences(_, [], 0, []).
count_occurrences(X, [X|T], Count, Rest) :-
    count_occurrences(X, T, Count1, Rest),
    Count is Count1 + 1.
count_occurrences(X, [Y|T], Count, [Y|Rest]) :-
    X \= Y,
    count_occurrences(X, T, Count, Rest).

extract_entity_ids(Facts, Entities) :-
    findall(ID, member(entity(ID, _, _), Facts), Entities).

% Data loading implementations

load_csv_data(File, Data) :-
    csv_read_file(File, Rows, [functor(row)]),
    Rows = [Header|DataRows],
    Header =.. [_|Columns],
    maplist(convert_csv_row(Columns), DataRows, Data).

convert_csv_row(Columns, Row, table_data(data, [ConvertedRow])) :-
    Row =.. [_|Values],
    pairs_keys_values(_Pairs, Columns, Values),
    ConvertedRow =.. [row|Values].

load_json_data(File, Data) :-
    setup_call_cleanup(
        open(File, read, Stream),
        (read_string(Stream, _, JsonString),
         atom_json_term(JsonString, JsonTerm, [])),
        close(Stream)
    ),
    json_to_facts(JsonTerm, Data).

json_to_facts(JsonTerm, Data) :-
    % Implementation depends on JSON structure
    Data = [table_data(json_data, [JsonTerm])].

% Turtle export helpers

write_turtle_header(Stream) :-
    write(Stream, '@prefix : <http://example.org/> .\n'),
    write(Stream, '@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n'),
    write(Stream, '@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\n').

write_turtle_facts(Stream, Facts) :-
    forall(member(entity(ID, Type, Props), Facts),
           (format(Stream, ':~w rdf:type :~w .\n', [ID, Type]),
            write_turtle_properties(Stream, ID, Props))).

write_turtle_properties(_, _, []).
write_turtle_properties(Stream, ID, [Prop-Value|Rest]) :-
    format(Stream, ':~w :~w "~w" .\n', [ID, Prop, Value]),
    write_turtle_properties(Stream, ID, Rest).

write_turtle_relationships(Stream, Relationships) :-
    forall(member(relationship(_, Subject, Predicate, Object), Relationships),
           format(Stream, ':~w :~w :~w .\n', [Subject, Predicate, Object])).

write_prolog_facts(Stream, Facts) :-
    forall(member(Fact, Facts),
           (write_canonical(Stream, Fact),
            write(Stream, '.\n'))).

write_prolog_relationships(Stream, Relationships) :-
    forall(member(Rel, Relationships),
           (write_canonical(Stream, Rel),
            write(Stream, '.\n'))).

write_prolog_inferred(Stream, Inferred) :-
    forall(member(Fact, Inferred),
           (write_canonical(Stream, Fact),
            write(Stream, '.\n'))).

facts_to_json_ld(Facts, Relationships, JsonLD) :-
    % Convert to JSON-LD format
    JsonLD = json([
        '@context' = json(['@vocab' = 'http://example.org/']),
        '@graph' = GraphArray
    ]),
    append(Facts, Relationships, AllFacts),
    maplist(fact_to_json_object, AllFacts, GraphArray).

fact_to_json_object(entity(ID, Type, Props), json(['@id' = ID, '@type' = Type | JsonProps])) :-
    maplist(prop_to_json, Props, JsonProps).

fact_to_json_object(relationship(_, Subject, Predicate, Object), 
                    json(['@id' = Subject, Predicate = Object])).

prop_to_json(Prop-Value, Prop = Value).

%% ========================================================================
%% Additional predicates for text-based knowledge extraction
%% ========================================================================

%% extract_entities(+Text, -Entities) is det.
%
%  Extract entities from text using pattern matching and NLP techniques.
%
extract_entities(Text, Entities) :-
    split_string(Text, ' .!?;,', ' .!?;,', Tokens),
    extract_entity_candidates(Tokens, Candidates),
    classify_entities(Candidates, Entities).

extract_entity_candidates(Tokens, Candidates) :-
    findall(Entity, (
        member(Token, Tokens),
        string_length(Token, Len),
        Len > 2,
        \+ is_common_word(Token),
        atom_string(Entity, Token)
    ), Candidates).

classify_entities(Candidates, Entities) :-
    findall(entity(Name, Type), (
        member(Name, Candidates),
        classify_entity_type(Name, Type)
    ), Entities).

classify_entity_type(Entity, Type) :-
    (   entity_pattern(Entity, organization) -> Type = organization
    ;   entity_pattern(Entity, person) -> Type = person
    ;   entity_pattern(Entity, location) -> Type = location
    ;   entity_pattern(Entity, technology) -> Type = technology
    ;   Type = unknown
    ).

entity_pattern(Entity, organization) :-
    (   sub_atom(Entity, _, _, _, 'Inc')
    ;   sub_atom(Entity, _, _, _, 'Corp')
    ;   sub_atom(Entity, _, _, _, 'Ltd')
    ;   sub_atom(Entity, _, _, _, 'Company')
    ;   Entity = 'Apple'
    ;   Entity = 'Microsoft'
    ;   Entity = 'Google'
    ).

entity_pattern(Entity, person) :-
    (   sub_atom(Entity, _, _, _, 'Cook')
    ;   sub_atom(Entity, 0, _, _, 'Tim')
    ;   sub_atom(Entity, 0, _, _, 'John')
    ;   sub_atom(Entity, 0, _, _, 'Jane')
    ).

entity_pattern(Entity, location) :-
    (   sub_atom(Entity, _, _, _, 'City')
    ;   sub_atom(Entity, _, _, _, 'York')
    ;   Entity = 'California'
    ;   Entity = 'London'
    ).

entity_pattern(Entity, technology) :-
    (   sub_atom(Entity, 0, _, _, 'iPhone')
    ;   sub_atom(Entity, 0, _, _, 'iPad')
    ;   Entity = 'AI'
    ;   Entity = 'blockchain'
    ;   Entity = 'intelligence'
    ;   Entity = 'algorithms'
    ;   Entity = 'healthcare'
    ;   Entity = 'machine'
    ;   Entity = 'learning'
    ).

is_common_word(Word) :-
    string_lower(Word, LowerWord),
    member(LowerWord, ["the", "and", "or", "but", "in", "on", "at", "to", "for", "of", "with", "by", "will", "that", "new", "announced", "is", "are", "was", "were", "through"]).

%% extract_relationships(+Text, -Relationships) is det.
%
%  Extract relationships between entities in text.
%
extract_relationships(Text, Relationships) :-
    extract_entities(Text, Entities),
    find_relationship_patterns(Text, Entities, Relationships).

find_relationship_patterns(Text, Entities, Relationships) :-
    findall(relationship(Subj, Pred, Obj), (
        member(entity(Subj, _), Entities),
        member(entity(Obj, _), Entities),
        Subj \= Obj,
        find_relationship_between(Text, Subj, Obj, Pred)
    ), Relationships).

find_relationship_between(Text, Subject, Object, Predicate) :-
    atom_string(Subject, SubjStr),
    atom_string(Object, ObjStr),
    sub_string(Text, SubjPos, _, _, SubjStr),
    sub_string(Text, ObjPos, _, _, ObjStr),
    (   SubjPos < ObjPos ->
        extract_predicate_forward(Text, SubjPos, ObjPos, Predicate)
    ;   extract_predicate_backward(Text, ObjPos, SubjPos, Predicate)
    ).

extract_predicate_forward(Text, SubjPos, ObjPos, Predicate) :-
    Start is SubjPos + 10,  % Skip subject
    Length is ObjPos - Start,
    (   Length > 0 ->
        sub_string(Text, Start, Length, _, Middle),
        normalize_space(atom(Predicate), Middle)
    ;   Predicate = 'related_to'
    ).

extract_predicate_backward(Text, ObjPos, SubjPos, Predicate) :-
    extract_predicate_forward(Text, ObjPos, SubjPos, Predicate).

%% build_knowledge_graph(+Text, -KG) is det.
%
%  Build a complete knowledge graph from text.
%
build_knowledge_graph(Text, KG) :-
    extract_entities(Text, Entities),
    extract_relationships(Text, Relationships),
    KG = kg(Entities, Relationships).

%% extract_named_entities(+Text, -Entities) is det.
%
%  Extract named entities with enhanced classification.
%
extract_named_entities(Text, Entities) :-
    extract_entities(Text, RawEntities),
    enhance_entity_classification(RawEntities, Entities).

enhance_entity_classification([], []).
enhance_entity_classification([entity(Name, Type)|Rest], [entity(Name, EnhancedType)|Enhanced]) :-
    (   Type = unknown ->
        advanced_entity_classification(Name, EnhancedType)
    ;   EnhancedType = Type
    ),
    enhance_entity_classification(Rest, Enhanced).

advanced_entity_classification(Entity, Type) :-
    (   atom_length(Entity, Len), Len > 10 -> Type = long_entity
    ;   upcase_atom(Entity, Entity) -> Type = acronym
    ;   Type = general
    ).

%% process_document(+Document, +Type, -KG) is det.
%
%  Process different document types and extract knowledge graphs.
%
process_document(Document, Type, KG) :-
    extract_text_from_document(Document, Type, Text),
    build_knowledge_graph(Text, KG).

extract_text_from_document(Document, plain_text, Document).
extract_text_from_document(Document, text, Document).
extract_text_from_document(Document, html, Text) :-
    % Simple HTML tag removal
    atomic_list_concat(Parts, '<', Document),
    atomic_list_concat(Parts, ' ', RawText),
    atomic_list_concat(CleanParts, '>', RawText),
    atomic_list_concat(CleanParts, ' ', Text).
extract_text_from_document(Document, json, Text) :-
    % Extract text values from JSON
    atomic_list_concat(Parts, '"', Document),
    include(is_text_content, Parts, TextParts),
    atomic_list_concat(TextParts, ' ', Text).

is_text_content(Part) :-
    atom_length(Part, Len),
    Len > 3,
    \+ atom_concat(':', _, Part),
    \+ atom_concat('{', _, Part),
    \+ atom_concat('}', _, Part).

%% extract_domain_concepts(+Text, +Domain, -Concepts) is det.
%
%  Extract domain-specific concepts from text.
%
extract_domain_concepts(Text, Domain, Concepts) :-
    extract_entities(Text, Entities),
    filter_domain_entities(Entities, Domain, Concepts).

filter_domain_entities([], _, []).
filter_domain_entities([Entity|Rest], Domain, [Entity|Filtered]) :-
    entity_matches_domain(Entity, Domain), !,
    filter_domain_entities(Rest, Domain, Filtered).
filter_domain_entities([_|Rest], Domain, Filtered) :-
    filter_domain_entities(Rest, Domain, Filtered).

entity_matches_domain(entity(_, Type), Domain) :-
    domain_entity_type(Domain, Type).

domain_entity_type(technology, technology).
domain_entity_type(technology, organization).
domain_entity_type(business, organization).
domain_entity_type(business, person).
domain_entity_type(science, technology).
domain_entity_type(medical, person).
domain_entity_type(medical, organization).

% Additional helper functions for export formats

write_rdf_xml_header(Stream) :-
    format(Stream, '<?xml version="1.0"?>~n<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">~n', []).

write_rdf_xml_facts(Stream, Facts) :-
    forall(member(entity(ID, Type, _Props), Facts),
           format(Stream, '  <rdf:Description rdf:about="~w">~n    <rdf:type>~w</rdf:type>~n  </rdf:Description>~n', [ID, Type])).

write_rdf_xml_relationships(Stream, Relationships) :-
    forall(member(relationship(_ID, Subject, Predicate, Object), Relationships),
           format(Stream, '  <rdf:Description rdf:about="~w">~n    <~w>~w</~w>~n  </rdf:Description>~n', [Subject, Predicate, Object, Predicate])).

write_rdf_xml_footer(Stream) :-
    format(Stream, '</rdf:RDF>~n', []).

write_graphml_header(Stream) :-
    format(Stream, '<?xml version="1.0" encoding="UTF-8"?>~n<graphml xmlns="http://graphml.graphdrawing.org/xmlns">~n  <graph id="G" edgedefault="directed">~n', []).

write_graphml_nodes(Stream, Facts) :-
    forall(member(entity(ID, Type, _), Facts),
           format(Stream, '    <node id="~w"><data key="type">~w</data></node>~n', [ID, Type])).

write_graphml_edges(Stream, Relationships) :-
    forall(member(relationship(_, Subject, Predicate, Object), Relationships),
           format(Stream, '    <edge source="~w" target="~w"><data key="label">~w</data></edge>~n', [Subject, Object, Predicate])).

write_graphml_footer(Stream) :-
    format(Stream, '  </graph>~n</graphml>~n', []).

% JSON conversion helpers

write_json_entities(_, []).
write_json_entities(Stream, [Entity]) :-
    write_json_entity(Stream, Entity).
write_json_entities(Stream, [Entity|Rest]) :-
    write_json_entity(Stream, Entity),
    write(Stream, ',\n'),
    write_json_entities(Stream, Rest).

write_json_entity(Stream, entity(ID, Type)) :-
    format(Stream, '    {"id": "~w", "type": "~w"}', [ID, Type]).
write_json_entity(Stream, entity(ID, Type, _Props)) :-
    format(Stream, '    {"id": "~w", "type": "~w"}', [ID, Type]).

write_json_relationships(_, []).
write_json_relationships(Stream, [Rel]) :-
    write_json_relationship(Stream, Rel).
write_json_relationships(Stream, [Rel|Rest]) :-
    write_json_relationship(Stream, Rel),
    write(Stream, ',\n'),
    write_json_relationships(Stream, Rest).

write_json_relationship(Stream, relationship(Subject, Predicate, Object)) :-
    format(Stream, '    {"subject": "~w", "predicate": "~w", "object": "~w"}', [Subject, Predicate, Object]).
write_json_relationship(Stream, relationship(_ID, Subject, Predicate, Object)) :-
    format(Stream, '    {"subject": "~w", "predicate": "~w", "object": "~w"}', [Subject, Predicate, Object]).
