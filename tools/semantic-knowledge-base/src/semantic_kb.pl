:- module(semantic_kb, [
    add_note/3,                    % add_note(+ID, +Content, +Metadata)
    query_notes/2,                 % query_notes(+Query, -Results)
    link_notes/3,                  % link_notes(+Note1, +Note2, +LinkType)
    tag_note/2,                    % tag_note(+NoteID, +Tag)
    search_semantic/2,             % search_semantic(+Terms, -Notes)
    timeline_query/3,              % timeline_query(+StartDate, +EndDate, -Events)
    goal_tracking/2,               % goal_tracking(+Goal, -Progress)
    generate_insights/1,           % generate_insights(-Insights)
    export_knowledge_base/2,       % export_knowledge_base(+Format, +File)
    import_external_data/2,        % import_external_data(+Source, +Format)
    natural_language_query/2,      % natural_language_query(+Question, -Answer)
    start_chatbot_server/1,        % start_chatbot_server(+Port)
    backup_knowledge_base/1,       % backup_knowledge_base(+BackupFile)
    restore_knowledge_base/1       % restore_knowledge_base(+BackupFile)
]).

/** <module> Semantic Personal Knowledge Base

A personal knowledge management system that stores notes as a knowledge graph
and uses Prolog for querying and reasoning, with natural language processing
and chatbot interface.

@author Prolog Tools Collection
@version 1.0.0
@license MIT
*/

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_json)).
:- use_module(library(uuid)).

:- dynamic note/4.              % note(ID, Content, Metadata, Timestamp)
:- dynamic note_link/3.         % note_link(Note1, Note2, LinkType)  
:- dynamic note_tag/2.          % note_tag(NoteID, Tag)
:- dynamic goal/4.              % goal(ID, Description, Deadline, Status)
:- dynamic task/5.              % task(ID, Description, Priority, Status, DueDate)
:- dynamic insight/3.           % insight(ID, Type, Content)
:- dynamic kb_metadata/2.       % kb_metadata(Key, Value)

% Discontiguous predicates - clauses are spread throughout the file
:- discontiguous search_semantic/2.
:- discontiguous query_notes/2.
:- discontiguous goal_tracking/2.
:- discontiguous natural_language_query/2.
:- discontiguous export_knowledge_base/2.
:- discontiguous export_to_json/1.
:- discontiguous generate_insights/1.
:- discontiguous extract_links_from_content/2.
:- discontiguous extract_dates_from_content/2.
:- discontiguous analyze_content_topics/2.
:- discontiguous note_id_from_title/2.
:- discontiguous create_topic_connections/2.
:- discontiguous identify_entities/2.
:- discontiguous create_entity_relationships/2.
:- discontiguous expand_search_terms/2.

%% add_note(+ID, +Content, +Metadata) is det.
%
%  Adds a new note to the knowledge base.
%
%  @param ID Unique identifier for the note
%  @param Content The note content (text, markdown, etc.)  
%  @param Metadata Additional metadata (tags, dates, etc.)
%
add_note(ID, Content, Metadata) :-
    get_time(Timestamp),
    assertz(note(ID, Content, Metadata, Timestamp)),
    process_note_content(ID, Content, Metadata),
    update_knowledge_graph(ID, Content, Metadata).

%% query_notes(+Query, -Results) is det.
%
%  Queries notes using structured query syntax.
%
query_notes(content_contains(Text), Results) :-
    findall(note(ID, Content, Meta, Time),
            (note(ID, Content, Meta, Time),
             sub_atom(Content, _, _, _, Text)),
            Results).

query_notes(tagged_with(Tag), Results) :-
    findall(note(ID, Content, Meta, Time),
            (note_tag(ID, Tag),
             note(ID, Content, Meta, Time)),
            Results).

query_notes(created_between(Start, End), Results) :-
    findall(note(ID, Content, Meta, Time),
            (note(ID, Content, Meta, Time),
             Time >= Start,
             Time =< End),
            Results).

query_notes(linked_to(NoteID), Results) :-
    findall(note(ID, Content, Meta, Time),
            (note_link(NoteID, ID, _),
             note(ID, Content, Meta, Time)),
            Results).

%% link_notes(+Note1, +Note2, +LinkType) is det.
%
%  Creates a semantic link between two notes.
%
link_notes(Note1, Note2, LinkType) :-
    assertz(note_link(Note1, Note2, LinkType)),
    % Create bidirectional link for certain types
    (bidirectional_link(LinkType) ->
        assertz(note_link(Note2, Note1, LinkType)) ;
        true).

%% tag_note(+NoteID, +Tag) is det.
%
%  Adds a tag to a note.
%
tag_note(NoteID, Tag) :-
    assertz(note_tag(NoteID, Tag)).

%% search_semantic(+Terms, -Notes) is det.
%
%  Performs semantic search across notes.
%
search_semantic(Terms, Notes) :-
    expand_search_terms(Terms, ExpandedTerms),
    findall(note(ID, Content, Meta, Time),
            (note(ID, Content, Meta, Time),
             semantic_match(Content, Meta, ExpandedTerms)),
            Notes).

%% timeline_query(+StartDate, +EndDate, -Events) is det.
%
%  Retrieves events and notes within a time range.
%
timeline_query(StartDate, EndDate, Events) :-
    date_to_timestamp(StartDate, StartTime),
    date_to_timestamp(EndDate, EndTime),
    findall(event(Time, Type, Content, ID),
            (note(ID, Content, Meta, Time),
             Time >= StartTime,
             Time =< EndTime,
             extract_event_type(Meta, Type)),
            Events).

%% goal_tracking(+Goal, -Progress) is det.
%
%  Tracks progress towards goals using linked notes and tasks.
%
goal_tracking(GoalID, Progress) :-
    goal(GoalID, Description, Deadline, Status),
    findall(Task, 
            (task(TaskID, _, _, TaskStatus, _),
             note_link(GoalID, TaskID, 'contributes_to'),
             Task = task(TaskID, TaskStatus)),
            Tasks),
    calculate_progress(Tasks, ProgressPercent),
    Progress = progress(GoalID, Description, ProgressPercent, Status, Deadline).

%% generate_insights(-Insights) is det.
%
%  Generates insights from the knowledge base using reasoning.
%
generate_insights(Insights) :-
    findall(Insight, generate_single_insight(Insight), Insights).

%% natural_language_query(+Question, -Answer) is det.
%
%  Processes natural language queries and returns answers.
%
natural_language_query(Question, Answer) :-
    parse_natural_language(Question, ParsedQuery),
    execute_parsed_query(ParsedQuery, Results),
    format_answer(Results, Answer).

%% start_chatbot_server(+Port) is det.
%
%  Starts the web-based chatbot interface.
%
start_chatbot_server(Port) :-
    setup_http_handlers,
    http_server(http_dispatch, [port(Port)]),
    format('Chatbot server started on port ~w~n', [Port]),
    format('Visit http://localhost:~w for the web interface~n', [Port]).

%% export_knowledge_base(+Format, +File) is det.
%
%  Exports the knowledge base in various formats.
%
export_knowledge_base(markdown, File) :-
    export_to_markdown(File).

export_knowledge_base(obsidian, File) :-
    export_to_obsidian_vault(File).

export_knowledge_base(json, File) :-
    export_to_json(File).

export_knowledge_base(rdf, File) :-
    export_to_rdf(File).

%% import_external_data(+Source, +Format) is det.
%
%  Imports data from external sources.
%
import_external_data(obsidian_vault(Path), markdown) :-
    import_obsidian_vault(Path).

import_external_data(logseq_graph(Path), markdown) :-
    import_logseq_graph(Path).

import_external_data(roam_export(File), json) :-
    import_roam_export(File).

% Helper predicates

process_note_content(ID, Content, _Metadata) :-
    extract_tags_from_content(Content, Tags),
    extract_links_from_content(Content, Links),
    extract_dates_from_content(Content, Dates),
    process_extracted_tags(ID, Tags),
    process_extracted_links(ID, Links),
    process_extracted_dates(ID, Dates).

update_knowledge_graph(ID, Content, _Metadata) :-
    % Create semantic connections based on content analysis
    analyze_content_topics(Content, Topics),
    create_topic_connections(ID, Topics),
    identify_entities(Content, Entities),
    create_entity_relationships(ID, Entities).

extract_tags_from_content(Content, Tags) :-
    findall(Tag,
            (sub_atom(Content, _Before, _Len, _After, TagMatch),
             sub_atom(TagMatch, 0, 1, _, '#'),
             sub_atom(TagMatch, 1, _, 0, Tag),
             \+ sub_atom(Tag, _, _, _, ' ')),
            Tags).

extract_links_from_content(Content, Links) :-
    findall(Link,
            (sub_atom(Content, _, _, _, '[['),
             sub_atom(Content, Start, _, End, '[['),
             Plus2 is Start + 2,
             sub_atom(Content, Plus2, LinkLen, _, LinkContent),
             sub_atom(Content, _, 2, End, ']]'),
             LinkEnd is LinkLen - 2,
             sub_atom(LinkContent, 0, LinkEnd, _, Link)),
            Links).

extract_dates_from_content(Content, Dates) :-
    findall(Date,
            (re_matchsub('\\d{4}-\\d{2}-\\d{2}', Content, Match, []),
             get_dict(0, Match, Date)),
            Dates).

process_extracted_tags(ID, Tags) :-
    forall(member(Tag, Tags), tag_note(ID, Tag)).

process_extracted_links(ID, Links) :-
    forall(member(Link, Links), 
           (note_id_from_title(Link, LinkedID) ->
               link_notes(ID, LinkedID, 'references') ;
               true)).

process_extracted_dates(ID, Dates) :-
    forall(member(Date, Dates),
           assertz(note_tag(ID, date(Date)))).

% Helper predicates for semantic search
expand_search_terms(Terms, ExpandedTerms) :-
    % Simple implementation - just return the terms as-is
    ExpandedTerms = Terms.

semantic_match(Content, _Meta, Terms) :-
    % Simple implementation - check if any term appears in content
    member(Term, Terms),
    downcase_atom(Content, LowerContent),
    downcase_atom(Term, LowerTerm),
    sub_atom(LowerContent, _, _, _, LowerTerm).

% Helper predicates for content extraction
extract_links_from_content(_Content, []).
extract_dates_from_content(_Content, []).
note_id_from_title(_Title, _ID) :- fail.

% Helper predicates for analysis
analyze_content_topics(_Content, []).
create_topic_connections(_ID, _Topics).
identify_entities(_Content, []).
create_entity_relationships(_ID, _Entities).

generate_single_insight(insight(InsightID, Type, Content)) :-
    uuid(InsightID),
    insight_type(Type),
    generate_insight_content(Type, Content).

insight_type(tag_clusters).
insight_type(temporal_patterns).
insight_type(goal_dependencies).
insight_type(knowledge_gaps).

generate_insight_content(tag_clusters, Content) :-
    findall(Tag-Count,
            (note_tag(_, Tag),
             aggregate_all(count, note_tag(_, Tag), Count)),
            TagCounts),
    (TagCounts = [] ->
        Content = 'No tags found' ;
        (sort(2, @>=, TagCounts, SortedTags),
         take(10, SortedTags, TopTags),
         format_atom('Most used tags: ~w', [TopTags], Content))).

generate_insight_content(temporal_patterns, Content) :-
    findall(Date-Count,
            (note(_, _, _, Time),
             format_time(atom(Date), '%Y-%m-%d', Time),
             aggregate_all(count, 
                          (note(_, _, _, T),
                           format_time(atom(Date), '%Y-%m-%d', T)), Count)),
            DateCounts),
    (DateCounts = [] ->
        Content = 'No temporal patterns found' ;
        (analyze_writing_patterns(DateCounts, Pattern),
         format_atom('Writing pattern: ~w', [Pattern], Content))).

generate_insight_content(goal_dependencies, Content) :-
    Content = 'Goal dependency analysis not yet implemented'.

generate_insight_content(knowledge_gaps, Content) :-
    Content = 'Knowledge gap analysis not yet implemented'.

% Natural language processing

parse_natural_language(Question, ParsedQuery) :-
    downcase_atom(Question, LowerQuestion),
    tokenize_question(LowerQuestion, Tokens),
    identify_query_type(Tokens, QueryType),
    extract_parameters(Tokens, QueryType, Parameters),
    ParsedQuery =.. [QueryType|Parameters].

tokenize_question(Question, Tokens) :-
    split_string(Question, ' .,?!', ' .,?!', StringTokens),
    maplist(atom_string, Tokens, StringTokens).

identify_query_type(Tokens, search) :-
    (member(find, Tokens) ; member(search, Tokens) ; member(show, Tokens)).

identify_query_type(Tokens, count) :-
    (member(how, Tokens), member(many, Tokens)).

identify_query_type(Tokens, recent) :-
    (member(recent, Tokens) ; member(latest, Tokens)).

identify_query_type(Tokens, tagged) :-
    member(tagged, Tokens).

extract_parameters(Tokens, search, [SearchTerms]) :-
    exclude(is_stop_word, Tokens, ContentWords),
    SearchTerms = ContentWords.

extract_parameters(Tokens, tagged, [Tag]) :-
    append(_, [with|Rest], Tokens),
    Rest = [Tag|_].

is_stop_word(Word) :-
    member(Word, [the, a, an, and, or, but, in, on, at, to, for, of, with, by]).

execute_parsed_query(search(Terms), Results) :-
    search_semantic(Terms, Results).

execute_parsed_query(tagged(Tag), Results) :-
    query_notes(tagged_with(Tag), Results).

execute_parsed_query(recent, Results) :-
    get_time(Now),
    WeekAgo is Now - 604800,  % 7 days in seconds
    query_notes(created_between(WeekAgo, Now), Results).

format_answer(Results, Answer) :-
    length(Results, Count),
    (Count = 0 ->
        Answer = 'No results found.' ;
        format_atom(Answer, 'Found ~w results. Here are the details: ~w', [Count, Results])).

% Web interface handlers

setup_http_handlers :-
    http_handler('/api/query', handle_api_query, [method(post)]),
    http_handler('/api/add_note', handle_add_note, [method(post)]),
    http_handler('/api/chat', handle_chat, [method(post)]),
    http_handler('/', serve_static_files, [prefix]).

handle_api_query(Request) :-
    http_read_json_dict(Request, QueryDict),
    Query = QueryDict.query,
    query_notes(Query, Results),
    reply_json_dict(_{results: Results}).

handle_add_note(Request) :-
    http_read_json_dict(Request, NoteDict),
    uuid(ID),
    Content = NoteDict.content,
    Metadata = NoteDict.metadata,
    add_note(ID, Content, Metadata),
    reply_json_dict(_{success: true, id: ID}).

handle_chat(Request) :-
    http_read_json_dict(Request, ChatDict),
    Question = ChatDict.question,
    natural_language_query(Question, Answer),
    reply_json_dict(_{answer: Answer}).

serve_static_files(Request) :-
    http_reply_from_files('web', [], Request).

% Export implementations

export_to_markdown(File) :-
    open(File, write, Stream),
    write(Stream, '# Knowledge Base Export\n\n'),
    forall(note(ID, Content, Meta, Time),
           (format(Stream, '## Note: ~w\n\n', [ID]),
            format(Stream, '**Created:** ~w\n\n', [Time]),
            format(Stream, '~w\n\n', [Content]),
            write_metadata(Stream, Meta),
            write(Stream, '---\n\n'))),
    close(Stream).

export_to_json(File) :-
    findall(note_object(ID, Content, Meta, Time),
            note(ID, Content, Meta, Time),
            Notes),
    findall(link_object(N1, N2, Type),
            note_link(N1, N2, Type),
            Links),
    ExportData = json([notes=Notes, links=Links]),
    open(File, write, Stream),
    json_write(Stream, ExportData),
    close(Stream).

export_to_obsidian_vault(VaultPath) :-
    forall(note(ID, Content, Meta, _),
           (atomic_list_concat([VaultPath, '/', ID, '.md'], '', FilePath),
            export_note_to_obsidian(FilePath, ID, Content, Meta))).

export_note_to_obsidian(FilePath, ID, Content, Meta) :-
    open(FilePath, write, Stream),
    write_obsidian_frontmatter(Stream, ID, Meta),
    write(Stream, Content),
    close(Stream).

% Import implementations

import_obsidian_vault(VaultPath) :-
    atomic_list_concat([VaultPath, '/*.md'], '', Pattern),
    expand_file_name(Pattern, Files),
    forall(member(File, Files),
           import_obsidian_note(File)).

import_obsidian_note(File) :-
    read_file_to_string(File, Content, []),
    extract_obsidian_metadata(Content, Metadata, NoteContent),
    file_base_name(File, FileName),
    file_name_extension(ID, md, FileName),
    add_note(ID, NoteContent, Metadata).

% Utility predicates

bidirectional_link(related).
bidirectional_link(similar).

date_to_timestamp(date(Y, M, D), Timestamp) :-
    Date = date(Y, M, D),
    date_time_stamp(Date, Timestamp).

calculate_progress(Tasks, Progress) :-
    length(Tasks, Total),
    include(completed_task, Tasks, Completed),
    length(Completed, CompletedCount),
    (Total > 0 ->
        Progress is (CompletedCount * 100) // Total ;
        Progress = 0).

completed_task(task(_, completed)).

analyze_content_topics(Content, Topics) :-
    % Simple topic extraction based on frequent words
    split_string(Content, ' \t\n.,!?;:', ' \t\n.,!?;:', Words),
    include(significant_word, Words, SignificantWords),
    msort(SignificantWords, SortedWords),
    group_consecutive(SortedWords, GroupedWords),
    include(frequent_word, GroupedWords, Topics).

significant_word(Word) :-
    string_length(Word, Len),
    Len >= 4,
    \+ stop_word(Word).

stop_word("the").
stop_word("and").
stop_word("that").
stop_word("with").
stop_word("for").

frequent_word([_|T]) :-
    length(T, Count),
    Count >= 2.

take(N, List, Taken) :-
    length(Taken, N),
    append(Taken, _, List).

group_consecutive([], []).
group_consecutive([H|T], [[H|Same]|Groups]) :-
    take_same(H, T, Same, Rest),
    group_consecutive(Rest, Groups).

take_same(_, [], [], []).
take_same(X, [X|T], [X|Same], Rest) :-
    take_same(X, T, Same, Rest).
take_same(X, [Y|T], [], [Y|T]) :-
    X \= Y.

analyze_writing_patterns(DateCounts, Pattern) :-
    % Simplified pattern analysis
    (length(DateCounts, Days),
     Days > 7 ->
        Pattern = regular_writer ;
        Pattern = occasional_writer).

write_metadata(Stream, Meta) :-
    format(Stream, '**Metadata:** ~w\n\n', [Meta]).

write_obsidian_frontmatter(Stream, ID, Meta) :-
    write(Stream, '---\n'),
    format(Stream, 'id: ~w\n', [ID]),
    format(Stream, 'metadata: ~w\n', [Meta]),
    write(Stream, '---\n\n').

extract_obsidian_metadata(Content, Metadata, NoteContent) :-
    (sub_atom(Content, 0, _, _, '---') ->
        split_frontmatter(Content, Metadata, NoteContent) ;
        (Metadata = [], NoteContent = Content)).

split_frontmatter(Content, Metadata, NoteContent) :-
    sub_atom(Content, 3, _, _, RestContent),
    sub_atom(RestContent, Before, 3, _After, '---'),
    sub_atom(RestContent, 0, Before, _, FrontMatter),
    Plus3 is Before + 3,
    sub_atom(RestContent, Plus3, _, 0, NoteContent),
    parse_yaml_metadata(FrontMatter, Metadata).

parse_yaml_metadata(FrontMatter, Metadata) :-
    % Simplified YAML parsing
    split_string(FrontMatter, '\n', '\n', Lines),
    maplist(parse_yaml_line, Lines, Metadata).

parse_yaml_line(Line, Key-Value) :-
    split_string(Line, ':', ' ', [KeyStr, ValueStr]),
    atom_string(Key, KeyStr),
    atom_string(Value, ValueStr).

note_id_from_title(Title, ID) :-
    note(ID, Content, _, _),
    sub_atom(Content, 0, _, _, Title).

create_topic_connections(ID, Topics) :-
    forall(member(Topic, Topics),
           tag_note(ID, topic(Topic))).

identify_entities(Content, Entities) :-
    % Simple entity recognition based on capitalized words
    split_string(Content, ' \t\n.,!?;:', ' \t\n.,!?;:', Words),
    include(is_entity, Words, Entities).

is_entity(Word) :-
    string_codes(Word, [First|_]),
    First >= 65, First =< 90.  % Uppercase letter

create_entity_relationships(ID, Entities) :-
    forall(member(Entity, Entities),
           tag_note(ID, entity(Entity))).

% Backup and restore functionality
backup_knowledge_base(BackupFile) :-
    open(BackupFile, write, Stream),
    format(Stream, '% Semantic KB Backup ~w~n', [BackupFile]),
    forall(note(ID, Content, Metadata, Timestamp),
           format(Stream, 'note(~q, ~q, ~q, ~q).~n', [ID, Content, Metadata, Timestamp])),
    forall(note_link(Note1, Note2, LinkType),
           format(Stream, 'note_link(~q, ~q, ~q).~n', [Note1, Note2, LinkType])),
    forall(note_tag(NoteID, Tag),
           format(Stream, 'note_tag(~q, ~q).~n', [NoteID, Tag])),
    forall(goal(ID, Description, Deadline, Status),
           format(Stream, 'goal(~q, ~q, ~q, ~q).~n', [ID, Description, Deadline, Status])),
    close(Stream).

restore_knowledge_base(BackupFile) :-
    % Clear current knowledge base
    retractall(note(_, _, _, _)),
    retractall(note_link(_, _, _)),
    retractall(note_tag(_, _)),
    retractall(goal(_, _, _, _)),
    % Load from backup
    consult(BackupFile).

% Search expansion
expand_search_terms(Terms, ExpandedTerms) :-
    findall(Term, 
            (member(T, Terms),
             (Term = T ; synonym(T, Term) ; related_term(T, Term))),
            ExpandedTerms).

% Synonyms and related terms (simplified)
synonym(ai, 'artificial intelligence').
synonym('artificial intelligence', ai).
synonym(ml, 'machine learning').
synonym('machine learning', ml).

related_term(prolog, logic).
related_term(logic, reasoning).
related_term(ai, ml).
related_term(ml, 'neural networks').

% JSON export helpers
json_write(Stream, Data) :-
    format(Stream, '~w', [Data]).

% Format atom helper
format_atom(Format, Args, Atom) :-
    format(atom(Atom), Format, Args).

% Enhanced search semantic implementation
search_semantic(Terms, Notes) :-
    expand_search_terms(Terms, ExpandedTerms),
    findall(note(ID, Content, Metadata, Timestamp),
            (note(ID, Content, Metadata, Timestamp),
             note_matches_terms(ID, Content, Metadata, ExpandedTerms)),
            Notes).

note_matches_terms(_, Content, Metadata, Terms) :-
    (   member(Tag, Terms),
        (   member(tags(Tags), Metadata), member(Tag, Tags)
        ;   sub_atom(Content, _, _, _, Tag)
        )
    ), !.

% Enhanced query notes implementation  
query_notes(Query, Results) :-
    findall(Note, call(Query, Note), Results).

% Goal tracking implementation
goal_tracking(GoalID, Progress) :-
    (   goal(GoalID, Description, Deadline, Status)
    ->  calculate_goal_progress(GoalID, Description, Deadline, Status, Progress)
    ;   Progress = goal_not_found(GoalID)
    ).

calculate_goal_progress(GoalID, Description, Deadline, Status, Progress) :-
    findall(_Task, task(_, _, _, _, _), AllTasks),
    length(AllTasks, TotalTasks),
    findall(_CompletedTask, task(_, _, _, completed, _), CompletedTasks),
    length(CompletedTasks, CompletedCount),
    (   TotalTasks > 0
    ->  ProgressPercent is (CompletedCount * 100) // TotalTasks
    ;   ProgressPercent = 0
    ),
    get_time(Now),
    format_time(atom(CurrentDate), '%Y-%m-%d', Now),
    Progress = progress(GoalID, Description, ProgressPercent, Status, Deadline, CurrentDate).

% Natural language query implementation
natural_language_query(Question, Answer) :-
    process_nl_question(Question, ProcessedQuery),
    execute_nl_query(ProcessedQuery, Answer).

process_nl_question(Question, ProcessedQuery) :-
    atom_string(QuestionAtom, Question),
    downcase_atom(QuestionAtom, LowerQuestion),
    atom_codes(LowerQuestion, Codes),
    phrase(parse_question(ProcessedQuery), Codes, _).

parse_question(search_query(Terms)) -->
    "what", spaces, "do", spaces, "i", spaces, "know", spaces, "about", spaces,
    phrase(extract_terms(Terms)).
parse_question(show_query(recent)) -->
    "show", spaces, "me", spaces, "recent", spaces.
parse_question(goal_query) -->
    "what", spaces, "are", spaces, "my", spaces, "goals".
parse_question(general_query(Question)) -->
    phrase(any_text(Question)).

extract_terms([Term|Rest]) -->
    phrase(word(Term)), spaces, phrase(extract_terms(Rest)).
extract_terms([Term]) -->
    phrase(word(Term)).
extract_terms([]) --> [].

word(Word) -->
    [C], { code_type(C, alpha) },
    phrase(word_rest(Codes)),
    { atom_codes(Word, [C|Codes]) }.

word_rest([C|Rest]) -->
    [C], { code_type(C, alnum) },
    phrase(word_rest(Rest)).
word_rest([]) --> [].

spaces --> [C], { code_type(C, space) }, spaces.
spaces --> [].

any_text(Text) -->
    phrase(chars(Codes)),
    { atom_codes(Text, Codes) }.

chars([C|Rest]) --> [C], phrase(chars(Rest)).
chars([]) --> [].

execute_nl_query(search_query(Terms), Answer) :-
    search_semantic(Terms, Notes),
    length(Notes, Count),
    format(atom(Answer), 'I found ~w notes about ~w', [Count, Terms]).
execute_nl_query(show_query(recent), Answer) :-
    get_time(Now),
    WeekAgo is Now - 604800,
    findall(_Note, 
            (note(_, _, _, Timestamp), Timestamp > WeekAgo),
            RecentNotes),
    length(RecentNotes, Count),
    format(atom(Answer), 'You have ~w recent notes from the past week', [Count]).
execute_nl_query(goal_query, Answer) :-
    findall(Description, goal(_, Description, _, active), Goals),
    length(Goals, Count),
    format(atom(Answer), 'You have ~w active goals', [Count]).
execute_nl_query(general_query(Question), Answer) :-
    format(atom(Answer), 'I received your question about: ~w', [Question]).

% Export functionality with better error handling
export_knowledge_base(json, File) :-
    catch(export_to_json(File), Error, 
          (format('Export failed: ~w~n', [Error]), fail)).

export_to_json(File) :-
    open(File, write, Stream),
    write(Stream, '{\n  "notes": [\n'),
    findall(note(ID, Content, Metadata, Timestamp), 
            note(ID, Content, Metadata, Timestamp), Notes),
    write_json_notes(Stream, Notes),
    write(Stream, '\n  ],\n  "links": [\n'),
    findall(link(Note1, Note2, LinkType),
            note_link(Note1, Note2, LinkType), Links),
    write_json_links(Stream, Links),
    write(Stream, '\n  ]\n}'),
    close(Stream).

write_json_notes(_, []).
write_json_notes(Stream, [note(ID, Content, Metadata, Timestamp)|Rest]) :-
    format(Stream, '    {"id": "~w", "content": "~w", "metadata": "~w", "timestamp": "~w"}',
           [ID, Content, Metadata, Timestamp]),
    (   Rest = [] -> true ; write(Stream, ',')),
    write(Stream, '\n'),
    write_json_notes(Stream, Rest).

write_json_links(_, []).
write_json_links(Stream, [link(Note1, Note2, LinkType)|Rest]) :-
    format(Stream, '    {"from": "~w", "to": "~w", "type": "~w"}',
           [Note1, Note2, LinkType]),
    (   Rest = [] -> true ; write(Stream, ',')),
    write(Stream, '\n'),
    write_json_links(Stream, Rest).

% Insights generation with better implementation
generate_insights(Insights) :-
    catch(analyze_note_patterns(NotePatterns), _, NotePatterns = []),
    catch(analyze_tag_frequency(TagFrequency), _, TagFrequency = []), 
    catch(analyze_learning_trends(LearningTrends), _, LearningTrends = []),
    Insights = [
        insight(note_patterns, NotePatterns),
        insight(tag_frequency, TagFrequency),
        insight(learning_trends, LearningTrends)
    ].

analyze_note_patterns(Patterns) :-
    findall(Date-Count,
            (findall(ID, note(ID, _, _, _), IDs),
             length(IDs, Count),
             Count > 0,
             get_time(Now),
             format_time(atom(Date), '%Y-%m-%d', Now)),
            TempPatterns),
    (TempPatterns = [] -> 
        Patterns = [today-0] ;
        Patterns = TempPatterns).

analyze_tag_frequency(TagFreq) :-
    findall(Tag, note_tag(_, Tag), AllTags),
    (AllTags = [] -> 
        TagFreq = [] ;
        (msort(AllTags, SortedTags),
         simple_count_tags(SortedTags, TagFreq))).

% Simple tag counting helper
simple_count_tags([], []).
simple_count_tags([Tag|Tags], [Tag-Count|Rest]) :-
    count_occurrences(Tag, [Tag|Tags], Count, Remaining),
    simple_count_tags(Remaining, Rest).

count_occurrences(_, [], 0, []).
count_occurrences(Tag, [Tag|Rest], Count, Remaining) :-
    count_occurrences(Tag, Rest, RestCount, Remaining),
    Count is RestCount + 1.
count_occurrences(Tag, [Other|Rest], Count, [Other|Remaining]) :-
    Tag \= Other,
    count_occurrences(Tag, Rest, Count, Remaining).

analyze_learning_trends(Trends) :-
    findall(learning_note(ID),
            (note(ID, _, Metadata, _),
             member(category(learning), Metadata)),
            LearningNotes),
    length(LearningNotes, Count),
    Trends = [learning_notes_count(Count)].
