#!/usr/bin/env swipl
/**
 * Chatbot Interface Example
 * 
 * This example demonstrates the natural language chatbot interface
 * for the semantic knowledge base.
 */

:- use_module('../src/semantic_kb').

% Example chatbot session
demo_chatbot_session :-
    format('=== Chatbot Interface Demo ===~n'),
    format('Setting up sample knowledge base...~n'),
    
    % Add sample knowledge
    setup_sample_knowledge,
    
    % Simulate chatbot interactions
    format('~n--- Simulated Chatbot Session ---~n'),
    
    % Query 1: What do I know about AI?
    format('User: "What do I know about AI?"~n'),
    natural_language_query("What do I know about AI?", Answer1),
    format('Assistant: ~w~n~n', [Answer1]),
    
    % Query 2: Show me my recent learning
    format('User: "Show me my recent learning progress"~n'),
    natural_language_query("Show me my recent learning progress", Answer2),
    format('Assistant: ~w~n~n', [Answer2]),
    
    % Query 3: What are my goals?
    format('User: "What are my current goals?"~n'),
    natural_language_query("What are my current goals?", Answer3),
    format('Assistant: ~w~n~n', [Answer3]),
    
    % Query 4: Find related concepts
    format('User: "Find concepts related to machine learning"~n'),
    natural_language_query("Find concepts related to machine learning", Answer4),
    format('Assistant: ~w~n~n', [Answer4]),
    
    % Query 5: Timeline query
    format('User: "What did I learn this week?"~n'),
    natural_language_query("What did I learn this week?", Answer5),
    format('Assistant: ~w~n~n', [Answer5]).

% Interactive chatbot (for manual testing)
start_interactive_chatbot :-
    format('=== Interactive Chatbot Started ===~n'),
    format('Type your questions and press Enter. Type "quit" to exit.~n~n'),
    setup_sample_knowledge,
    chatbot_loop.

chatbot_loop :-
    format('You: '),
    read_line_to_string(user_input, Question),
    (   Question = "quit"
    ->  format('Goodbye!~n')
    ;   process_user_question(Question),
        chatbot_loop
    ).

process_user_question(Question) :-
    (   natural_language_query(Question, Answer)
    ->  format('Assistant: ~w~n~n', [Answer])
    ;   format('Assistant: I didn\'t understand that question. Could you rephrase it?~n~n')
    ).

% Setup sample knowledge for demo
setup_sample_knowledge :-
    % Clear existing data
    retractall(note(_, _, _, _)),
    retractall(note_link(_, _, _)),
    retractall(note_tag(_, _)),
    retractall(goal(_, _, _, _)),
    
    % Add AI/ML related notes
    add_note(ai_001,
             "Artificial Intelligence is the simulation of human intelligence in machines",
             [tags([ai, definition, intelligence]),
              date(2025, 1, 10),
              category(learning)]),
    
    add_note(ml_001,
             "Machine Learning is a subset of AI that learns patterns from data",
             [tags([machine_learning, ai, data, patterns]),
              date(2025, 1, 12),
              category(learning)]),
    
    add_note(dl_001,
             "Deep Learning uses neural networks with multiple layers",
             [tags([deep_learning, neural_networks, layers]),
              date(2025, 1, 15),
              category(learning)]),
    
    add_note(prolog_001,
             "Started learning Prolog for symbolic AI and logic programming",
             [tags([prolog, symbolic_ai, logic_programming]),
              date(2025, 1, 18),
              category(learning),
              status(in_progress)]),
    
    add_note(project_001,
             "Working on a knowledge base system using Prolog",
             [tags([project, prolog, knowledge_base]),
              date(2025, 1, 20),
              category(project),
              status(active)]),
    
    % Add some goals
    add_goal(goal_ai, "Master AI fundamentals", date(2025, 3, 1)),
    add_goal(goal_prolog, "Build practical Prolog applications", date(2025, 2, 15)),
    
    % Create relationships
    link_notes(ai_001, ml_001, contains),
    link_notes(ml_001, dl_001, contains),
    link_notes(ai_001, prolog_001, related_to),
    link_notes(prolog_001, project_001, enables),
    
    % Add tags
    tag_note(ai_001, fundamental),
    tag_note(ml_001, important),
    tag_note(dl_001, advanced),
    tag_note(prolog_001, current),
    tag_note(project_001, hands_on).

% Natural language processing helpers
process_question_type(Question, Type) :-
    string_lower(Question, LowerQ),
    (   sub_string(LowerQ, _, _, _, "what")
    ->  Type = what_question
    ;   sub_string(LowerQ, _, _, _, "how")
    ->  Type = how_question
    ;   sub_string(LowerQ, _, _, _, "when")
    ->  Type = when_question
    ;   sub_string(LowerQ, _, _, _, "show")
    ->  Type = show_request
    ;   sub_string(LowerQ, _, _, _, "find")
    ->  Type = find_request
    ;   Type = general_query
    ).

extract_keywords(Question, Keywords) :-
    string_lower(Question, LowerQ),
    split_string(LowerQ, ' .,!?', ' .,!?', Words),
    filter_keywords(Words, Keywords).

filter_keywords([], []).
filter_keywords([Word|Rest], [Word|FilteredRest]) :-
    \+ member(Word, ["the", "a", "an", "is", "are", "was", "were", "what", "how", "when", "where", "why"]),
    atom_length(Word, Len),
    Len > 2, !,
    filter_keywords(Rest, FilteredRest).
filter_keywords([_|Rest], FilteredRest) :-
    filter_keywords(Rest, FilteredRest).

% Enhanced natural language query processing
enhanced_nl_query(Question, Answer) :-
    process_question_type(Question, Type),
    extract_keywords(Question, Keywords),
    generate_response(Type, Keywords, Answer).

generate_response(what_question, Keywords, Answer) :-
    (   member("ai", Keywords)
    ->  find_ai_knowledge(Answer)
    ;   member("goal", Keywords)
    ->  find_goals(Answer)
    ;   member("learn", Keywords)
    ->  find_learning_progress(Answer)
    ;   Answer = "I found several notes that might be relevant to your question."
    ).

generate_response(show_request, Keywords, Answer) :-
    (   member("progress", Keywords)
    ->  show_learning_progress(Answer)
    ;   member("recent", Keywords)
    ->  show_recent_activity(Answer)
    ;   Answer = "Here's what I found in your knowledge base."
    ).

generate_response(find_request, Keywords, Answer) :-
    find_related_content(Keywords, Answer).

generate_response(_, Keywords, Answer) :-
    length(Keywords, NumKeywords),
    (   NumKeywords > 0
    ->  find_related_content(Keywords, Answer)
    ;   Answer = "I'd be happy to help! Could you be more specific about what you're looking for?"
    ).

% Helper predicates for response generation
find_ai_knowledge(Answer) :-
    findall(Content, 
            (note(_, Content, Metadata, _), 
             member(tags(Tags), Metadata),
             (member(ai, Tags); member(artificial_intelligence, Tags))), 
            AIContents),
    (   AIContents = []
    ->  Answer = "I don't have any notes about AI yet."
    ;   format_content_list(AIContents, Answer)
    ).

find_goals(Answer) :-
    findall(Description,
            goal(_, Description, _, active),
            Goals),
    (   Goals = []
    ->  Answer = "You don't have any active goals recorded."
    ;   format_goal_list(Goals, Answer)
    ).

find_learning_progress(Answer) :-
    findall(Content,
            (note(_, Content, Metadata, _),
             member(category(learning), Metadata)),
            LearningNotes),
    length(LearningNotes, Count),
    format(string(Answer), "You have ~w learning notes in your knowledge base.", [Count]).

show_recent_activity(Answer) :-
    get_time(Now),
    WeekAgo is Now - 604800,  % 7 days in seconds
    findall(Content,
            (note(_, Content, Metadata, Time),
             Time > WeekAgo),
            RecentNotes),
    length(RecentNotes, Count),
    format(string(Answer), "You have ~w recent notes from the past week.", [Count]).

find_related_content(Keywords, Answer) :-
    findall(Content,
            (note(_, Content, Metadata, _),
             member(tags(Tags), Metadata),
             intersection(Keywords, Tags, Common),
             Common \= []),
            RelatedNotes),
    length(RelatedNotes, Count),
    format(string(Answer), "I found ~w notes related to your query.", [Count]).

format_content_list([Content], Answer) :-
    format(string(Answer), "Here's what I found: ~w", [Content]).
format_content_list([Content1, Content2], Answer) :-
    format(string(Answer), "I found 2 relevant notes: ~w, and ~w", [Content1, Content2]).
format_content_list(Contents, Answer) :-
    length(Contents, Count),
    format(string(Answer), "I found ~w relevant notes in your knowledge base.", [Count]).

format_goal_list([Goal], Answer) :-
    format(string(Answer), "Your current goal: ~w", [Goal]).
format_goal_list(Goals, Answer) :-
    length(Goals, Count),
    format(string(Answer), "You have ~w active goals in your system.", [Count]).

% Helper for adding goals
add_goal(ID, Description, Deadline) :-
    assertz(goal(ID, Description, Deadline, active)).

% Run the demo
run_chatbot_demo :-
    demo_chatbot_session,
    format('~n=== Chatbot demo completed ===~n'),
    format('Type "start_interactive_chatbot." for hands-on testing~n').

% Query interface
?- write('Loading chatbot demo...'), nl.
?- write('Type "run_chatbot_demo." to see the demo'), nl.
?- write('Type "start_interactive_chatbot." for interactive testing'), nl.
