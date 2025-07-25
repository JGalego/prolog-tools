#!/usr/bin/env swipl
/**
 * Personal Learning Journal Example
 * 
 * This example demonstrates using the semantic knowledge base
 * for personal learning and note-taking.
 */

:- use_module('../src/semantic_kb').

% Example: Setting up a personal learning journal
demo_personal_journal :-
    format('=== Personal Learning Journal Demo ===~n'),
    
    % Create some learning notes
    add_note(note_001, 
             "Started learning Prolog today. It's a declarative language based on logic.",
             [tags([prolog, learning, logic]), 
              date(2025, 1, 15),
              category(learning)]),
    
    add_note(note_002,
             "Prolog uses unification and backtracking for problem solving.",
             [tags([prolog, unification, backtracking]),
              date(2025, 1, 16), 
              category(learning),
              difficulty(intermediate)]),
    
    add_note(note_003,
             "Built my first Prolog predicate that calculates factorials recursively.",
             [tags([prolog, recursion, practice]),
              date(2025, 1, 17),
              category(practice),
              achievement(first_program)]),
    
    % Link related notes
    link_notes(note_001, note_002, builds_on),
    link_notes(note_002, note_003, leads_to),
    
    % Tag notes for easy retrieval
    tag_note(note_001, beginner),
    tag_note(note_002, concepts),
    tag_note(note_003, hands_on),
    
    % Query the knowledge base
    format('~n--- Finding all Prolog-related notes ---~n'),
    search_semantic([prolog], PrologNotes),
    print_notes(PrologNotes),
    
    % Timeline query
    format('~n--- Learning timeline ---~n'),
    timeline_query(date(2025, 1, 15), date(2025, 1, 20), Events),
    print_timeline(Events),
    
    % Generate insights
    format('~n--- Learning insights ---~n'),
    generate_insights(Insights),
    print_insights(Insights).

% Helper predicate to print notes
print_notes([]).
print_notes([note(ID, Content, Metadata, _)|Rest]) :-
    format('Note ~w: ~w~n', [ID, Content]),
    format('  Metadata: ~w~n', [Metadata]),
    print_notes(Rest).

% Helper predicate to print timeline
print_timeline([]).
print_timeline([Event|Rest]) :-
    format('Event: ~w~n', [Event]),
    print_timeline(Rest).

% Helper predicate to print insights  
print_insights([]).
print_insights([Insight|Rest]) :-
    format('Insight: ~w~n', [Insight]),
    print_insights(Rest).

% Goal tracking example
demo_goal_tracking :-
    format('~n=== Goal Tracking Demo ===~n'),
    
    % Add a learning goal
    add_goal(goal_001, "Master Prolog basics", date(2025, 2, 1)),
    
    % Add tasks related to the goal
    add_task(task_001, "Complete Prolog tutorial", high, todo, date(2025, 1, 20)),
    add_task(task_002, "Build 5 Prolog programs", medium, in_progress, date(2025, 1, 25)),
    add_task(task_003, "Understand cut operator", low, todo, date(2025, 1, 30)),
    
    % Check goal progress
    goal_tracking(goal_001, Progress),
    format('Goal progress: ~w~n', [Progress]).

% Natural language query example
demo_natural_language :-
    format('~n=== Natural Language Query Demo ===~n'),
    
    % Process natural language queries
    natural_language_query("What did I learn about Prolog?", Answer1),
    format('Q: What did I learn about Prolog?~n'),
    format('A: ~w~n~n', [Answer1]),
    
    natural_language_query("Show me my recent achievements", Answer2),
    format('Q: Show me my recent achievements~n'),
    format('A: ~w~n~n', [Answer2]),
    
    natural_language_query("What are my learning goals?", Answer3),
    format('Q: What are my learning goals?~n'),
    format('A: ~w~n', [Answer3]).

% Run all demos
run_demo :-
    demo_personal_journal,
    demo_goal_tracking,
    demo_natural_language,
    format('~n=== Demo completed ===~n').

% Helper predicates for the semantic KB
add_goal(ID, Description, Deadline) :-
    get_time(Now),
    assertz(goal(ID, Description, Deadline, active)),
    format('Added goal: ~w~n', [Description]).

add_task(ID, Description, Priority, Status, DueDate) :-
    assertz(task(ID, Description, Priority, Status, DueDate)),
    format('Added task: ~w~n', [Description]).

% Query interface
?- write('Loading personal journal demo...'), nl.
?- write('Type "run_demo." to see all examples'), nl.
?- write('Type "demo_personal_journal.", "demo_goal_tracking.", or "demo_natural_language." for specific demos'), nl.
