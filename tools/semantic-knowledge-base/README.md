# Semantic Knowledge Base

A personal knowledge management system that stores notes as a knowledge graph and uses Prolog for querying and reasoning.

## Features

- **Knowledge Graph Storage**: Store notes and documents as interconnected semantic graphs
- **Natural Language Processing**: Convert natural language to Prolog rules and queries
- **Timeline Reasoning**: Track temporal relationships and dependencies
- **Goal Tracking**: Monitor progress toward objectives with logical reasoning
- **Chatbot Interface**: Interactive query system for knowledge discovery
- **Semantic Search**: Find related concepts and ideas through reasoning
- **Export Capabilities**: Generate various knowledge formats

## Quick Start

```prolog
?- use_module(library(semantic_kb)).
?- create_knowledge_base(my_kb).
?- add_note(my_kb, "Machine learning improves with more data", [ml, data, improvement]).
?- query_kb(my_kb, related_to(ml, X), Results).
```

## Installation

1. Ensure SWI-Prolog is installed
2. Install NLP dependencies:
   ```bash
   swipl -g "pack_install(nlp)" -t halt
   ```

## Core Concepts

### Knowledge Representation

Notes are stored as semantic triples with rich metadata:

```prolog
% Basic note structure
note(note_id, content, tags, timestamp, relations).

% Example note
note(note_001, 
     "Prolog is excellent for symbolic AI", 
     [prolog, ai, symbolic], 
     timestamp(2025, 1, 15, 10, 30, 0),
     [related_to(note_002), follows_from(note_003)]).
```

### Reasoning Rules

The system includes built-in reasoning capabilities:

```prolog
% Transitive relationships
related_to(X, Z) :- related_to(X, Y), related_to(Y, Z).

% Temporal reasoning
before(Event1, Event2) :- 
    event_time(Event1, Time1),
    event_time(Event2, Time2),
    Time1 @< Time2.

% Goal achievement tracking
achieves_goal(Action, Goal) :-
    action_outcome(Action, Outcome),
    goal_condition(Goal, Condition),
    satisfies(Outcome, Condition).
```

## Usage Examples

### Adding Knowledge

```prolog
% Add a simple note
?- add_note("Learning Prolog helps with logical thinking", [prolog, logic, learning]).

% Add a note with relationships
?- add_note("Neural networks are inspired by biological neurons", 
            [ml, biology, neurons],
            [related_to(brain_research), builds_on(neuroscience)]).

% Add temporal information
?- add_event("Started learning Prolog", 
             date(2025, 1, 1),
             [learning, prolog, start]).
```

### Querying Knowledge

```prolog
% Find all notes about machine learning
?- find_notes_by_tag(ml, MLNotes).

% Find related concepts
?- find_related_concepts(prolog, RelatedConcepts).

% Timeline queries
?- events_between(date(2025, 1, 1), date(2025, 1, 31), Events).

% Goal tracking
?- check_goal_progress(learn_prolog, Progress).
```

### Natural Language Interface

```prolog
% Process natural language queries
?- nl_query("What did I learn about machine learning last week?", Results).
?- nl_query("Show me notes related to artificial intelligence", Results).
?- nl_query("What are my goals for this month?", Results).
```

### Chatbot Interface

```prolog
% Start interactive session
?- start_chatbot.
> "What do I know about Prolog?"
Found 15 notes about Prolog. Here are the key concepts:
- Logic programming paradigm
- Excellent for symbolic AI
- Used in knowledge representation
...

> "How is my goal to learn AI progressing?"
Goal: Learn AI fundamentals
Progress: 65% complete
Recent activities:
- Completed Prolog tutorial (Jan 15)
- Read about neural networks (Jan 18)
- Started machine learning course (Jan 20)
```

## API Reference

### Core Predicates

- `create_knowledge_base/1` - Initialize a new knowledge base
- `add_note/2` - Add a note with automatic relation detection
- `add_note/3` - Add a note with explicit tags
- `add_note/4` - Add a note with explicit relationships
- `query_kb/3` - Query the knowledge base
- `find_notes_by_tag/2` - Find notes with specific tags
- `find_related_concepts/2` - Find conceptually related items

### Natural Language Processing

- `nl_to_prolog/2` - Convert natural language to Prolog query
- `nl_query/2` - Process natural language query
- `extract_concepts/2` - Extract key concepts from text
- `generate_summary/2` - Generate summary of knowledge area

### Timeline and Goals

- `add_event/3` - Add temporal events
- `add_goal/3` - Define goals with conditions
- `track_progress/2` - Track goal progress
- `timeline_analysis/2` - Analyze temporal patterns

### Export and Integration

- `export_kb/2` - Export knowledge base in various formats
- `import_from_file/2` - Import from external sources
- `sync_with_external/2` - Synchronize with external systems

## Configuration

### Custom Reasoning Rules

```prolog
% Define domain-specific rules
:- dynamic custom_rule/2.

custom_rule(expertise_development,
    (expert_in(Person, Topic) :-
        practiced(Person, Topic, Hours),
        Hours > 10000)).

custom_rule(knowledge_flow,
    (influences(Concept1, Concept2) :-
        note_mentions(Note, Concept1),
        note_mentions(Note, Concept2),
        temporal_order(Concept1, Concept2))).
```

### Personalization Settings

```prolog
% Configure personal preferences
set_preference(learning_style, visual).
set_preference(reminder_frequency, daily).
set_preference(goal_review_period, weekly).
```

## Web Interface

The system includes a web-based interface for easier interaction:

```bash
# Start web server
?- start_web_interface(8080).
# Navigate to http://localhost:8080
```

Features:
- Visual knowledge graph browser
- Note editor with auto-completion
- Timeline visualization
- Goal dashboard
- Search interface

## Testing

Run the comprehensive test suite:

```bash
./quick_test.sh
```

## Examples

See the `examples/` directory for:
- Personal learning journal setup
- Research note organization
- Project management system
- Book and article summaries

## License

MIT License - see LICENSE file for details.
