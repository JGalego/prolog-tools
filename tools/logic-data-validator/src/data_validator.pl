:- module(data_validator, [
    validate_data/3,        % validate_data(+Data, +Rules, -Violations)
    load_rules/1,           % load_rules(+RuleFile)
    validate_file/3,        % validate_file(+DataFile, +RuleFile, -Violations)
    add_rule/2,             % add_rule(+RuleName, +Rule)
    remove_rule/1,          % remove_rule(+RuleName)
    list_rules/0,           % list_rules
    validate_fact/2,        % validate_fact(+Fact, +Rules)
    batch_validate/3,       % batch_validate(+DataList, +Rules, -AllViolations)
    generate_report/2       % generate_report(+Violations, +Format)
]).

/** <module> Logic-Based Data Validator

This module provides a framework for defining and applying Prolog rules
to validate data for inconsistencies, violations, or anomalies.

@author Prolog Tools Collection
@version 1.0.0
@license MIT
*/

:- dynamic validation_rule/2.
:- dynamic data_fact/1.

%% validate_data(+Data, +Rules, -Violations) is det.
%
%  Validates a list of data facts against validation rules.
%  
%  @param Data List of data facts to validate
%  @param Rules List of validation rules  
%  @param Violations List of detected violations
%
validate_data(Data, Rules, Violations) :-
    retractall(data_fact(_)),
    retractall(validation_rule(_, _)),
    assert_data_facts(Data),
    assert_rules(Rules),
    findall(violation(Rule, Fact, Reason), 
            check_violation(Rule, Fact, Reason),
            Violations).

%% validate_fact(+Fact, +Rules) is det.
%
%  Validates a single fact against all rules.
%
validate_fact(Fact, Rules) :-
    validate_data([Fact], Rules, Violations),
    (Violations = [] -> 
        write('Fact is valid') ; 
        format('Violations found: ~w~n', [Violations])
    ).

%% batch_validate(+DataList, +Rules, -AllViolations) is det.
%
%  Validates multiple datasets against the same rules.
%
batch_validate([], _, []).
batch_validate([Data|DataRest], Rules, [Violations|ViolationsRest]) :-
    validate_data(Data, Rules, Violations),
    batch_validate(DataRest, Rules, ViolationsRest).

%% load_rules(+RuleFile) is det.
%
%  Loads validation rules from a Prolog file.
%
load_rules(RuleFile) :-
    exists_file(RuleFile),
    consult(RuleFile),
    !.
load_rules(RuleFile) :-
    format('Error: Rule file ~w not found~n', [RuleFile]).

%% validate_file(+DataFile, +RuleFile, -Violations) is det.
%
%  Validates data from a file against rules from another file.
%
validate_file(DataFile, RuleFile, Violations) :-
    load_rules(RuleFile),
    read_data_file(DataFile, Data),
    findall(rule(Name, Body), validation_rule(Name, Body), Rules),
    validate_data(Data, Rules, Violations).

%% add_rule(+RuleName, +Rule) is det.
%
%  Dynamically adds a validation rule.
%
add_rule(RuleName, Rule) :-
    assertz(validation_rule(RuleName, Rule)).

%% remove_rule(+RuleName) is det.
%
%  Removes a validation rule.
%
remove_rule(RuleName) :-
    retractall(validation_rule(RuleName, _)).

%% list_rules is det.
%
%  Lists all current validation rules.
%
list_rules :-
    forall(validation_rule(Name, Rule),
           format('Rule ~w: ~w~n', [Name, Rule])).

%% generate_report(+Violations, +Format) is det.
%
%  Generates a report of violations in specified format.
%
generate_report(Violations, text) :-
    format('=== DATA VALIDATION REPORT ===~n'),
    format('Total violations: ~w~n~n', [length(Violations)]),
    forall(member(violation(Rule, Fact, Reason), Violations),
           format('VIOLATION: ~w~n  Fact: ~w~n  Reason: ~w~n~n', 
                  [Rule, Fact, Reason])).

generate_report(Violations, json) :-
    format('{"violations": ['),
    write_json_violations(Violations),
    format(']}~n').

generate_report(Violations, csv) :-
    format('Rule,Fact,Reason~n'),
    forall(member(violation(Rule, Fact, Reason), Violations),
           format('~w,~w,~w~n', [Rule, Fact, Reason])).

% Helper predicates

assert_data_facts([]).
assert_data_facts([Fact|Rest]) :-
    assertz(data_fact(Fact)),
    assert_data_facts(Rest).

assert_rules([]).
assert_rules([rule(Name, Body)|Rest]) :-
    assertz(validation_rule(Name, Body)),
    assert_rules(Rest).

check_violation(RuleName, Fact, Reason) :-
    validation_rule(RuleName, Rule),
    data_fact(Fact),
    \+ call(Rule),
    format_atom(Reason, 'Rule ~w violated by fact ~w', [RuleName, Fact]).

read_data_file(File, Data) :-
    open(File, read, Stream),
    read_terms(Stream, Data),
    close(Stream).

read_terms(Stream, [Term|Rest]) :-
    read_term(Stream, Term, []),
    Term \== end_of_file,
    !,
    read_terms(Stream, Rest).
read_terms(_, []).

write_json_violations([]).
write_json_violations([violation(Rule, Fact, Reason)]) :-
    format('{"rule":"~w","fact":"~w","reason":"~w"}', [Rule, Fact, Reason]).
write_json_violations([violation(Rule, Fact, Reason)|Rest]) :-
    format('{"rule":"~w","fact":"~w","reason":"~w"},', [Rule, Fact, Reason]),
    write_json_violations(Rest).

% Built-in validation rules

%% Range validation
validate_range(Field, Min, Max) :-
    data_fact(Fact),
    Fact =.. [_|Args],
    nth1(Field, Args, Value),
    number(Value),
    Value >= Min,
    Value =< Max.

%% Type validation  
validate_type(Field, Type) :-
    data_fact(Fact),
    Fact =.. [_|Args],
    nth1(Field, Args, Value),
    check_type(Value, Type).

check_type(Value, integer) :- integer(Value).
check_type(Value, float) :- float(Value).
check_type(Value, number) :- number(Value).
check_type(Value, atom) :- atom(Value).
check_type(Value, string) :- string(Value).

%% Uniqueness validation
validate_unique(Field) :-
    findall(Value, (data_fact(Fact), 
                   Fact =.. [_|Args], 
                   nth1(Field, Args, Value)), Values),
    list_to_set(Values, UniqueValues),
    length(Values, L1),
    length(UniqueValues, L2),
    L1 = L2.

%% Required field validation
validate_required(Field) :-
    forall(data_fact(Fact),
           (Fact =.. [_|Args],
            nth1(Field, Args, Value),
            Value \== '',
            Value \== null)).

%% Cross-field validation
validate_dependency(Field1, Field2, Condition) :-
    forall(data_fact(Fact),
           (Fact =.. [_|Args],
            nth1(Field1, Args, Val1),
            nth1(Field2, Args, Val2),
            call(Condition, Val1, Val2))).

%% Utility predicate for formatting atoms
format_atom(Atom, Format, Args) :-
    format(atom(Atom), Format, Args).
