:- module(validation_rules, [
    age_rule/0,
    salary_rule/0,
    email_rule/0,
    date_consistency_rule/0,
    business_rule_1/0,
    referential_integrity/0
]).

/** <module> Standard Validation Rules

Common validation rules for data validation.

@author Prolog Tools Collection
*/

:- use_module('../src/data_validator').

%% Age validation rule
age_rule :-
    forall(data_fact(person(_, _, Age, _)),
           (number(Age), Age >= 0, Age =< 150)).

%% Salary validation rule  
salary_rule :-
    forall(data_fact(employee(_, _, _, Salary)),
           (number(Salary), Salary >= 0, Salary =< 1000000)).

%% Email validation rule
email_rule :-
    forall(data_fact(user(_, Email, _)),
           (atom_codes(Email, Codes),
            append(Before, [64|After], Codes),  % Contains @
            Before \= [],
            After \= [])).

%% Date consistency rule
date_consistency_rule :-
    forall(data_fact(event(_, StartDate, EndDate)),
           (date_before_or_equal(StartDate, EndDate))).

%% Business rule: Manager salary > employee salary
business_rule_1 :-
    forall((data_fact(employee(EmpId, _, _, EmpSalary)),
            data_fact(manager(MgrId, EmpId, _, MgrSalary))),
           MgrSalary > EmpSalary).

%% Referential integrity
referential_integrity :-
    forall(data_fact(order(_, CustomerId, _)),
           data_fact(customer(CustomerId, _, _))).

% Helper predicates
date_before_or_equal(date(Y1,M1,D1), date(Y2,M2,D2)) :-
    (Y1 < Y2 ; 
     (Y1 = Y2, M1 < M2) ; 
     (Y1 = Y2, M1 = M2, D1 =< D2)).
