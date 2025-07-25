% Custom validation rules for specific business logic

% Rule: Employee age must be between 18 and 65
:- validation_rule(working_age, 
    forall(data_fact(employee(_, _, Age, _)), 
           (Age >= 18, Age =< 65))).

% Rule: All email addresses must be unique
:- validation_rule(unique_emails,
    (findall(Email, data_fact(user(_, Email, _)), Emails),
     list_to_set(Emails, UniqueEmails),
     length(Emails, L1),
     length(UniqueEmails, L2),
     L1 = L2)).

% Rule: Department budgets must not exceed company total
:- validation_rule(budget_constraint,
    (findall(Budget, data_fact(department(_, _, Budget)), Budgets),
     sum_list(Budgets, Total),
     data_fact(company_budget(MaxBudget)),
     Total =< MaxBudget)).

% Rule: Project end dates must be after start dates
:- validation_rule(project_dates,
    forall(data_fact(project(_, _, Start, End)),
           date_before(Start, End))).

% Rule: Employee salary must be within department range
:- validation_rule(salary_range,
    forall((data_fact(employee(_, Dept, _, Salary)),
            data_fact(dept_salary_range(Dept, Min, Max))),
           (Salary >= Min, Salary =< Max))).

% Helper predicate
date_before(date(Y1,M1,D1), date(Y2,M2,D2)) :-
    (Y1 < Y2 ; 
     (Y1 = Y2, M1 < M2) ; 
     (Y1 = Y2, M1 = M2, D1 < D2)).
