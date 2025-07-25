/**
 * Advanced Query Examples
 * 
 * This file demonstrates advanced querying capabilities and patterns.
 */

:- use_module('../src/sql_bridge').

% Example 1: Complex Filtering and Aggregation
example_complex_filtering :-
    sql_connect('DSN=company;Driver=SQLite3', Connection),
    sql_register_tables(Connection),
    
    % Find departments with average salary above threshold
    findall(Dept-AvgSalary,
            (department(Dept, _, _),
             findall(Salary, employee(_, _, Dept, Salary), Salaries),
             Salaries \= [],
             sum_list(Salaries, Total),
             length(Salaries, Count),
             AvgSalary is Total / Count,
             AvgSalary > 65000),
            HighPayingDepts),
    
    format('High-paying departments:~n'),
    forall(member(Dept-AvgSalary, HighPayingDepts),
           format('  ~w: $~2f average~n', [Dept, AvgSalary])),
    
    sql_disconnect(Connection).

% Example 2: Hierarchical Queries (Self-Joins)
example_hierarchical_queries :-
    sql_connect('DSN=org;Driver=SQLite3', Connection),
    sql_register_table(Connection, employee),
    
    % Assuming employee table has manager_id field
    % Find all direct reports for each manager
    findall(Manager-DirectReports,
            (employee(MgrID, Manager, _, _),
             findall(employee(EmpID, EmpName, Dept, Salary),
                     (employee(EmpID, EmpName, Dept, Salary),
                      employee_manager(EmpID, MgrID)),
                     DirectReports),
             DirectReports \= []),
            ManagerReports),
    
    format('Management hierarchy:~n'),
    forall(member(Manager-Reports, ManagerReports),
           (format('~w manages:~n', [Manager]),
            forall(member(employee(ID, Name, Dept, _), Reports),
                   format('  ~w: ~w (~w)~n', [ID, Name, Dept])))),
    
    sql_disconnect(Connection).

% Helper predicate for manager relationships
% This would typically be derived from a manager_id field
employee_manager(EmpID, MgrID) :-
    % Placeholder implementation
    % In real scenario, this would query a manager_id field
    employee(EmpID, _, _, _),
    employee(MgrID, _, _, _),
    EmpID \= MgrID,
    EmpID > MgrID.  % Simple heuristic

% Example 3: Data Validation and Consistency Checks
example_data_validation :-
    sql_connect('DSN=company;Driver=SQLite3', Connection),
    sql_register_tables(Connection),
    
    % Check for employees without valid departments
    findall(employee(ID, Name, Dept, Salary),
            (employee(ID, Name, Dept, Salary),
             \+ department(Dept, _, _)),
            OrphanEmployees),
    
    (   OrphanEmployees = []
    ->  format('All employees have valid departments~n')
    ;   format('Employees with invalid departments:~n'),
        forall(member(employee(ID, Name, Dept, _), OrphanEmployees),
               format('  ~w: ~w (invalid dept: ~w)~n', [ID, Name, Dept]))
    ),
    
    % Check for salary anomalies
    findall(Salary, employee(_, _, _, Salary), AllSalaries),
    statistics_summary(AllSalaries, Stats),
    Stats = stats(Min, Max, Mean, StdDev),
    Threshold is Mean + 2 * StdDev,
    
    findall(employee(ID, Name, Dept, Salary),
            (employee(ID, Name, Dept, Salary),
             Salary > Threshold),
            HighOutliers),
    
    format('Salary statistics: Min=$~2f, Max=$~2f, Mean=$~2f, StdDev=$~2f~n',
           [Min, Max, Mean, StdDev]),
    
    (   HighOutliers = []
    ->  format('No salary outliers found~n')
    ;   format('High salary outliers:~n'),
        forall(member(employee(ID, Name, Dept, Salary), HighOutliers),
               format('  ~w: ~w ($~2f)~n', [ID, Name, Salary]))
    ),
    
    sql_disconnect(Connection).

% Example 4: Temporal Queries (if date fields are available)
example_temporal_queries :-
    sql_connect('DSN=hr;Driver=SQLite3', Connection),
    sql_register_table(Connection, employee),
    
    % Assuming hire_date field exists
    % Find recently hired employees (last 90 days)
    get_time(Now),
    DaysAgo90 is Now - (90 * 24 * 60 * 60),
    
    findall(employee(ID, Name, Dept, HireDate),
            (employee(ID, Name, Dept, _, HireDate),
             parse_date(HireDate, HireTimestamp),
             HireTimestamp > DaysAgo90),
            RecentHires),
    
    format('Recent hires (last 90 days):~n'),
    forall(member(employee(ID, Name, Dept, HireDate), RecentHires),
           format('  ~w: ~w (~w) hired ~w~n', [ID, Name, Dept, HireDate])),
    
    sql_disconnect(Connection).

% Helper predicate for date parsing (simplified)
parse_date(DateString, Timestamp) :-
    % Simplified date parsing - would need proper implementation
    % for real date strings
    atom_number(DateString, Timestamp).

% Example 5: Cross-Table Analytics
example_cross_table_analytics :-
    sql_connect('DSN=analytics;Driver=SQLite3', Connection),
    sql_register_tables(Connection),
    
    % Department efficiency analysis
    findall(analysis(Dept, EmpCount, AvgSalary, TotalBudget, ProjectCount),
            (department(Dept, _, Budget),
             findall(_, employee(_, _, Dept, _), Employees),
             length(Employees, EmpCount),
             EmpCount > 0,
             findall(Salary, employee(_, _, Dept, Salary), Salaries),
             sum_list(Salaries, TotalSalary),
             AvgSalary is TotalSalary / EmpCount,
             TotalBudget = Budget,
             findall(_, project(_, Dept, _), Projects),
             length(Projects, ProjectCount)),
            DeptAnalysis),
    
    format('Department Analysis:~n'),
    format('Dept~t~20|Employees~t~30|Avg Salary~t~42|Budget~t~52|Projects~n'),
    format('~`-t~60|~n'),
    forall(member(analysis(Dept, EmpCount, AvgSalary, Budget, ProjCount), DeptAnalysis),
           format('~w~t~20|~w~t~30|$~2f~t~42|$~w~t~52|~w~n',
                  [Dept, EmpCount, AvgSalary, Budget, ProjCount])),
    
    sql_disconnect(Connection).

% Example 6: Dynamic Query Building
example_dynamic_queries :-
    sql_connect('DSN=flexible;Driver=SQLite3', Connection),
    sql_register_table(Connection, employee),
    
    % Build queries based on criteria
    Criteria = [
        dept('Engineering'),
        salary_range(50000, 100000),
        name_pattern('J%')
    ],
    
    build_employee_query(Criteria, Query),
    format('Built query: ~w~n', [Query]),
    
    % Execute the dynamically built query
    call(Query),
    
    sql_disconnect(Connection).

% Helper predicate for building dynamic queries
build_employee_query(Criteria, Query) :-
    Query = employee(ID, Name, Dept, Salary),
    % In a real implementation, this would build constraints
    % based on the criteria list
    true.  % Simplified for example

% Statistical utility predicates
statistics_summary(Values, stats(Min, Max, Mean, StdDev)) :-
    min_list(Values, Min),
    max_list(Values, Max),
    sum_list(Values, Sum),
    length(Values, Count),
    Mean is Sum / Count,
    
    % Calculate standard deviation
    maplist(squared_diff(Mean), Values, SquaredDiffs),
    sum_list(SquaredDiffs, SumSquaredDiffs),
    Variance is SumSquaredDiffs / Count,
    StdDev is sqrt(Variance).

squared_diff(Mean, Value, SquaredDiff) :-
    Diff is Value - Mean,
    SquaredDiff is Diff * Diff.

% Example runner
run_advanced_examples :-
    format('=== Complex Filtering Example ===~n'),
    catch(example_complex_filtering, Error,
          format('Error: ~w~n', [Error])),
    
    format('~n=== Hierarchical Queries Example ===~n'),
    catch(example_hierarchical_queries, Error,
          format('Error: ~w~n', [Error])),
    
    format('~n=== Data Validation Example ===~n'),
    catch(example_data_validation, Error,
          format('Error: ~w~n', [Error])),
    
    format('~n=== Temporal Queries Example ===~n'),
    catch(example_temporal_queries, Error,
          format('Error: ~w~n', [Error])),
    
    format('~n=== Cross-Table Analytics Example ===~n'),
    catch(example_cross_table_analytics, Error,
          format('Error: ~w~n', [Error])),
    
    format('~n=== Dynamic Queries Example ===~n'),
    catch(example_dynamic_queries, Error,
          format('Error: ~w~n', [Error])).
