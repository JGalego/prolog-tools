% Sample data for testing validation rules

% Person data
person(1, 'John Doe', 25, 'Engineer').
person(2, 'Jane Smith', 30, 'Manager').
person(3, 'Bob Wilson', -5, 'Developer').  % Invalid age
person(4, 'Alice Brown', 200, 'Analyst').  % Invalid age

% Employee data
employee(101, 'John', 'Engineering', 50000).
employee(102, 'Jane', 'Management', 80000).
employee(103, 'Bob', 'Development', 60000).
employee(104, 'Alice', 'Analysis', 1500000).  % Invalid salary

% User data with emails
user(1, 'john@example.com', 'active').
user(2, 'jane.smith@company.org', 'active').
user(3, 'invalid-email', 'inactive').  % Invalid email
user(4, 'missing@domain', 'active').

% Event data
event(1, date(2024,1,15), date(2024,1,20)).
event(2, date(2024,2,10), date(2024,2,5)).  % Invalid: end before start
event(3, date(2024,3,1), date(2024,3,1)).   % Valid: same day

% Manager-Employee relationships
manager(201, 101, 'Senior Manager', 90000).  % Manages John
manager(202, 103, 'Team Lead', 45000).       % Invalid: lower salary than Bob

% Customer and Order data
customer(1001, 'ACME Corp', 'active').
customer(1002, 'TechCo', 'active').

order(5001, 1001, 'laptop').      % Valid reference
order(5002, 1002, 'software').    % Valid reference  
order(5003, 9999, 'mouse').       % Invalid reference - customer doesn't exist
