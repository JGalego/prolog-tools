-- Sample Database Schema and Data
-- This SQL script creates sample tables and data for testing the Prolog-SQL bridge

-- Create Employee table
CREATE TABLE employee (
    id INTEGER PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    department VARCHAR(50) NOT NULL,
    salary DECIMAL(10,2),  -- Allow NULL for testing
    hire_date DATE,
    manager_id INTEGER,
    FOREIGN KEY (manager_id) REFERENCES employee(id)
);

-- Create Department table
CREATE TABLE department (
    name VARCHAR(50) PRIMARY KEY,
    manager VARCHAR(100),
    budget DECIMAL(12,2)
);

-- Create Project table
CREATE TABLE project (
    id INTEGER PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    department VARCHAR(50) NOT NULL,
    budget DECIMAL(10,2),
    start_date DATE,
    end_date DATE,
    FOREIGN KEY (department) REFERENCES department(name)
);

-- Create Employee_Project junction table
CREATE TABLE employee_project (
    employee_id INTEGER,
    project_id INTEGER,
    role VARCHAR(50),
    allocation_percent DECIMAL(3,1),
    PRIMARY KEY (employee_id, project_id),
    FOREIGN KEY (employee_id) REFERENCES employee(id),
    FOREIGN KEY (project_id) REFERENCES project(id)
);

-- Insert sample departments
INSERT INTO department (name, manager, budget) VALUES
('Engineering', 'Alice Johnson', 2500000.00),
('Marketing', 'Bob Smith', 1200000.00),
('Sales', 'Carol Davis', 1800000.00),
('HR', 'David Wilson', 800000.00),
('Finance', 'Eve Brown', 1000000.00);

-- Insert sample employees
INSERT INTO employee (id, name, department, salary, hire_date, manager_id) VALUES
(1, 'Alice Johnson', 'Engineering', 120000.00, '2020-01-15', NULL),
(2, 'Bob Smith', 'Marketing', 95000.00, '2019-03-22', NULL),
(3, 'Carol Davis', 'Sales', 85000.00, '2021-06-10', NULL),
(4, 'David Wilson', 'HR', 75000.00, '2018-09-05', NULL),
(5, 'Eve Brown', 'Finance', 90000.00, '2020-11-30', NULL),
(6, 'Frank Miller', 'Engineering', 95000.00, '2021-02-14', 1),
(7, 'Grace Lee', 'Engineering', 88000.00, '2022-04-18', 1),
(8, 'Henry Garcia', 'Engineering', 82000.00, '2023-01-09', 1),
(9, 'Ivy Chen', 'Marketing', 65000.00, '2022-07-25', 2),
(10, 'Jack Rodriguez', 'Marketing', 58000.00, '2023-03-12', 2),
(11, 'Kate Thompson', 'Sales', 72000.00, '2021-08-16', 3),
(12, 'Liam Anderson', 'Sales', 68000.00, '2022-01-20', 3),
(13, 'Maya Patel', 'Sales', 75000.00, '2022-09-08', 3),
(14, 'Noah Williams', 'HR', 55000.00, '2021-05-03', 4),
(15, 'Olivia Taylor', 'Finance', 70000.00, '2022-10-11', 5),
(16, 'Paul Martinez', 'Finance', 65000.00, '2023-02-28', 5),
(17, 'Quinn Jackson', 'Engineering', 78000.00, '2023-06-15', 1),
(18, 'Ruby White', 'Marketing', 62000.00, '2023-04-22', 2),
(19, 'Sam Harris', 'Sales', 69000.00, '2023-08-07', 3),
(20, 'Tina Clark', 'HR', 52000.00, '2023-07-14', 4);

-- Insert sample projects
INSERT INTO project (id, name, department, budget, start_date, end_date) VALUES
(1, 'Web Platform Redesign', 'Engineering', 500000.00, '2023-01-01', '2023-12-31'),
(2, 'Mobile App Development', 'Engineering', 750000.00, '2023-03-01', '2024-02-29'),
(3, 'Database Migration', 'Engineering', 300000.00, '2023-06-01', '2023-11-30'),
(4, 'Brand Refresh Campaign', 'Marketing', 200000.00, '2023-02-01', '2023-08-31'),
(5, 'Social Media Strategy', 'Marketing', 150000.00, '2023-04-01', '2023-12-31'),
(6, 'Q4 Sales Push', 'Sales', 100000.00, '2023-10-01', '2023-12-31'),
(7, 'Customer Retention Program', 'Sales', 180000.00, '2023-05-01', '2024-04-30'),
(8, 'Employee Wellness Initiative', 'HR', 75000.00, '2023-01-01', '2023-12-31'),
(9, 'Performance Review System', 'HR', 120000.00, '2023-07-01', '2024-06-30'),
(10, 'Budget Planning Tool', 'Finance', 200000.00, '2023-09-01', '2024-08-31');

-- Insert sample employee-project assignments
INSERT INTO employee_project (employee_id, project_id, role, allocation_percent) VALUES
(1, 1, 'Project Lead', 50.0),
(1, 2, 'Technical Advisor', 25.0),
(6, 1, 'Senior Developer', 80.0),
(7, 1, 'Developer', 90.0),
(8, 1, 'Junior Developer', 100.0),
(17, 1, 'QA Engineer', 70.0),
(6, 2, 'Lead Developer', 20.0),
(7, 2, 'Mobile Developer', 10.0),
(8, 3, 'Database Specialist', 0.0),
(17, 3, 'QA Engineer', 30.0),
(2, 4, 'Campaign Manager', 60.0),
(9, 4, 'Marketing Specialist', 80.0),
(10, 4, 'Content Creator', 70.0),
(18, 4, 'Designer', 90.0),
(2, 5, 'Strategy Lead', 40.0),
(9, 5, 'Social Media Manager', 20.0),
(18, 5, 'Content Creator', 10.0),
(3, 6, 'Sales Director', 30.0),
(11, 6, 'Account Manager', 50.0),
(12, 6, 'Sales Representative', 60.0),
(13, 6, 'Sales Representative', 55.0),
(19, 6, 'Sales Representative', 40.0),
(3, 7, 'Program Manager', 70.0),
(11, 7, 'Customer Success Manager', 50.0),
(12, 7, 'Account Specialist', 40.0),
(4, 8, 'HR Director', 40.0),
(14, 8, 'Wellness Coordinator', 60.0),
(20, 8, 'Administrative Assistant', 30.0),
(4, 9, 'System Owner', 60.0),
(14, 9, 'Implementation Specialist', 40.0),
(5, 10, 'Finance Director', 50.0),
(15, 10, 'Senior Analyst', 75.0),
(16, 10, 'Financial Analyst', 80.0);

-- Create some views for more complex queries
CREATE VIEW employee_summary AS
SELECT 
    e.id,
    e.name,
    e.department,
    e.salary,
    e.hire_date,
    m.name as manager_name,
    d.budget as dept_budget
FROM employee e
LEFT JOIN employee m ON e.manager_id = m.id
JOIN department d ON e.department = d.name;

CREATE VIEW project_summary AS
SELECT 
    p.id,
    p.name,
    p.department,
    p.budget,
    p.start_date,
    p.end_date,
    COUNT(ep.employee_id) as employee_count,
    AVG(ep.allocation_percent) as avg_allocation
FROM project p
LEFT JOIN employee_project ep ON p.id = ep.project_id
GROUP BY p.id, p.name, p.department, p.budget, p.start_date, p.end_date;

-- Create indexes for better performance
CREATE INDEX idx_employee_department ON employee(department);
CREATE INDEX idx_employee_salary ON employee(salary);
CREATE INDEX idx_employee_hire_date ON employee(hire_date);
CREATE INDEX idx_project_department ON project(department);
CREATE INDEX idx_project_dates ON project(start_date, end_date);

-- Insert some test data for edge cases
INSERT INTO employee (id, name, department, salary, hire_date, manager_id) VALUES
(21, 'Test NULL Salary', 'Engineering', NULL, '2023-01-01', 1),
(22, 'Test High Salary', 'Engineering', 250000.00, '2023-01-01', 1);

-- Add a department with no employees for testing
INSERT INTO department (name, manager, budget) VALUES
('Research', 'TBD', 500000.00);

-- Add some historical data for temporal testing
INSERT INTO employee (id, name, department, salary, hire_date, manager_id) VALUES
(23, 'Former Employee', 'Engineering', 80000.00, '2018-01-01', 1);

-- Comments for testing:
-- This database contains:
-- - 23 employees across 5 departments (plus 1 empty department)
-- - 10 active projects with varying budgets and timelines
-- - Employee-project assignments with different roles and allocations
-- - Manager-employee relationships
-- - Some edge cases (NULL salary, very high salary, empty department)
-- - Views for complex queries
-- - Indexes for performance testing

-- Usage with Prolog-SQL bridge:
-- 1. Connect: sql_connect('DSN=sample_db', Connection)
-- 2. Register: sql_register_tables(Connection)
-- 3. Query: employee(ID, Name, Department, Salary)
-- 4. Complex: employee(ID, Name, 'Engineering', Salary), Salary > 80000
