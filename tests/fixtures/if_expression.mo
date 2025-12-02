// Test if expressions
// Simple if-then-else
model SimpleIfExpression
  Real x(start = 0);
  Real y;
equation
  der(x) = 1;
  y = if x > 0 then 1 else -1;
end SimpleIfExpression;

// If expression with elseif
model ElseIfExpression
  Real x(start = -2);
  Real y;
equation
  der(x) = 1;
  y = if x < -1 then -1 elseif x > 1 then 1 else 0;
end ElseIfExpression;

// Multiple elseif branches
model MultipleElseIf
  Real x(start = -5);
  Real category;
equation
  der(x) = 1;
  // Categorize x into ranges
  category = if x < -2 then 1 elseif x < 0 then 2 elseif x < 2 then 3 else 4;
end MultipleElseIf;

// Nested if expressions
model NestedIfExpression
  Real x(start = 0);
  Real y(start = 0);
  Real z;
equation
  der(x) = 1;
  der(y) = 0.5;
  z = if x > 0 then (if y > 0 then 1 else 2) else (if y > 0 then 3 else 4);
end NestedIfExpression;

// If expression with complex conditions
model ComplexCondition
  Real x(start = 0);
  Real y(start = 1);
  Real z;
equation
  der(x) = 1;
  der(y) = -0.1;
  z = if x > 0 and y > 0 then x * y else 0;
end ComplexCondition;

// If expression in equation RHS
model IfInEquation
  parameter Real threshold = 5;
  Real x(start = 0);
  Real clamped;
equation
  der(x) = 1;
  clamped = if x > threshold then threshold elseif x < -threshold then -threshold else x;
end IfInEquation;
