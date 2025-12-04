// Test fixtures for comparison operator evaluation in balance checking
// Tests that ==, <>, <, <=, >, >= are correctly evaluated for parameter conditions

package ComparisonOperators

// Simple model testing equality comparison
// With n=0, condition is true, so this model has 1 equation
model EqualityTrue
  parameter Integer n = 0;
  input Real u;
  output Real y;
equation
  if n == 0 then
    y = u;  // 1 equation when n == 0
  else
    y = 2*u;  // Different equation when n != 0
  end if;
end EqualityTrue;

// Testing equality with non-zero value
// With n=3, condition n==0 is false, still 1 equation from else
model EqualityFalse
  parameter Integer n = 3;
  input Real u;
  output Real y;
equation
  if n == 0 then
    y = u;
  else
    y = 2*u;  // 1 equation when n != 0
  end if;
end EqualityFalse;

// Testing inequality (<>)
// With n=5, condition n<>0 is true
model InequalityTrue
  parameter Integer n = 5;
  input Real u;
  output Real y;
equation
  if n <> 0 then
    y = 3*u;  // 1 equation when n <> 0
  else
    y = u;
  end if;
end InequalityTrue;

// Testing less than (<)
// With n=2, condition n<5 is true
model LessThanTrue
  parameter Integer n = 2;
  input Real u;
  output Real y;
equation
  if n < 5 then
    y = u;  // 1 equation when n < 5
  else
    y = 2*u;
  end if;
end LessThanTrue;

// Testing greater than (>)
// With n=10, condition n>5 is true
model GreaterThanTrue
  parameter Integer n = 10;
  input Real u;
  output Real y;
equation
  if n > 5 then
    y = u;  // 1 equation when n > 5
  else
    y = 2*u;
  end if;
end GreaterThanTrue;

// Testing less than or equal (<=)
// With n=5, condition n<=5 is true
model LessEqualTrue
  parameter Integer n = 5;
  input Real u;
  output Real y;
equation
  if n <= 5 then
    y = u;  // 1 equation when n <= 5
  else
    y = 2*u;
  end if;
end LessEqualTrue;

// Testing greater than or equal (>=)
// With n=5, condition n>=5 is true
model GreaterEqualTrue
  parameter Integer n = 5;
  input Real u;
  output Real y;
equation
  if n >= 5 then
    y = u;  // 1 equation when n >= 5
  else
    y = 2*u;
  end if;
end GreaterEqualTrue;

// Model with comparison using size() function
// a = {1,2,3}, size(a,1) = 3, so nx = size(a,1) - 1 = 2
// Condition nx == 0 is false, so we use else branch (2 equations)
model SizeComparisonFalse
  parameter Real a[:] = {1, 2, 3};
  parameter Integer nx = size(a, 1) - 1;  // nx = 2
  Real x[nx];  // x[2]
equation
  if nx == 0 then
    // This branch should not be counted (0 equations here)
  else
    // This branch should be counted: 2 equations
    for i in 1:nx loop
      der(x[i]) = -x[i];
    end for;
  end if;
end SizeComparisonFalse;

// Model where size() comparison is true
// a = {1}, size(a,1) = 1, so nx = size(a,1) - 1 = 0
// Condition nx == 0 is true, so we use then branch (0 equations, 0 unknowns)
model SizeComparisonTrue
  parameter Real a[:] = {1};
  parameter Integer nx = size(a, 1) - 1;  // nx = 0
  input Real u;
  output Real y;
  // Real x[nx];  // x[0] - empty array, no unknowns
equation
  if nx == 0 then
    y = u;  // 1 equation when nx == 0
  else
    // This branch should not be counted
    y = 2*u;
  end if;
end SizeComparisonTrue;

// Simpler test to debug the issue
// This is similar to SizeComparisonTrue but with protected parameters
// a = {1}, so nx = size(a, 1) - 1 = 0
// if nx == 0 should be true, so 1 equation
model ProtectedParamTest
  parameter Real a[:] = {1};
  input Real u;
  output Real y;
protected
  parameter Integer nx = size(a, 1) - 1;  // nx = 0 (protected)
equation
  if nx == 0 then
    y = u;  // 1 equation when nx == 0
  else
    y = 2*u;
  end if;
end ProtectedParamTest;

// Faithful reproduction of TransferFunction structure
// a = {1}, so na = size(a,1) = 1, nx = size(a,1) - 1 = 0
// With nx=0, the if-branch (y=d*u) should be counted (1 equation)
// Unknowns: y only (since x[0] and x_scaled[0] are empty)
// Expected: 1 equation, 1 unknown = balanced
model TransferFunctionLike
  parameter Real b[:] = {1};
  parameter Real a[:] = {1};
  input Real u;
  output Real y;
  output Real x[size(a, 1) - 1];  // x[0] with default a={1}
protected
  parameter Integer na = size(a, 1);  // na = 1
  parameter Integer nb = size(b, 1);  // nb = 1
  parameter Integer nx = size(a, 1) - 1;  // nx = 0
  parameter Real d = b[1]/a[1];  // d = 1
  Real x_scaled[size(x, 1)];  // x_scaled[0] with default
equation
  if nx == 0 then
    y = d*u;  // 1 equation when nx == 0
  else
    // These equations should NOT be counted when nx == 0
    der(x_scaled[1]) = (u)/a[1];
    der(x_scaled[2:nx]) = x_scaled[1:nx-1];
    y = x_scaled[nx] + d*u;
    x = x_scaled;
  end if;
end TransferFunctionLike;

end ComparisonOperators;
