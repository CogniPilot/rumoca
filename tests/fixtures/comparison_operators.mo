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
      y = u;
    else
      y = 2 * u;
    end if;
  end EqualityTrue;

  // 1 equation when n == 0
  // Different equation when n != 0
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
      y = 2 * u;
    end if;
  end EqualityFalse;

  // 1 equation when n != 0
  // Testing inequality (<>)
  // With n=5, condition n<>0 is true
  model InequalityTrue
    parameter Integer n = 5;
    input Real u;
    output Real y;
  equation
    if n <> 0 then
      y = 3 * u;
    else
      y = u;
    end if;
  end InequalityTrue;

  // 1 equation when n <> 0
  // Testing less than (<)
  // With n=2, condition n<5 is true
  model LessThanTrue
    parameter Integer n = 2;
    input Real u;
    output Real y;
  equation
    if n < 5 then
      y = u;
    else
      y = 2 * u;
    end if;
  end LessThanTrue;

  // 1 equation when n < 5
  // Testing greater than (>)
  // With n=10, condition n>5 is true
  model GreaterThanTrue
    parameter Integer n = 10;
    input Real u;
    output Real y;
  equation
    if n > 5 then
      y = u;
    else
      y = 2 * u;
    end if;
  end GreaterThanTrue;

  // 1 equation when n > 5
  // Testing less than or equal (<=)
  // With n=5, condition n<=5 is true
  model LessEqualTrue
    parameter Integer n = 5;
    input Real u;
    output Real y;
  equation
    if n <= 5 then
      y = u;
    else
      y = 2 * u;
    end if;
  end LessEqualTrue;

  // 1 equation when n <= 5
  // Testing greater than or equal (>=)
  // With n=5, condition n>=5 is true
  model GreaterEqualTrue
    parameter Integer n = 5;
    input Real u;
    output Real y;
  equation
    if n >= 5 then
      y = u;
    else
      y = 2 * u;
    end if;
  end GreaterEqualTrue;

  // 1 equation when n >= 5
  // Model with comparison using size() function
  // a = {1,2,3}, size(a,1) = 3, so nx = size(a,1) - 1 = 2
  // Condition nx == 0 is false, so we use else branch (2 equations)
  model SizeComparisonFalse
    parameter Real a[:] = {1, 2, 3};
    parameter Integer nx = size(a, 1) - 1; // nx = 2
    Real x[nx]; // x[2]
  equation
    if nx == 0 then
    else
      for i in 1:nx loop
        der(x[i]) = -x[i];
      end for;
    end if;
  end SizeComparisonFalse;

  // This branch should not be counted (0 equations here)
  // This branch should be counted: 2 equations
  // Model where size() comparison is true
  // a = {1}, size(a,1) = 1, so nx = size(a,1) - 1 = 0
  // Condition nx == 0 is true, so we use then branch (0 equations, 0 unknowns)
  model SizeComparisonTrue
    parameter Real a[:] = {1};
    parameter Integer nx = size(a, 1) - 1; // nx = 0
    input Real u;
    output Real y;
    // Real x[nx];  // x[0] - empty array, no unknowns
  equation
    if nx == 0 then
      y = u;
    else
      y = 2 * u;
    end if;
  end SizeComparisonTrue;

  // 1 equation when nx == 0
  // This branch should not be counted
  // Simpler test to debug the issue
  // This is similar to SizeComparisonTrue but with protected parameters
  // a = {1}, so nx = size(a, 1) - 1 = 0
  // if nx == 0 should be true, so 1 equation
  model ProtectedParamTest
    parameter Real a[:] = {1};
    input Real u;
    output Real y;
    parameter Integer nx = size(a, 1) - 1; // nx = 0 (protected)
  equation
    if nx == 0 then
      y = u;
    else
      y = 2 * u;
    end if;
  end ProtectedParamTest;

  // 1 equation when nx == 0
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
    parameter Integer na = size(a, 1); // na = 1
    parameter Integer nb = size(b, 1); // nb = 1
    parameter Integer nx = size(a, 1) - 1; // nx = 0
    parameter Real d = b[1] / a[1]; // d = 1
    output Real x[nx]; // x[0] with default a={1}
    Real x_scaled[nx]; // x_scaled[0] with default
  equation
    if nx == 0 then
      y = d * u;
    else
      der(x_scaled[1]) = (u) / a[1];
      der(x_scaled[2:nx]) = x_scaled[1:nx - 1];
      y = x_scaled[nx] + d * u;
      x = x_scaled;
    end if;
  end TransferFunctionLike;
end ComparisonOperators;
