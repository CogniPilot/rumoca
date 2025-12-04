// Test fixtures for parameter-based array dimensions and for-loop ranges
// Tests that for-loops with parameter-based ranges are correctly counted
package ParameterArrays
  // Simple model with parameter-based for-loop
  // n = 3, x[3] states = 3 unknowns, for i in 1:n = 3 equations
  model SimpleParameterForLoop
    parameter Integer n = 3 "Array size";
    Real x[n];
  equation
    for i in 1:n loop
      der(x[i]) = -x[i];
    end for;
  end SimpleParameterForLoop;

  // Model with arithmetic in parameter
  // n = 2, x[4] states = 4 unknowns, for i in 1:2*n = 4 equations
  model ParameterArithmetic
    parameter Integer n = 2 "Half array size";
    Real x[2 * n];
  equation
    for i in 1:2 * n loop
      der(x[i]) = -x[i];
    end for;
  end ParameterArithmetic;

  // Model with multiple parameters
  // n = 2, m = 3, total states = 5, equations: for i in 1:n (2) + for j in 1:m (3) = 5
  model MultipleParameters
    parameter Integer n = 2;
    parameter Integer m = 3;
    Real x[n];
    Real y[m];
  equation
    for i in 1:n loop
      der(x[i]) = -x[i];
    end for;
    for j in 1:m loop
      der(y[j]) = -y[j];
    end for;
  end MultipleParameters;

  // Model with nested for-loop
  // n = 2, m = 2, total iterations = 4
  model NestedForLoop
    parameter Integer n = 2;
    parameter Integer m = 2;
    Real x[n, m];
  equation
    for i in 1:n loop
      for j in 1:m loop
        der(x[i, j]) = -x[i, j];
      end for;
    end for;
  end NestedForLoop;
end ParameterArrays;
