// Test fixtures for array balance checking
// Tests that array variables and for-loop equations are properly counted
package ArrayBalance
  // Simple array state model
  // n array elements = n unknowns, n equations from for loop
  model ArrayState
    parameter Integer n = 3;
    Real x(start = 0);
  equation
    for i in 1:n loop
      der(x[i]) = -x[i];
    end for;
  end ArrayState;

  // State space model (simplified)
  // x[n] states + y[p] outputs = n + p unknowns
  // n state equations + p output equations = n + p equations
  model SimpleStateSpace
    parameter Integer n = 2 "Number of states";
    parameter Integer p = 1 "Number of outputs";
    parameter Real A[2, 2] = {{-1, 0}, {0, -2}};
    parameter Real B[2, 1] = {{1}, {1}};
    parameter Real C[1, 2] = {{1, 1}};
    parameter Real D[1, 1] = {{0}};
    input Real u;
    Real x[n](each start = 0);
    output Real y[p];
  equation
    // n equations for states
    for i in 1:n loop
      der(x[i]) = sum({ A[i, j] * x[j] for j in 1:n }) + B[i, 1] * u;
    end for;
    // p equations for outputs
    for i in 1:p loop
      y[i] = sum({ C[i, j] * x[j] for j in 1:n }) + D[i, 1] * u;
    end for;
  end SimpleStateSpace;

  // Transfer function (simplified)
  // n states = n unknowns
  // 1 output equation + n-1 state equations + 1 additional state eq = n+1? No...
  // Actually: x[n] states, y output
  // n equations from der(x[i]) = ... for i in 1:n
  // 1 equation for y = c*x + d*u
  // Total: n+1 unknowns (x[n] + y), n+1 equations
  model SimpleTransferFunction
    parameter Integer n = 2 "Order";
    parameter Real b = {1, 0, 0} "Numerator coefficients";
    parameter Real a = {1, 2, 1} "Denominator coefficients";
    input Real u;
    output Real y;
    Real x(start = 0);
  equation
    // Controllable canonical form
    der(x[1]) = -a[2] / a[1] * x[1] + (if n > 1 then x[2] else 0) + b[2] / a[1] * u;
    // For n > 1, chain the states
    for i in 2:n loop
      der(x[i]) = -a[i + 1] / a[1] * x[1] + (if i < n then x[i + 1] else 0) + b[i + 1] / a[1] * u;
    end for;
    y = x[1] + b[1] / a[1] * u;
  end SimpleTransferFunction;

  // Simplest possible array test - fixed size
  model FixedArrayState
    Real x[3](start = 0);
  equation
    der(x[1]) = -x[1];
    der(x[2]) = -x[2];
    der(x[3]) = -x[3];
  end FixedArrayState;

  // Array test with explicit for loop
  model ForLoopArrayState
    Real x[3](start = 0);
  equation
    for i in 1:3 loop
      der(x[i]) = -x[i];
    end for;
  end ForLoopArrayState;

  // Vector equation test - no for loop, just array assignment
  model VectorEquation
    Real x[3];
    Real y[3];
  equation
    x = y;
    // This is 3 equations
    der(y) = -y;
  end VectorEquation;
end ArrayBalance;
// This is 3 equations
