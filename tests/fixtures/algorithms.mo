// Test fixtures for algorithm section balance checking
package Algorithms
  // Simple algorithm block with single output
  // Should be balanced: 1 eq (from algorithm), 1 unk (y)
  block SimpleAlgorithm
    input Real u;
    output Real y;
  algorithm
    y := u * 2;
  end SimpleAlgorithm;

  // Algorithm with multiple outputs
  // Should be balanced: 2 eq (from algorithm), 2 unk (y1, y2)
  block MultipleOutputs
    input Real u;
    output Real y1;
    output Real y2;
  algorithm
    y1 := u;
    y2 := u * 2;
  end MultipleOutputs;

  // Algorithm with if statement
  // Should be balanced: 1 eq, 1 unk (y assigned in both branches)
  block AlgorithmWithIf
    input Real u;
    output Real y;
  algorithm
    if u > 0 then
      y := u;
    else
      y := -u;
    end if;
  end AlgorithmWithIf;

  // Algorithm with for loop
  // Should be balanced: 1 eq, 1 unk (y is assigned)
  block AlgorithmWithFor
    input Real u;
    output Real y;
    parameter Integer n = 3;
  algorithm
    y := 0;
    for i in 1:n loop
      y := y + u;
    end for;
  end AlgorithmWithFor;

  // Mixed equation and algorithm sections
  // Should be balanced: 2 eq (1 from equation, 1 from algorithm), 2 unk (x, y)
  block MixedEquationAlgorithm
    input Real u;
    Real x;
    output Real y;
  equation
    x = u + 1;
  algorithm
    y := x * 2;
  end MixedEquationAlgorithm;

  // Digital-like table source (like MSL Digital.Sources.Table)
  // Should be balanced: 1 eq (from algorithm), 1 unk (y)
  block TableSource
    parameter Real values[:] = {0, 1, 0};
    parameter Real times[:] = {0, 0.5, 1.0};
    output Real y;
  algorithm
    // Simplified - just assign first value
    y := values[1];
    for i in 1:size(values, 1) loop
      if time >= times[i] then
        y := values[i];
      end if;
    end for;
  end TableSource;
end Algorithms;
