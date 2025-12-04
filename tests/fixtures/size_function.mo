// Test fixtures for size() function evaluation in balance checking
// Tests that size(array, dim) is correctly evaluated for parameter arrays
package SizeFunction
  // Simple model with size() in array dimension
  // a = {1,2,3}, so size(a,1) = 3, x[size(a,1)-1] = x[2]
  model SimpleSizeFunction
    parameter Real a[:] = {1, 2, 3} "Array parameter";
    Real x[size(a, 1) - 1] "Array with size-dependent dimension";
  equation
    for i in 1:size(a, 1) - 1 loop
      der(x[i]) = -x[i];
    end for;
  end SimpleSizeFunction;

  // Model with multiple size() calls
  // b = {1,2,3,4}, size(b,1) = 4
  // y[size(b,1)] = y[4], for i in 1:size(b,1) = 4 iterations
  model MultipleSizeCalls
    parameter Real b[:] = {1, 2, 3, 4};
    Real y[size(b, 1)];
  equation
    for i in 1:size(b, 1) loop
      der(y[i]) = -y[i];
    end for;
  end MultipleSizeCalls;

  // Model with nested size() in for-loop range
  // c = {1,2}, size(c,1) = 2
  model SizeInForLoop
    parameter Integer n = 3;
    parameter Real c[:] = {1, 2};
    Real z[n];
  equation
    // First size(c,1)=2 equations
    for i in 1:size(c, 1) loop
      der(z[i]) = -z[i];
    end for;
    // Remaining n - size(c,1) = 1 equation
    der(z[n]) = -z[n];
  end SizeInForLoop;

  // Model with size() of a 2D array
  // mat[2,3], size(mat,1)=2, size(mat,2)=3
  model Size2DArray
    parameter Real mat[2, 3] = {{1, 2, 3}, {4, 5, 6}};
    Real x[size(mat, 1)]; // x[2]
    Real y[size(mat, 2)]; // y[3]
  equation
    for i in 1:size(mat, 1) loop
      der(x[i]) = -x[i];
    end for;
    for j in 1:size(mat, 2) loop
      der(y[j]) = -y[j];
    end for;
  end Size2DArray;
end SizeFunction;
