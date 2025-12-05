model ArrayLiteralTest
  // Simple 1D array literal
  parameter Real v1[3] = {1.0, 2.0, 3.0};
  // Nested 2D array literal (matrix)
  parameter Real A[2, 3] = {{1.0, 2.0, 3.0}, {4.0, 5.0, 6.0}};
  // Array with integer elements
  parameter Integer idx[4] = {1, 2, 3, 4};
  // Array with expressions
  Real y[3];
  Real x(start = 0.0);
equation
  der(x) = -0.1 * x;
  y = v1 * x;
end ArrayLiteralTest;
