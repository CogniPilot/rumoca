model MatrixTest "Test model for matrix operations with array dimensions"
  // 2x3 matrix A - using zeros initialization
  Real A[2, 3];
  // 3x4 matrix B
  Real B[3, 4];
  // Result C = A * B will be 2x4
  Real C[2, 4];
  // Scalar parameter
  parameter Real scale = 2.0;
  // Vector test
  Real v[3];
  Real w[3];
  // Dynamic state variable (1D array)
  Real x[2](start = 0.0);
equation
  // Matrix multiplication C = A * B
  C = A * B;
  // Vector scaling
  w = scale * v;
  // Dynamic equation with array subscripting
  der(x[1]) = -0.1 * x[1] + x[2];
  der(x[2]) = -0.2 * x[2];
end MatrixTest;
