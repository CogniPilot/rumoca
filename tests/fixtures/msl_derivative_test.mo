model DerivativeTest
  Modelica.Blocks.Continuous.Derivative D(k = 1, T = 0.1);
equation
  D.u = 1.0;
end DerivativeTest;
