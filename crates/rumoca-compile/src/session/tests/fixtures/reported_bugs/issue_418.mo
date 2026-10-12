model B
  Real x(start = 1, fixed = true);
  Real y;
equation
  y = 2*x;      // remove this line (and use der(x) = -2*x) and the export succeeds
  der(x) = -y;
end B;
