model Seed
  parameter Real k = 1.0;
  Real x(start = 0);
equation
  der(x) = -k * x;
end Seed;
