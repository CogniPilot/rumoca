model P
  input Real u = 0;
  parameter Integer tag = 1;
  Real x(start = 1, fixed = true);
  output Real y;
equation
  der(x) = -x + u;
  y = 2*x;
end P;
