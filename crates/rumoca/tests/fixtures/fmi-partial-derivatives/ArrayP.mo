model ArrayP
  input Real u[2] = {0, 0};
  parameter Integer tag = 1;
  Real x[2](start = {1, 2}, each fixed = true);
  output Real y[2];
equation
  der(x) = {-x[1] + u[1], -2*x[2] + u[2]};
  y = {2*x[1] + 3*u[2], x[1] - x[2]};
end ArrayP;
