model TimeP
  input Real u = 0;
  Real x(start = 1, fixed = true);
  output Real y;
equation
  der(x) = time*x + u;
  y = 3*time + 2*x;
end TimeP;
