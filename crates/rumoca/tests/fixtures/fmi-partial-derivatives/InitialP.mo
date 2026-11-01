model InitialP
  parameter Real gain = 2;
  parameter Real offset(start = 6, fixed = false);
  input Real u = 0;
  Real x(start = 1, fixed = true);
  output Real y;
initial equation
  offset = 3*gain + u;
equation
  der(x) = -gain*x + offset + u;
  y = offset*x + u;
end InitialP;
