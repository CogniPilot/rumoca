model InitialEquation "Model with initial equations"
  parameter Real x0 = 1.0 "Initial position";
  parameter Real v0 = 0.0 "Initial velocity";
  Real x "Position";
  Real v "Velocity";
equation
  der(x) = v;
  der(v) = -x;
initial equation
  x = x0;
  v = v0;
end InitialEquation;
