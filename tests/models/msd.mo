model msd "Mass spring damper"
  parameter Real k=1.0 "Coefficient of spring";
  parameter Real c=1.0 "Coefficient of damper";
  parameter Real m=1.0 "mass";
  Real x = 0.0 "Position";
  Real v "Velocity";
  input Real u "Disturbance";
equation
  der(x) = v;
  der(v) = -(k/m)*x - (c/m)*v - (1/m)*u;
end msd;

