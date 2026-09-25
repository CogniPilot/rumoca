model LinearLoop
  Real x1(start = 1);
  Real x2(start = 0, fixed = true);
  Real v1(start = 0);
  Real v2(start = 1, fixed = true);
  Real lambda;
equation
  der(x1) = v1;
  der(x2) = v2;
  der(v1) = -lambda - x1;
  der(v2) = -2*lambda;
  x1 + 2*x2 = 1;
end LinearLoop;
