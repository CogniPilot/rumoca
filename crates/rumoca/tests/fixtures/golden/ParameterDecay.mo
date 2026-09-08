model ParameterDecay
  parameter Real a = -1.0;
  Real x(start = 2.0, fixed = true);
equation
  der(x) = a * x;
end ParameterDecay;
