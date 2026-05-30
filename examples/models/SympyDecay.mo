model SympyDecay
  Real x(start=1);
  parameter Real k = 0.5;
equation
  der(x) = -k*x;
end SympyDecay;
