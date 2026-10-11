model C3
  parameter Real m[3, 2] = {i + 10 * j for i in 1:3, j in 1:2};
  Real y(start = 1, fixed = true);
equation
  der(y) = -m[3, 2] * y;
end C3;
