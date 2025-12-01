function polyEval
  input Real a;
  input Real b;
  input Real c;
  input Real x;
  output Real y;
algorithm
  y := a * x * x + b * x + c;
end polyEval;

model FunctionTest
  parameter Real a = 1;
  parameter Real b = -2;
  parameter Real c = 1;
  Real x(start = 0);
  Real y;
equation
  der(x) = 1;
  y = polyEval(a, b, c, x);
end FunctionTest;
