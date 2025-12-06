// Test constant propagation
block Inner
  constant Real c1 = 1.0 "A constant";
  parameter Real p1 = 2.0 "A parameter with default";
  parameter Real p2 "A parameter without default";
  Real x;
equation
  der(x) = c1 * p1 * p2;
end Inner;

model ConstantTest
  parameter Real gain = 3.0;
  Inner inner1(p2 = gain);
end ConstantTest;
