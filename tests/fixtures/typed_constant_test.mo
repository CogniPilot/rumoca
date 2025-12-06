// Test typed constant propagation (like MSL's SI.Time)
package SI
  type Time = Real(unit = "s");
end SI;

block Inner
  constant SI.Time c1 = 1.0 "A typed constant";
  parameter SI.Time p1 = 2.0 "A typed parameter";
  Real x;
equation
  der(x) = c1 + p1;
end Inner;

model TypedConstantTest
  Inner inner1;
end TypedConstantTest;
