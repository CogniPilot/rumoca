// Test constant with annotation (like MSL's unitTime)
package SI
  type Time = Real(unit = "s");
end SI;

block Inner
  constant SI.Time unitTime = 1 annotation(HideResult = true);
  parameter Real k = unitTime * 2;
  Real x;
equation
  der(x) = k * unitTime;
end Inner;

model AnnotationConstantTest
  Inner inner1;
end AnnotationConstantTest;
