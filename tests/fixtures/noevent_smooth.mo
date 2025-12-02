// Test model for noEvent and smooth operators
model NoEventSmoothTest
  Real x(start = 0.0);
  Real y(start = 1.0);
  Real z;
equation
  // noEvent prevents zero-crossing event detection
  der(x) = noEvent(if x > 0 then 1.0 else -1.0);
  // smooth(p, expr) indicates expr is p times continuously differentiable
  der(y) = smooth(0, if y > 0.5 then y else 0.5);
  // noEvent can wrap any expression
  z = noEvent(abs(x - y));
end NoEventSmoothTest;
