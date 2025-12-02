// Test model for variable scoping with extends and type aliases
// Base class with components
class Base
  Real x(start = 1.0);
  parameter Real k = 2.0;
equation
  der(x) = -k * x;
end Base;

// Extended class that uses parent's variables
class Extended
  extends Base;
  Real y;
equation
  y = 2 * x;
end Extended;

// Should reference x from Base
// Main test model - nested component test
model ScopingTest
  Extended e1;
  Extended e2(k = 3.0);
  Real total;
equation
  total = e1.x + e2.x;
end ScopingTest;
// Should reference e1.x and e2.x
