// Test case for issue #10: Model Constant Scope
// A constant defined in the main model should be accessible in an internal function
model NestedFunctionConstant
  constant Real k = 2.0; // Constant defined in the model
  // Uses constant 'k' from parent scope
  Real x(start = 1);
  Real y;
  function Scale
    input Real x;
    output Real y;
  algorithm
    y := k * x;
  end Scale;
equation
  der(x) = 1;
  y = Scale(x);
end NestedFunctionConstant;
