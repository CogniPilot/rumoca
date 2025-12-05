// Test fixture for binding equation balance checking
package BindingTest
  // Simple model with binding equation in protected section
  // Should be balanced: 1 state (y from der(y)), 1 algebraic (val)
  // 2 equations: val binding equation + der(y) equation
  model SlewLike
    input Real u;
    output Real y(start = 0);
    Real val = (u - y) / 0.001; // binding equation
  equation
    der(y) = val;
  end SlewLike;

  // Simpler test: just binding equation
  // Should be balanced: 1 algebraic (val), 1 equation (binding)
  model BindingOnly
    input Real u;
    output Real val = 2 * u; // output with binding
  end BindingOnly;

  // Test with explicit equation instead of binding
  // Uses (start=0) modification instead of = 0 binding
  model ExplicitEquation
    input Real u;
    output Real y(start = 0);
    Real val(start = 0); // explicit start modification (not binding)
  equation
    val = (u - y) / 0.001;
    // explicit equation
    der(y) = val;
  end ExplicitEquation;
end BindingTest;
