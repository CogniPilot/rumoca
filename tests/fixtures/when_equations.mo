// Test fixtures for when equation balance checking
package WhenEquations
  // Simple when equation with single discrete variable
  // Should be: 1 state (x), 1 discrete (y), 2 equations (der(x), when y=...)
  model SimpleWhen
    input Real u;
    Real x(start = 0);
    discrete Real y(start = 0);
  equation
    der(x) = u;
    when x > 1 then
      y = pre(y) + 1;
    end when;
  end SimpleWhen;

  // When equation with multiple discrete variables
  // Should be: 1 state (x), 2 discrete (y_min, y_max), 3 equations
  model MultipleDiscreteWhen
    input Real u;
    Real x(start = 0);
    discrete Real y_min(start = 0);
    discrete Real y_max(start = 0);
  equation
    der(x) = u - x;
    when x < pre(y_min) then
      y_min = x;
    end when;
    when x > pre(y_max) then
      y_max = x;
    end when;
  end MultipleDiscreteWhen;

  // When equation with elsewhen
  // Should be: 1 discrete (y), 1 equation (when/elsewhen provides 1 eq for y)
  model WhenElsewhen
    input Real u;
    discrete Real y(start = 0);
  equation
    when u > 1 then
      y = 1;
    elsewhen u < -1 then
      y = -1;
    end when;
  end WhenElsewhen;

  // Sample-based when (like clocked systems)
  // Should be: 1 discrete (y), 1 equation
  model SampleWhen
    input Real u;
    discrete Real y(start = 0);
  equation
    when sample(0, 0.1) then
      y = u;
    end when;
  end SampleWhen;

  // Pattern from ContinuousSignalExtrema - when with multiple conditions
  // Should be: 1 state (x), 4 discrete (y_min, y_max, t_min, t_max), 5 equations
  model ExtremaLike
    input Real u;
    parameter Real T = 0.001;
    Real x(start = 0);
    discrete Real y_min(start = 0);
    discrete Real y_max(start = 0);
    discrete Real t_min(start = 0);
    discrete Real t_max(start = 0);
  equation
    der(x) = (u - x) / T;
    when {u <= x, u >= x} then
      y_min = min(pre(y_min), u);
      y_max = max(pre(y_max), u);
      t_min = if y_min < pre(y_min) then time else pre(t_min);
      t_max = if y_max > pre(y_max) then time else pre(t_max);
    end when;
  initial equation
    x = u;
    y_min = u;
    y_max = u;
  end ExtremaLike;
end WhenEquations;
