// Test fixtures for conditional component handling
// Tests that conditional components are properly filtered based on parameter defaults
package ConditionalComponents
  // Interfaces for testing
  connector RealInput = input Real;

  connector RealOutput = output Real;

  connector BooleanInput = input Boolean;

  connector BooleanOutput = output Boolean;

  // Simple model WITHOUT conditional components - should balance as 1 eq, 1 unk
  model SimpleNoConditional
    RealInput u;
    RealOutput y;
  equation
    y = 2 * u;
  end SimpleNoConditional;

  // Model WITH conditional component that defaults to FALSE
  // The conditional input should NOT be counted
  // Should balance as 1 eq, 1 unk (reset is excluded)
  model ConditionalInputFalse
    RealInput u;
    RealOutput y;
    parameter Boolean use_reset = false "Enable reset functionality";
    BooleanInput reset "Reset input (conditional)";
  equation
    y = 2 * u;
  end ConditionalInputFalse;

  // Model WITH conditional component that defaults to TRUE
  // The conditional input SHOULD be counted
  // Should be unbalanced: 1 eq, 2 unk (reset is included)
  model ConditionalInputTrue
    RealInput u;
    RealOutput y;
    parameter Boolean use_reset = true "Enable reset functionality";
    BooleanInput reset "Reset input (conditional)";
  equation
    y = 2 * u;
  end ConditionalInputTrue;

  // Model with multiple conditional components
  // Both conditional components default to false, so neither should count
  // Should balance as 1 eq, 1 unk
  model MultipleConditionalsFalse
    RealInput u;
    RealOutput y;
    parameter Boolean use_reset = false;
    parameter Boolean use_set = false;
    BooleanInput reset;
    RealInput set;
  equation
    y = 2 * u;
  end MultipleConditionalsFalse;

  // Model with AND condition: `if use_reset and use_set`
  // Both default to false, so component should NOT be counted
  model ConditionalWithAnd
    RealInput u;
    RealOutput y;
    parameter Boolean use_reset = false;
    parameter Boolean use_set = false;
    RealInput special;
  equation
    y = 2 * u;
  end ConditionalWithAnd;
end ConditionalComponents;
