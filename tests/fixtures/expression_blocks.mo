// Test fixtures for expression block balance checking
// These blocks have output connectors with binding expressions but no equation section
package ExpressionBlocks
  // Simplified connector definitions
  connector RealOutput = output Real;

  connector IntegerOutput = output Integer;

  connector BooleanOutput = output Boolean;

  // RealExpression-style block: output with binding, no equation section
  // Should be balanced: 1 equation (y = 0.0), 1 unknown (y)
  block RealExpressionLike
    RealOutput y = 0.0 "Value of Real output";
  end RealExpressionLike;

  // IntegerExpression-style block
  // Should be balanced: 1 equation (y = 0), 1 unknown (y)
  block IntegerExpressionLike
    IntegerOutput y = 0 "Value of Integer output";
  end IntegerExpressionLike;

  // BooleanExpression-style block
  // Should be balanced: 1 equation (y = false), 1 unknown (y)
  block BooleanExpressionLike
    BooleanOutput y = false "Value of Boolean output";
  end BooleanExpressionLike;

  // Expression block with non-default value
  // Should be balanced: 1 equation (y = 1.5), 1 unknown (y)
  block RealExpressionNonDefault
    RealOutput y = 1.5 "Non-default value";
  end RealExpressionNonDefault;

  // Output with explicit equation - should NOT double-count
  // Should be balanced: 1 equation (y = 1.0 from equation section), 1 unknown (y)
  block OutputWithEquation
    RealOutput y = 0.0;
  equation
    y = 1.0;
  end OutputWithEquation;

  // Output without binding - should be under-determined (partial)
  // 0 equations, 1 unknown (y), 1 external connector
  block OutputNoBind
    RealOutput y;
  end OutputNoBind;
end ExpressionBlocks;
