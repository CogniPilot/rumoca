// Test cases for DeMultiplexer-style blocks where inputs appear on LHS
// Mimics MSL's Modelica.Blocks.Routing.DeMultiplex2 pattern
//
// The key issue is that equations like [u] = [y1; y2] have the INPUT u on the LHS.
// This should NOT cause u to become an algebraic variable - it should remain an input.
package DemultiplexBalance
  connector RealInput = input Real;

  connector RealOutput = output Real;

  // Simple demultiplexer - input on LHS (the problematic pattern)
  // u is input, y1 and y2 are outputs
  // Equation: [u] = [y1; y2] which means u[1]=y1, u[2]=y2
  // Expected: 2 equations, 2 unknowns (y1, y2), 2 inputs (u[1], u[2])
  block DeMultiplex2Like
    parameter Integer n1 = 1;
    parameter Integer n2 = 1;
    RealInput u[n1 + n2];
    RealOutput y1[n1];
    RealOutput y2[n2];
  equation
    [u] = [y1; y2];
  end DeMultiplex2Like;

  // Equivalent but written differently - should have same balance
  // y1[1] = u[1]; y2[1] = u[2];
  block DeMultiplex2Explicit
    parameter Integer n1 = 1;
    parameter Integer n2 = 1;
    RealInput u[n1 + n2];
    RealOutput y1[n1];
    RealOutput y2[n2];
  equation
    y1[1] = u[1];
    y2[1] = u[2];
  end DeMultiplex2Explicit;

  // Simpler scalar version for debugging
  // u is input scalar, y is output scalar
  // Equation: u = y (input on LHS)
  // Expected: 1 equation, 1 unknown (y), 1 input (u)
  block SimplePassthrough
    RealInput u;
    RealOutput y;
  equation
    u = y; // Input on LHS
  end SimplePassthrough;

  // Same but with y on LHS (normal case)
  block SimplePassthroughNormal
    RealInput u;
    RealOutput y;
  equation
    y = u; // Output on LHS (normal)
  end SimplePassthroughNormal;

  // Connect-style equation (like Show.RealValue)
  block ConnectStyleBlock
    parameter Boolean use_input = true;
    RealInput inPort if use_input;
    RealOutput outPort;
  protected
    RealInput localIn = if use_input then inPort else 0;
  equation
    outPort = localIn;
  end ConnectStyleBlock;
end DemultiplexBalance;
