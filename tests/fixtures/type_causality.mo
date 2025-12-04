// Test type causality propagation
// Mimics MSL's Modelica.Blocks.Interfaces pattern
package Interfaces
  connector RealInput
    extends Real;
  end RealInput;

  connector RealOutput
    extends Real;
  end RealOutput;

  block SISO "Single input single output"
    RealInput u "Input";
    RealOutput y "Output";
  end SISO;
end Interfaces;

block Der "Derivative block"
  extends Interfaces.SISO;
equation
  y = der(u);
end Der;
