// Test type causality propagation
// Mimics MSL's Modelica.Blocks.Interfaces pattern
package Interfaces
  connector RealInput = input Real;

  connector RealOutput = output Real;

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
