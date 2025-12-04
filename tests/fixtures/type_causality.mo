// Test type causality propagation
// Mimics MSL's Modelica.Blocks.Interfaces pattern
package Interfaces
  connector RealInput = input Real "Input connector";
  connector RealOutput = output Real "Output connector";

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
