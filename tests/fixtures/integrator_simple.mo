// Simplified Integrator to debug balance issue
// Based on MSL Modelica.Blocks.Continuous.Integrator
package Interfaces
  connector RealInput = input Real;

  connector RealOutput = output Real;

  connector BooleanOutput = output Boolean;

  block SISO "Single input single output"
    RealInput u "Input";
    RealOutput y "Output";
  end SISO;
end Interfaces;

block SimpleIntegrator "Basic integrator without conditionals"
  extends Interfaces.SISO;
  parameter Real k = 1 "Gain";
equation
  der(y) = k * u;
end SimpleIntegrator;

block IntegratorWithProtected "Integrator with protected outputs"
  extends Interfaces.SISO;
  parameter Real k = 1 "Gain";
  parameter Boolean use_reset = false "Enable reset";
  Interfaces.BooleanOutput local_reset;
  Interfaces.RealOutput local_set;
equation
  // These should count as equations for the protected outputs
  local_reset = false;
  local_set = 0;
  der(y) = k * u;
end IntegratorWithProtected;

block IntegratorWithIf "Integrator with if-equations"
  extends Interfaces.SISO;
  parameter Real k = 1 "Gain";
  parameter Boolean use_reset = false "Enable reset";
  Interfaces.BooleanOutput local_reset;
  Interfaces.RealOutput local_set;
equation
  if use_reset then
    local_reset = true;
    local_set = 1.0;
  else
    local_reset = false;
    local_set = 0;
  end if;
  // Would be connected in real code
  der(y) = k * u;
end IntegratorWithIf;
