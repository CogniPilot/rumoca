// Test conditional input components and causality propagation
// Mimics MSL's Modelica.Blocks.Interaction.Show.RealValue pattern
package ConditionalInput
  connector RealInput = input Real;

  connector RealOutput = output Real;

  // Simple case: conditional input with type alias
  block ConditionalInputBlock
    parameter Boolean use_numberPort = true;
    RealInput numberPort if use_numberPort "Conditional input";
    RealOutput showNumber;
  equation
    if use_numberPort then
      showNumber = numberPort;
    else
      showNumber = 0;
    end if;
  end ConditionalInputBlock;

  // Same as above but with use_numberPort=false
  block ConditionalInputBlockFalse
    parameter Boolean use_numberPort = false;
    RealInput numberPort if use_numberPort "Conditional input";
    RealOutput showNumber;
  equation
    if use_numberPort then
      showNumber = numberPort;
    else
      showNumber = 0;
    end if;
  end ConditionalInputBlockFalse;
end ConditionalInput;
