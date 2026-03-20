model SwitchedRLC_MSL "An RLC circuit with a switched voltage source"
  import Modelica.Electrical.Analog.Basic;
  import Modelica.Electrical.Analog.Sources;

  parameter Modelica.Units.SI.Voltage Vb = 24 "Battery voltage";
  parameter Modelica.Units.SI.Inductance L = 1000;
  parameter Modelica.Units.SI.Resistance R = 100;
  parameter Modelica.Units.SI.Capacitance C = 1e-3;

  Sources.StepVoltage source(
    V = Vb,
    startTime = 0.5) "Voltage steps from 0 to Vb at t=0.5 s";

  Basic.Inductor inductor(L = L);
  Basic.Resistor resistor(R = R);
  Basic.Capacitor capacitor(C = C);
  Basic.Ground ground;
equation
  connect(source.p, inductor.p);
  connect(inductor.n, resistor.p);
  connect(inductor.n, capacitor.p);
  connect(resistor.n, ground.p);
  connect(capacitor.n, ground.p);
  connect(source.n, ground.p);
end SwitchedRLC_MSL;