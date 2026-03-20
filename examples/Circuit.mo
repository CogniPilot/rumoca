package Circuit

connector Pin
  Real v "voltage";
  flow Real i "current";
end Pin;

model TwoPin
  Pin p, n;
  Real v;
  Real i;
equation
  v = p.v - n.v;
  i = p.i;
  p.i + n.i = 0;
end TwoPin;

model Ground
  Pin p;
equation
  p.v = 0;
end Ground;

model VoltageSource
  extends TwoPin;
  parameter Real V = 5.0 "V";
equation
  v = V;
end VoltageSource;

model Resistor
  extends TwoPin;
  parameter Real R = 2.5 "Resistance";
equation
  v = R * i;
end Resistor;

model Capacitor
  extends TwoPin;
  parameter Real C = 1e-2 "Capacitance";
equation
  i = C * der(v);
end Capacitor;

model Test
  VoltageSource src;
  Ground gnd;
  Resistor res;
  Capacitor cap;
equation
  connect(src.p, res.p);
  connect(res.n, cap.p);
  connect(cap.n, src.n);
  connect(src.n, gnd.p);
end Test;

end Circuit;