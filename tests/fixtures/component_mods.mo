// Test component modifications with expressions
// Mimics MSL PID pattern: D(k=Td/unitTime, T=max([...]))
block Inner
  parameter Real k = 1 "Gain";
  parameter Real T = 1 "Time constant";
  Real x;
equation
  der(x) = k * T;
end Inner;

model TestCompMods
  parameter Real gain = 2.0;
  parameter Real tau = 0.5;
  // Component modification with expressions
  Inner inner1(k = gain * tau, T = tau / 2);
end TestCompMods;
