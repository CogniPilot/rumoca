// Simple PID-like test model
// Mimics MSL PID structure with component modifications
block Gain
  parameter Real k = 1 "Gain";
  Real u;
  Real y;
equation
  y = k * u;
end Gain;

block Integrator
  parameter Real k = 1 "Gain";
  Real u;
  Real y(start = 0);
equation
  der(y) = k * u;
end Integrator;

block Derivative
  parameter Real k = 1 "Derivative gain";
  parameter Real T = 0.01 "Time constant";
  Real u;
  Real y;
  Real x(start = 0) "State variable";
equation
  der(x) = (u - x) / T;
  y = k * (u - x) / T;
end Derivative;

model SimplePID
  parameter Real Kp = 2.0 "Proportional gain";
  parameter Real Ti = 1.0 "Integral time";
  parameter Real Td = 0.5 "Derivative time";
  parameter Real Nd = 10 "Derivative filter coefficient";
  Real u "Control input";
  Real y "Control output";
  // Component modifications with expressions
  Gain P(k = Kp);
  Integrator I(k = Kp / Ti);
  Derivative D(k = Kp * Td, T = Td / Nd);
equation
  // Connect all components to same input
  P.u = u;
  I.u = u;
  D.u = u;
  // Sum outputs: PID = P + I + D
  y = P.y + I.y + D.y;
end SimplePID;
