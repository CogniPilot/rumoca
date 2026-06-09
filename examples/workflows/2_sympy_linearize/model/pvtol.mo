// =====================================================================
// PVTOL — Planar Vertical Take-Off and Landing aircraft
//
// A textbook 2-D quadrotor (Murray, "Optimization-Based Control"; also
// Schoellig et al.). Used here as the single Modelica source feeding
// §6.2's three downstream backends: CasADi (linear MPC about hover),
// SymPy (symbolic linearization + cross-backend certification), JAX
// (gradient-based parameter identification).
//
// State (6):  x, z, theta, x_dot, z_dot, theta_dot
//   - x: horizontal position [m]
//   - z: vertical position [m] (Z-up; positive = altitude)
//   - theta: pitch angle [rad] (about y-axis; 0 = body horizontal)
//
// Input (2):  T, M
//   - T: total thrust along the body z-axis [N]
//   - M: pitching moment about the body y-axis [N*m]
//
// Dynamics (rigid body in the (x, z) plane):
//   m * x_dot_dot     =  -T * sin(theta)
//   m * z_dot_dot     =   T * cos(theta) - m * g
//   J * theta_dot_dot =   M
//
// Hover trim:  theta = 0, T = m*g, M = 0.
// =====================================================================

model PVTOL

  parameter Real m = 2.496  "Mass [kg]";
  parameter Real J = 0.0344 "Moment of inertia about y-axis [kg*m^2]";
  parameter Real g = 9.80665 "Gravitational acceleration [m/s^2]";

  // States
  Real x(start = 0)         "Horizontal position [m]";
  Real z(start = 0)         "Vertical position (up) [m]";
  Real theta(start = 0)     "Pitch angle [rad]";
  Real x_dot(start = 0)     "Horizontal velocity [m/s]";
  Real z_dot(start = 0)     "Vertical velocity [m/s]";
  Real theta_dot(start = 0) "Pitch rate [rad/s]";

  // Inputs
  input Real T "Total thrust [N]";
  input Real M "Pitching moment [N*m]";

equation

  der(x)         = x_dot;
  der(z)         = z_dot;
  der(theta)     = theta_dot;
  m * der(x_dot)     = -T * sin(theta);
  m * der(z_dot)     =  T * cos(theta) - m * g;
  J * der(theta_dot) =  M;

end PVTOL;
