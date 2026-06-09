// =====================================================================
// Perch  —  longitudinal (2-D) fixed-wing UAV for the perching demo.
//
// A small UAV flying in the vertical (x, z) plane. The point of the model
// is the *post-stall* aerodynamics: a smooth blend from an attached-flow
// airfoil to a flat plate once the angle of attack passes the stall angle.
// That flat-plate regime is what makes a "perched landing" possible — the
// aircraft pitches its nose up past the stall and uses the huge broadside
// drag as an air brake to arrive at a point with almost no speed, the way
// a bird lands on a wire.
//
// State (6):  x, z, theta, vx, vz, q
//   x, z   : world position [m]          (z up)
//   theta  : pitch angle [rad]           (0 = nose level)
//   vx, vz : world velocity [m/s]
//   q      : pitch rate [rad/s]          (= der(theta))
//
// Input (2):  delta_e, thrust
//   delta_e : elevator deflection [rad]  (+ = nose up)
//   thrust  : propeller thrust [N]       (along the body x-axis)
//
// Sign / frame conventions are deliberately simple: everything is in the
// world frame, lift is perpendicular to the velocity vector and drag is
// anti-parallel to it, so the aerodynamics stay valid through a full
// nose-up pitch-over (no small-angle assumption anywhere).
// =====================================================================

model Perch

  // ---- mass / geometry ----
  parameter Real m    = 1.5    "Mass [kg]";
  parameter Real Iyy  = 0.08   "Pitch moment of inertia [kg*m^2]";
  parameter Real g    = 9.81   "Gravity [m/s^2]";
  parameter Real rho  = 1.225  "Air density [kg/m^3]";
  parameter Real S    = 0.25   "Wing reference area [m^2]";
  parameter Real cbar = 0.18   "Mean aerodynamic chord [m]";

  // ---- attached-flow (pre-stall) aerodynamic coefficients ----
  parameter Real CL0   = 0.30  "Lift coefficient at zero angle of attack";
  parameter Real CLa   = 5.0   "Lift-curve slope [1/rad]";
  parameter Real CD0   = 0.05  "Parasitic drag coefficient";
  parameter Real k_ind = 0.08  "Induced-drag factor";
  parameter Real Cm0   = 0.05  "Pitching moment at zero angle of attack";
  parameter Real Cma   = -0.7  "Pitch stiffness (static stability) [1/rad]";
  parameter Real Cmq   = -8.0  "Pitch-rate damping";
  parameter Real Cmde  = 2.6   "Elevator pitch effectiveness [1/rad]";

  // ---- post-stall (flat-plate) blend ----
  parameter Real CD_fp       = 2.0    "Flat-plate drag coefficient (broadside)";
  parameter Real alpha_stall = 0.262  "Stall angle [rad] (~15 deg)";
  parameter Real blend_width = 0.05   "Stall blend half-width [rad]";

  parameter Real eps = 1e-3 "Airspeed floor to keep expressions finite [m/s]";

  // ---- states ----
  Real x(start = 0)      "World x position [m]";
  Real z(start = 0)      "World z position (up) [m]";
  Real theta(start = 0)  "Pitch angle [rad]";
  Real vx(start = 13)    "World x velocity [m/s]";
  Real vz(start = 0)     "World z velocity [m/s]";
  Real q(start = 0)      "Pitch rate [rad/s]";

  // ---- inputs ----
  input Real delta_e "Elevator deflection [rad]";
  input Real thrust  "Thrust [N]";

  // ---- named aerodynamic quantities (functions of the state) ----
  Real V      "Airspeed [m/s]";
  Real alpha  "Angle of attack [rad]";
  Real qbar   "Dynamic pressure [Pa]";
  Real sigma  "Stall blend (0 = attached flow, 1 = flat plate)";
  Real CL     "Lift coefficient";
  Real CD     "Drag coefficient";
  Real Cm     "Pitching-moment coefficient";
  Real L      "Lift force [N]";
  Real D      "Drag force [N]";

equation
  // Airspeed and angle of attack. alpha is the pitch angle minus the
  // direction the aircraft is actually moving, so a hard nose-up pitch
  // while still translating forward drives alpha past the stall.
  V     = sqrt(vx*vx + vz*vz + eps*eps);
  alpha = theta - atan2(vz, vx);
  qbar  = 0.5 * rho * V * V;

  // Smooth stall blend: sigma goes 0 -> 1 across alpha_stall.
  sigma = 0.5 * (1 + tanh((alpha - alpha_stall) / blend_width));

  // Lift / drag coefficients blend attached-flow and flat-plate models.
  CL = (1 - sigma) * (CL0 + CLa * alpha)
       + sigma * sin(2 * alpha);
  CD = (1 - sigma) * (CD0 + k_ind * (CL0 + CLa * alpha)^2)
       + sigma * CD_fp * sin(alpha)^2;
  Cm = Cm0 + Cma * alpha + Cmq * (cbar / (2 * V)) * q + Cmde * delta_e;

  L = qbar * S * CL;
  D = qbar * S * CD;

  // Kinematics.
  der(x)     = vx;
  der(z)     = vz;
  der(theta) = q;

  // Translational dynamics in the world frame. Drag acts along -velocity,
  // lift perpendicular to it (rotate the unit velocity by +90 deg), thrust
  // along the body x-axis, gravity down.
  m * der(vx) = (-D * vx - L * vz) / V + thrust * cos(theta);
  m * der(vz) = (-D * vz + L * vx) / V + thrust * sin(theta) - m * g;

  // Pitch dynamics.
  Iyy * der(q) = qbar * S * cbar * Cm;

end Perch;
