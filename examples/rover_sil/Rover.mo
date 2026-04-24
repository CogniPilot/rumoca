// Kinematic bicycle model of an Ackermann-steered rover.
//
// States (5):
//   x, y    — world position [m]
//   theta   — heading [rad] (0 = +X axis; CCW positive)
//   v       — longitudinal speed along body frame [m/s]
//   delta   — steering angle of front wheels [rad] (0 = straight)
//
// Inputs:
//   throttle : [-1..1]  throttle → target speed
//   steering    : [-1..1]  steering → target steering angle
//
// Both commands pass through first-order lags so the vehicle doesn't
// teleport its state on sudden stick moves, and so the front-wheel
// visualization has smooth angular motion to track.
//
// Kinematics at the rear axle (standard bicycle model):
//   der(x)     = v * cos(theta)
//   der(y)     = v * sin(theta)
//   der(theta) = v * tan(delta) / wheelbase
//
// The rover cannot sideslip — it follows the tangent of the front wheels
// by construction, so there's no "skidding" failure mode of the old
// differential-drive form.

model Rover

  // --- Geometry ---
  // Sized to match the GLB jeep used by rover_scene.js at its viewer scale.
  parameter Real wheelbase     = 0.62  "Axle-to-axle distance [m]";
  parameter Real track         = 0.40  "Distance between left/right wheels [m]";
  parameter Real wheel_radius  = 0.09  "Wheel radius [m]";

  // --- Command mapping ---
  parameter Real v_max      = 4.0   "Top forward speed [m/s] at full stick";
  parameter Real delta_max  = 0.6   "Max steering angle [rad] (~34°)";

  // --- First-order response time constants ---
  parameter Real tau_speed  = 0.25 "Speed tracking time constant [s]";
  parameter Real tau_steer  = 0.10 "Steering tracking time constant [s]";

  // --- Inputs ---
  input Real throttle(start = 0) "Throttle stick [-1..1]";
  input Real steering(start = 0)    "Steering stick [-1..1]";

  // --- States ---
  Real x(start = 0)     "World X [m]";
  Real y(start = 0)     "World Y [m]";
  Real theta(start = 0) "Heading [rad]";
  Real v(start = 0)     "Longitudinal speed [m/s]";
  Real delta(start = 0) "Steering angle [rad]";

  // --- Exposed scene-visualization helpers ---
  // The JS scene reads these to spin wheels and yaw the front pair.
  output Real wheel_rpm       "Rear-wheel angular speed [rad/s]";
  output Real front_wheel_yaw "Steering angle [rad] (same as delta)";
  output Real yaw_rate        "d(theta)/dt [rad/s]";

equation
  // First-order commands: state tracks setpoint with tau_*.
  der(v)     = (throttle * v_max - v)            / tau_speed;
  der(delta) = (steering    * delta_max - delta)    / tau_steer;

  // Kinematic bicycle.
  der(x)     = v * cos(theta);
  der(y)     = v * sin(theta);
  der(theta) = v * tan(delta) / wheelbase;

  // Derived outputs for the viewer.
  wheel_rpm       = v / wheel_radius;
  front_wheel_yaw = delta;
  yaw_rate        = v * tan(delta) / wheelbase;

end Rover;
