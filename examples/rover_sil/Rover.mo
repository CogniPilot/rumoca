// Simple differential-drive rover plant model.
//
// Inputs:
//   forward_cmd:  normalized forward command [-1..1]
//   turn_cmd:     normalized turn command    [-1..1]  (+ = turn right)
//
// States:
//   x, y:    world-frame position [m]
//   theta:   heading [rad]   (0 = +X axis; CCW positive)
//
// The model owns the differential-drive mixing so the config file can stay
// a clean 1:1 map from gamepad locals to model inputs.

class Rover

  parameter Real wheel_radius = 0.1 "Wheel radius [m]";
  parameter Real track = 0.3 "Distance between left/right wheels [m]";
  parameter Real max_wheel = 20 "Max wheel angular velocity [rad/s]";

  input Real forward_cmd(start = 0) "Normalized forward command [-1..1]";
  input Real turn_cmd(start = 0) "Normalized turn command [-1..1]";

  Real x(start = 0) "World X [m]";
  Real y(start = 0) "World Y [m]";
  Real theta(start = 0) "Heading [rad]";

  Real wheel_left  "Left wheel angular velocity [rad/s]";
  Real wheel_right "Right wheel angular velocity [rad/s]";
  Real v "Linear velocity [m/s]";
  Real omega "Yaw rate [rad/s]";

equation
  // Differential-drive mixing.
  wheel_left  = (forward_cmd - turn_cmd) * max_wheel;
  wheel_right = (forward_cmd + turn_cmd) * max_wheel;

  // Forward kinematics.
  v     = (wheel_left + wheel_right) * wheel_radius / 2;
  omega = (wheel_right - wheel_left) * wheel_radius / track;

  der(x)     = v * cos(theta);
  der(y)     = v * sin(theta);
  der(theta) = omega;

end Rover;
