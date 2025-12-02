model Rover
  // parmeters
  parameter Real x0 = 0 "initial x position";
  parameter Real y0 = 0 "initial y position";
  parameter Real z0 = 0.25 "initial z position";
  parameter Real theta0 = 0 "initial orientation";
  parameter Real l = 0.7 "length of chassis";
  parameter Real w = 0.3 "width of chassis";
  parameter Real h = 0.1 "height of chassis";
  parameter Real r = 0.1 "radius of wheel";
  parameter Real ixx = 1 "moment of inertia of about body x axis";
  parameter Real iyy = 1 "moment of inertia of about body y axis";
  parameter Real izz = 1 "moment of inertia of about body z axis";
  parameter Real m = 1.0 "mass of chassis";
  parameter Real wheel_m = 0.1 "mass of wheel";
  parameter Real wheel_base = 0.5 "wheel base";
  parameter Real wheel_separation = 0.5 "wheel separation";
  parameter Real wheel_radius = 0.1 "radius of wheel";
  parameter Real wheel_width = 0.05 "width of wheel";
  parameter Real wheel_ixx = 0.1 "moment of inertia of wheel about x axis";
  parameter Real wheel_iyy = 0.1 "moment of inertia of wheel about y axis";
  parameter Real wheel_izz = 0.1 "moment of inertia of wheel about z axis";
  parameter Real wheel_max_turn_angle = 0.7854 "maximum steering angle of wheel";
  parameter Real mag_decl = 0 "world magnetic declination";
  parameter Real wheel_front_pos_x = 0.25 "front wheel x position";
  parameter Real wheel_rear_pos_x = 0.25 "rear wheel x position";
  parameter Real wheel_left_pos_y = 0.175 "left wheel y position";
  parameter Real wheel_right_pos_y = 0.175 "right wheel y position";
  parameter Real wheel_pos_z = .05 "wheel z position";
  // states
  Real x(start = x0);
  Real y(start = y0);
  Real theta(start = theta0);
  Real z(start = z0);
  Real v;
  // inputs
  input Real thr, str;
  // subsystems
  Motor m1;
equation
  v = r * m1.omega;
  der(x) = v * cos(theta);
  der(y) = v * sin(theta);
  der(z) = 0;
  der(theta) = (v / wheel_base) * tan(str);
  m1.omega_ref = thr;
end Rover;

model Motor
  parameter Real tau = 1.0;
  Real omega_ref;
  Real omega;
equation
  der(omega) = (1 / tau) * (omega_ref - omega);
end Motor;
