model CartPole "Upright cart-pole with an external continuous-force policy"
  parameter Real cartMass = 1 "Cart mass [kg]";
  parameter Real poleMass = 0.1 "Pole mass [kg]";
  parameter Real halfLength = 0.5 "Pole centre-of-mass distance from pivot [m]";
  parameter Real gravity = 9.8 "Gravity [m/s^2]";
  parameter Real initialAngle = 0.03 "Initial angle from upright [rad]";
  input Real force = 0 "Horizontal force [N], bounded by the policy to +/-10";
  output Real x(start = 0, fixed = true) "Cart position [m]";
  output Real v(start = 0, fixed = true) "Cart velocity [m/s]";
  output Real theta(start = initialAngle, fixed = true) "Angle from upright [rad]";
  output Real omega(start = 0, fixed = true) "Pole angular velocity [rad/s]";
protected
  Real acceleration;
  Real angularAcceleration;
  Real drive;
equation
  drive = (force + poleMass*halfLength*omega^2*sin(theta))/(cartMass + poleMass);
  angularAcceleration = (gravity*sin(theta) - cos(theta)*drive)/
    (halfLength*(4.0/3.0 - poleMass*cos(theta)^2/(cartMass + poleMass)));
  acceleration = drive - poleMass*halfLength*angularAcceleration*cos(theta)/
    (cartMass + poleMass);
  der(x) = v;
  der(v) = acceleration;
  der(theta) = omega;
  der(omega) = angularAcceleration;
  annotation(experiment(StopTime = 10, Interval = 0.02, Solver = "rk-like"));
end CartPole;
