// Strapdown INS process model — the EKF *prediction* model for the fixed-wing.
//
// This is the model the SymForce backend consumes: a self-contained, IMU-driven
// rigid-body navigation kinematics with a body->world quaternion attitude. It is
// deliberately separate from the closed-loop SIL plant (`fixedwing.mo`): an
// estimator predicts the open-loop kinematics driven by measured IMU, it does
// not contain the controller, aero or landing-gear.
//
//   World frame : Z-up        (gravity acts along -Z)
//   Body frame  : FLU (x forward / nose, y left, z up)
//   Quaternion  : {w, x, y, z} scalar-first, body-to-world
//
// State  (10) : world position p[3], world velocity v[3], quaternion q[4]
// Input  ( 6) : body angular rate omega[3] [rad/s], body specific force f_b[3] [m/s^2]
//               (i.e. the raw gyro + accelerometer triad of a strapdown IMU)
//
// Dynamics:
//   der(p) = v
//   der(v) = R(q) * f_b + g_world          g_world = {0, 0, -g}
//   der(q) = 0.5 * q (x) [0, omega]        quaternion kinematics
//
// The quaternion makes this the natural target for SymForce: downstream the
// 4-component q is wrapped in sf.Rot3 and the EKF linearizes in the 3-DOF
// tangent space, so the attitude covariance is full-rank and the estimate
// stays on the unit sphere by construction.
model FixedWingINS
  parameter Real g = 9.81 "Gravity [m/s^2]";

  // strapdown IMU triad (the EKF prediction inputs)
  input Real wx "Body roll rate gyro [rad/s]";
  input Real wy "Body pitch rate gyro [rad/s]";
  input Real wz "Body yaw rate gyro [rad/s]";
  input Real fx "Body specific force x (accel) [m/s^2]";
  input Real fy "Body specific force y (accel) [m/s^2]";
  input Real fz "Body specific force z (accel) [m/s^2]";

  // navigation state
  Real px(start = 0) "World position North [m]";
  Real py(start = 0) "World position East [m]";
  Real pz(start = 100) "World position Up [m]";
  Real vx(start = 18) "World velocity x [m/s]";
  Real vy(start = 0) "World velocity y [m/s]";
  Real vz(start = 0) "World velocity z [m/s]";
  Real qw(start = 1) "Quaternion w (body->world)";
  Real qx(start = 0) "Quaternion x";
  Real qy(start = 0) "Quaternion y";
  Real qz(start = 0) "Quaternion z";

  // world-frame specific force (diagnostic output)
  output Real ax "World acceleration x [m/s^2]";
  output Real ay "World acceleration y [m/s^2]";
  output Real az "World acceleration z [m/s^2]";

protected
  Real R11, R12, R13, R21, R22, R23, R31, R32, R33 "Body->world rotation";

equation
  // body->world rotation matrix from the (scalar-first) quaternion
  R11 = 1 - 2*(qy*qy + qz*qz);
  R12 = 2*(qx*qy - qw*qz);
  R13 = 2*(qx*qz + qw*qy);
  R21 = 2*(qx*qy + qw*qz);
  R22 = 1 - 2*(qx*qx + qz*qz);
  R23 = 2*(qy*qz - qw*qx);
  R31 = 2*(qx*qz - qw*qy);
  R32 = 2*(qy*qz + qw*qx);
  R33 = 1 - 2*(qx*qx + qy*qy);

  // specific force rotated into the world frame, plus gravity
  ax = R11*fx + R12*fy + R13*fz;
  ay = R21*fx + R22*fy + R23*fz;
  az = R31*fx + R32*fy + R33*fz - g;

  // translational kinematics
  der(px) = vx;
  der(py) = vy;
  der(pz) = vz;
  der(vx) = ax;
  der(vy) = ay;
  der(vz) = az;

  // quaternion kinematics:  qdot = 0.5 * q (x) [0, omega]
  der(qw) = 0.5*(-qx*wx - qy*wy - qz*wz);
  der(qx) = 0.5*( qw*wx + qy*wz - qz*wy);
  der(qy) = 0.5*( qw*wy - qx*wz + qz*wx);
  der(qz) = 0.5*( qw*wz + qx*wy - qy*wx);
end FixedWingINS;
