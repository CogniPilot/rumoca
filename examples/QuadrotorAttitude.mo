// 6-DOF quadrotor with direct attitude PID control.
//
// Inputs (from stepper):
//   cmd_roll   — desired roll angle [rad]   (+ = tilt right)
//   cmd_pitch  — desired pitch angle [rad]  (+ = nose down in NED)
//   cmd_yaw    — desired yaw rate [rad/s]   (+ = clockwise)
//   cmd_thrust — desired total thrust [N]   (0 = freefall, ~19.6 = hover at 2kg)
//
// A PD attitude controller compares desired vs actual Euler angles
// and commands body moments. Thrust is applied directly.

class RigidBodyQuat

  parameter Real g = 9.80665 "Gravity [m/s^2]";

  Real px(start = 0) "Position North [m]";
  Real py(start = 0) "Position East [m]";
  Real pz(start = -1) "Position Down [m] (start 1m altitude)";

  Real vx(start = 0) "Velocity North [m/s]";
  Real vy(start = 0) "Velocity East [m/s]";
  Real vz(start = 0) "Velocity Down [m/s]";

  Real q0(start = 1) "Quaternion scalar (w)";
  Real q1(start = 0) "Quaternion vector x";
  Real q2(start = 0) "Quaternion vector y";
  Real q3(start = 0) "Quaternion vector z";

  Real omega_x "Body roll rate [rad/s]";
  Real omega_y "Body pitch rate [rad/s]";
  Real omega_z "Body yaw rate [rad/s]";

  Real a_bx "Body specific force X [m/s^2]";
  Real a_by "Body specific force Y [m/s^2]";
  Real a_bz "Body specific force Z [m/s^2]";

equation
  der(px) = vx;
  der(py) = vy;
  der(pz) = vz;

  der(vx) = (1 - 2*(q2*q2 + q3*q3)) * a_bx
          + 2*(q1*q2 - q0*q3)       * a_by
          + 2*(q1*q3 + q0*q2)       * a_bz;

  der(vy) = 2*(q1*q2 + q0*q3)       * a_bx
          + (1 - 2*(q1*q1 + q3*q3)) * a_by
          + 2*(q2*q3 - q0*q1)       * a_bz;

  der(vz) = 2*(q1*q3 - q0*q2)       * a_bx
          + 2*(q2*q3 + q0*q1)       * a_by
          + (1 - 2*(q1*q1 + q2*q2)) * a_bz
          + g;

  der(q0) = 0.5 * (-q1*omega_x - q2*omega_y - q3*omega_z);
  der(q1) = 0.5 * ( q0*omega_x - q3*omega_y + q2*omega_z);
  der(q2) = 0.5 * ( q3*omega_x + q0*omega_y - q1*omega_z);
  der(q3) = 0.5 * (-q2*omega_x + q1*omega_y + q0*omega_z);

end RigidBodyQuat;


model QuadrotorAttitude

  extends RigidBodyQuat;

  parameter Real mass = 2.0   "Total mass [kg]";
  parameter Real Ixx  = 0.020 "Roll  moment of inertia [kg*m^2]";
  parameter Real Iyy  = 0.020 "Pitch moment of inertia [kg*m^2]";
  parameter Real Izz  = 0.040 "Yaw   moment of inertia [kg*m^2]";

  // --- Attitude PD gains ---
  parameter Real Kp_roll  = 8.0  "Roll angle P gain";
  parameter Real Kd_roll  = 1.2  "Roll rate  D gain";
  parameter Real Kp_pitch = 8.0  "Pitch angle P gain";
  parameter Real Kd_pitch = 1.2  "Pitch rate  D gain";
  parameter Real Kp_yaw   = 4.0  "Yaw rate P gain";
  parameter Real Kd_yaw   = 0.8  "Yaw rate D gain";

  // --- Command inputs (set by stepper) ---
  input Real cmd_thrust "Desired thrust [N]";
  input Real cmd_roll   "Desired roll angle [rad]";
  input Real cmd_pitch  "Desired pitch angle [rad]";
  input Real cmd_yaw    "Desired yaw rate [rad/s]";

  // --- Internal variables ---
  Real T  "Total thrust [N]";
  Real Mx "Roll  moment [N*m]";
  Real My "Pitch moment [N*m]";
  Real Mz "Yaw   moment [N*m]";

  Real roll  "Current roll angle [rad]";
  Real pitch "Current pitch angle [rad]";
  Real yaw   "Current yaw angle [rad]";

equation
  // --- Plant: thrust along body -Z ---
  a_bx = 0;
  a_by = 0;
  a_bz = -T / mass;

  // --- Angular dynamics (Euler's equations) ---
  der(omega_x) = (Mx + (Iyy - Izz) * omega_y * omega_z) / Ixx;
  der(omega_y) = (My + (Izz - Ixx) * omega_x * omega_z) / Iyy;
  der(omega_z) = (Mz + (Ixx - Iyy) * omega_x * omega_y) / Izz;

  // --- Extract Euler angles from quaternion ---
  roll  = atan2(2*(q0*q1 + q2*q3), 1 - 2*(q1*q1 + q2*q2));
  pitch = asin(2*(q0*q2 - q3*q1));
  yaw   = atan2(2*(q0*q3 + q1*q2), 1 - 2*(q2*q2 + q3*q3));

  // --- Thrust: direct pass-through ---
  T = cmd_thrust;

  // --- Attitude PD controller ---
  // Roll/pitch: PD on angle error + rate damping
  Mx = Ixx * (Kp_roll  * (cmd_roll  - roll)  - Kd_roll  * omega_x);
  My = Iyy * (Kp_pitch * (cmd_pitch - pitch) - Kd_pitch * omega_y);
  // Yaw: P on rate error + rate damping
  Mz = Izz * (Kp_yaw   * (cmd_yaw - omega_z) - Kd_yaw * omega_z);

end QuadrotorAttitude;
