// 6-DOF quadrotor SIL plant model (NED frame, FRD body).
//
// Inputs:  4 motor angular velocities [rad/s]
// Outputs: synthetic sensor readings (IMU accel/gyro, magnetometer)
//          + DCM elements for visualization
//
// Motor layout (X-config, matching cerebri MixQuadX):
//   1: front-right  CW   (+yaw torque)
//   2: rear-right   CCW  (-yaw torque)
//   3: rear-left    CW   (+yaw torque)
//   4: front-left   CCW  (-yaw torque)
//
// Conventions:
//   World frame: NED (North-East-Down)
//   Body frame:  FRD (Forward-Right-Down)
//   Quaternion:  {w, x, y, z} scalar-first, body-to-world
//   Thrust:      along body -Z (upward when level)

class RigidBodyQuatNED

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

  Real omega_x(start = 0) "Body roll rate p [rad/s]";
  Real omega_y(start = 0) "Body pitch rate q [rad/s]";
  Real omega_z(start = 0) "Body yaw rate r [rad/s]";

  Real a_bx "Body specific force X [m/s^2]";
  Real a_by "Body specific force Y [m/s^2]";
  Real a_bz "Body specific force Z [m/s^2]";

  // DCM elements (body-to-world) for visualization
  Real R11; Real R12; Real R13;
  Real R21; Real R22; Real R23;
  Real R31; Real R32; Real R33;

equation
  // Body-to-world DCM from quaternion
  R11 = 1 - 2*(q2*q2 + q3*q3);
  R12 = 2*(q1*q2 - q0*q3);
  R13 = 2*(q1*q3 + q0*q2);
  R21 = 2*(q1*q2 + q0*q3);
  R22 = 1 - 2*(q1*q1 + q3*q3);
  R23 = 2*(q2*q3 - q0*q1);
  R31 = 2*(q1*q3 - q0*q2);
  R32 = 2*(q2*q3 + q0*q1);
  R33 = 1 - 2*(q1*q1 + q2*q2);

  // Translational dynamics: a_world = R * a_body + [0, 0, g]
  der(px) = vx;
  der(py) = vy;
  der(pz) = vz;

  der(vx) = R11 * a_bx + R12 * a_by + R13 * a_bz;
  der(vy) = R21 * a_bx + R22 * a_by + R23 * a_bz;
  der(vz) = R31 * a_bx + R32 * a_by + R33 * a_bz + g;

  // Quaternion kinematics
  der(q0) = 0.5 * (-q1*omega_x - q2*omega_y - q3*omega_z);
  der(q1) = 0.5 * ( q0*omega_x - q3*omega_y + q2*omega_z);
  der(q2) = 0.5 * ( q3*omega_x + q0*omega_y - q1*omega_z);
  der(q3) = 0.5 * (-q2*omega_x + q1*omega_y + q0*omega_z);

end RigidBodyQuatNED;


model QuadrotorSIL

  extends RigidBodyQuatNED;

  // --- Physical parameters ---
  parameter Real mass = 2.0 "Total mass [kg]";
  parameter Real Ixx = 0.020 "Roll moment of inertia [kg*m^2]";
  parameter Real Iyy = 0.020 "Pitch moment of inertia [kg*m^2]";
  parameter Real Izz = 0.040 "Yaw moment of inertia [kg*m^2]";

  // --- Motor/propeller parameters ---
  parameter Real Ct = 8.5e-6 "Thrust coefficient [N/(rad/s)^2]";
  parameter Real Cm = 1.36e-7 "Torque coefficient [N*m/(rad/s)^2]";
  parameter Real arm_length = 0.2 "Arm length [m]";

  // --- Magnetic field (NED, typical mid-latitude) ---
  parameter Real mag_world_n = 0.21 "Mag field North [Gauss]";
  parameter Real mag_world_e = 0.0 "Mag field East [Gauss]";
  parameter Real mag_world_d = 0.45 "Mag field Down [Gauss]";

  // --- Motor angular velocity inputs [rad/s] ---
  // Start at hover: omega = sqrt(mass * g / (4 * Ct)) ~ 759.5 rad/s
  input Real omega_m1(start = 759.5) "Motor 1 (FR, CW) angular velocity";
  input Real omega_m2(start = 759.5) "Motor 2 (RR, CCW) angular velocity";
  input Real omega_m3(start = 759.5) "Motor 3 (RL, CW) angular velocity";
  input Real omega_m4(start = 759.5) "Motor 4 (FL, CCW) angular velocity";

  // --- Derived quantities ---
  Real F1 "Motor 1 thrust [N]";
  Real F2 "Motor 2 thrust [N]";
  Real F3 "Motor 3 thrust [N]";
  Real F4 "Motor 4 thrust [N]";
  Real T  "Total thrust [N]";
  Real Mx "Roll moment [N*m]";
  Real My "Pitch moment [N*m]";
  Real Mz "Yaw moment [N*m]";

  // --- Sensor outputs ---
  output Real accel_x "Body-frame accelerometer X [m/s^2]";
  output Real accel_y "Body-frame accelerometer Y [m/s^2]";
  output Real accel_z "Body-frame accelerometer Z [m/s^2]";
  output Real gyro_x "Body-frame gyroscope X [rad/s]";
  output Real gyro_y "Body-frame gyroscope Y [rad/s]";
  output Real gyro_z "Body-frame gyroscope Z [rad/s]";
  output Real mag_x "Body-frame magnetometer X [Gauss]";
  output Real mag_y "Body-frame magnetometer Y [Gauss]";
  output Real mag_z "Body-frame magnetometer Z [Gauss]";

protected
  // Arm moment arm (X-config: motors at 45 degrees)
  parameter Real d = arm_length * 0.7071067811865476 "Effective moment arm [m]";

equation
  // --- Motor thrusts ---
  F1 = Ct * omega_m1 * omega_m1;
  F2 = Ct * omega_m2 * omega_m2;
  F3 = Ct * omega_m3 * omega_m3;
  F4 = Ct * omega_m4 * omega_m4;

  // --- Total thrust and moments (X-config, matching MixQuadX) ---
  T  = F1 + F2 + F3 + F4;
  Mx = d * (-F1 - F2 + F3 + F4);
  My = d * ( F1 - F2 - F3 + F4);
  Mz = (Cm / Ct) * (F1 - F2 + F3 - F4);

  // --- Body specific force (thrust along body -Z) ---
  a_bx = 0;
  a_by = 0;
  a_bz = -T / mass;

  // --- Angular dynamics (Euler's equations, diagonal inertia) ---
  der(omega_x) = (Mx + (Iyy - Izz) * omega_y * omega_z) / Ixx;
  der(omega_y) = (My + (Izz - Ixx) * omega_x * omega_z) / Iyy;
  der(omega_z) = (Mz + (Ixx - Iyy) * omega_x * omega_y) / Izz;

  // --- Sensor: gyroscope (trivially the body angular velocity) ---
  gyro_x = omega_x;
  gyro_y = omega_y;
  gyro_z = omega_z;

  // --- Sensor: accelerometer (specific force in body frame) ---
  // Accelerometer reads: total non-gravitational acceleration in body frame
  // = [0, 0, -T/mass] for a quadrotor with only thrust along body -Z
  accel_x = 0;
  accel_y = 0;
  accel_z = -T / mass;

  // --- Sensor: magnetometer (world mag field rotated to body frame) ---
  // mag_body = R^T * mag_world (R is body-to-world, so R^T is world-to-body)
  mag_x = R11 * mag_world_n + R21 * mag_world_e + R31 * mag_world_d;
  mag_y = R12 * mag_world_n + R22 * mag_world_e + R32 * mag_world_d;
  mag_z = R13 * mag_world_n + R23 * mag_world_e + R33 * mag_world_d;

end QuadrotorSIL;
