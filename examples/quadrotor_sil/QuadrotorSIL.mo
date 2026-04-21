// 6-DOF quadrotor SIL plant model (NED frame, FRD body).
//
// Inputs:  4 motor angular velocities [rad/s]
// States:  position, velocity, quaternion, angular velocity (13 total)
//
// All intermediate computations are inlined into the derivative
// equations to avoid algebraic variables (pure ODE, no DAE).
//
// Motor layout (X-config, matching cerebri MixQuadX):
//   1: front-right  CW   2: rear-right  CCW
//   3: rear-left    CW   4: front-left  CCW

model QuadrotorSIL

  // --- Physical parameters ---
  parameter Real mass = 2.0 "Total mass [kg]";
  parameter Real g = 9.8 "Gravity [m/s^2]";
  parameter Real Ixx = 0.02166666666666667 "Roll moment of inertia [kg*m^2]";
  parameter Real Iyy = 0.02166666666666667 "Pitch moment of inertia [kg*m^2]";
  parameter Real Izz = 0.04000000000000001 "Yaw moment of inertia [kg*m^2]";
  parameter Real Ct = 8.54858e-6 "Thrust coefficient [N/(rad/s)^2]";
  parameter Real Cm = 0.016 "Torque coefficient [N*m/(rad/s)^2]";
  parameter Real arm_length = 0.25 "Arm length [m]";
  parameter Real d = arm_length * 0.7071067811865476 "Effective moment arm [m]"; // X config, matches cyecca
    // Additional aerodynamic parameters from cyecca (not used in original equations, but included for completeness)
    parameter Real CD0 = 0 "Zero-lift drag coefficient";
    parameter Real S = 0.1 "Reference area [m^2]";
    parameter Real rho = 1.225 "Air density [kg/m^3]";
    parameter Real Jxz = 0.0 "Product of inertia [kg*m^2]";
  parameter Real mag_world_n = 0.21 "Mag field North [Gauss]";
  parameter Real mag_world_e = 0.0 "Mag field East [Gauss]";
  parameter Real mag_world_d = 0.45 "Mag field Down [Gauss]";

  // --- Ground contact (spring-damper) ---
  parameter Real ground_k = 1000 "Ground stiffness [N/m]";
  parameter Real ground_c = 100 "Ground damping [N*s/m]";
  parameter Real ground_eps = 0.02 "Ground contact smoothing [m]";

  // --- Quaternion normalization feedback gain ---
  parameter Real qnorm_gain = 1.0 "Quaternion renormalization gain";

  // --- Motor inputs [rad/s] (start at zero on the ground) ---
  input Real omega_m1(start = 0) "Motor 1 (FR, CW)";
  input Real omega_m2(start = 0) "Motor 2 (RR, CCW)";
  input Real omega_m3(start = 0) "Motor 3 (RL, CW)";
  input Real omega_m4(start = 0) "Motor 4 (FL, CCW)";

  // --- States ---
  Real px(start = 0) "Position North [m]";
  Real py(start = 0) "Position East [m]";
  Real pz(start = 0) "Position Down [m] (start on ground)";
  Real vx(start = 0) "Velocity North [m/s]";
  Real vy(start = 0) "Velocity East [m/s]";
  Real vz(start = 0) "Velocity Down [m/s]";
  Real q0(start = 1) "Quaternion w";
  Real q1(start = 0) "Quaternion x";
  Real q2(start = 0) "Quaternion y";
  Real q3(start = 0) "Quaternion z";
  Real omega_x(start = 0) "Body roll rate [rad/s]";
  Real omega_y(start = 0) "Body pitch rate [rad/s]";
  Real omega_z(start = 0) "Body yaw rate [rad/s]";

  // --- Outputs (sensor readings, computed from state) ---
  // These are algebraic but trivially eliminable
  output Real accel_x "Body accelerometer X [m/s^2] (specific force)";
  output Real accel_y "Body accelerometer Y [m/s^2] (specific force)";
  output Real accel_z "Body accelerometer Z [m/s^2] (specific force)";
  output Real gyro_x "Body gyroscope X [rad/s]";
  output Real gyro_y "Body gyroscope Y [rad/s]";
  output Real gyro_z "Body gyroscope Z [rad/s]";
  output Real mag_x "Body magnetometer X [Gauss]";
  output Real mag_y "Body magnetometer Y [Gauss]";
  output Real mag_z "Body magnetometer Z [Gauss]";

  // DCM elements for visualization
  Real R11; Real R12; Real R13;
  Real R21; Real R22; Real R23;
  Real R31; Real R32; Real R33;

protected
  // Motor thrusts and totals (used in equations only)
  Real F1; Real F2; Real F3; Real F4;
  Real T; Real Mx; Real My; Real Mz;
  Real a_bz "Body Z acceleration (thrust/mass)";
  // Ground contact: force in NED Down direction (negative = pushes up)
  Real F_ground "Ground normal force [N] (in NED Down, negative=up)";
  // World-frame total acceleration (NED)
  Real a_wx; Real a_wy; Real a_wz;

equation
  // Motor thrusts
  F1 = Ct * omega_m1 * omega_m1;
  F2 = Ct * omega_m2 * omega_m2;
  F3 = Ct * omega_m3 * omega_m3;
  F4 = Ct * omega_m4 * omega_m4;
  T = F1 + F2 + F3 + F4;
  Mx = d * (-F1 - F2 + F3 + F4);
  My = d * ( F1 - F2 - F3 + F4);
  Mz = Cm * (F1 - F2 + F3 - F4);
  a_bz = -T / mass;

  // DCM from quaternion (body-to-world)
  R11 = 1 - 2*(q2*q2 + q3*q3);
  R12 = 2*(q1*q2 - q0*q3);
  R13 = 2*(q1*q3 + q0*q2);
  R21 = 2*(q1*q2 + q0*q3);
  R22 = 1 - 2*(q1*q1 + q3*q3);
  R23 = 2*(q2*q3 - q0*q1);
  R31 = 2*(q1*q3 - q0*q2);
  R32 = 2*(q2*q3 + q0*q1);
  R33 = 1 - 2*(q1*q1 + q2*q2);

  // --- Ground contact (NED: pz > 0 means below ground) ---
  F_ground = -ground_k * (pz + sqrt(pz*pz + ground_eps*ground_eps)) / 2
             - ground_c * vz * (1 + pz / sqrt(pz*pz + ground_eps*ground_eps)) / 2;

  // --- Translational dynamics (NED: gravity along +z) ---
  der(px) = vx;
  der(py) = vy;
  der(pz) = vz;
  // a_world = R * [0, 0, a_bz] + [0, 0, g + F_ground/mass]
  der(vx) = R13 * a_bz;
  der(vy) = R23 * a_bz;
  der(vz) = R33 * a_bz + g + F_ground / mass;
  // World-frame total acceleration (NED). Expanded inline rather than
  // aliased from der(vx/y/z) because the BLT output-elimination pass
  // substitutes these via the runtime env reconstructor, which does not
  // resolve der() references — it only sees state variable values. So
  // `a_wx = der(vx)` would leave accel_x stuck at its init value when
  // the output is eliminated. Writing the RHS directly keeps the
  // eliminated substitution expression composed of env-resolvable terms.
  a_wx = R13 * a_bz;
  a_wy = R23 * a_bz;
  a_wz = R33 * a_bz + g + F_ground / mass;

  // --- Quaternion kinematics (with Baumgarte stabilization) ---
  // The correction term -lambda*(|q|^2-1)*q drives the quaternion
  // back toward unit norm, preventing drift that crashes the solver.
  der(q0) = 0.5 * (-q1*omega_x - q2*omega_y - q3*omega_z) - qnorm_gain * (q0*q0+q1*q1+q2*q2+q3*q3-1)*q0;
  der(q1) = 0.5 * ( q0*omega_x - q3*omega_y + q2*omega_z) - qnorm_gain * (q0*q0+q1*q1+q2*q2+q3*q3-1)*q1;
  der(q2) = 0.5 * ( q3*omega_x + q0*omega_y - q1*omega_z) - qnorm_gain * (q0*q0+q1*q1+q2*q2+q3*q3-1)*q2;
  der(q3) = 0.5 * (-q2*omega_x + q1*omega_y + q0*omega_z) - qnorm_gain * (q0*q0+q1*q1+q2*q2+q3*q3-1)*q3;

  // --- Angular dynamics (Euler's equations) ---
  der(omega_x) = (Mx + (Iyy - Izz) * omega_y * omega_z) / Ixx;
  der(omega_y) = (My + (Izz - Ixx) * omega_x * omega_z) / Iyy;
  der(omega_z) = (Mz + (Ixx - Iyy) * omega_x * omega_y) / Izz;

  // --- Sensor outputs ---
  gyro_x = omega_x;
  gyro_y = omega_y;
  gyro_z = omega_z;
  // Accelerometer: specific force in body frame = R^T * (a_world - [0,0,g])
  // In NED, gravity is [0, 0, g] (positive Down)
  accel_x = R11 * a_wx + R21 * a_wy + R31 * (a_wz - g);
  accel_y = R12 * a_wx + R22 * a_wy + R32 * (a_wz - g);
  accel_z = R13 * a_wx + R23 * a_wy + R33 * (a_wz - g);
  // mag_body = R^T * mag_world
  mag_x = R11 * mag_world_n + R21 * mag_world_e + R31 * mag_world_d;
  mag_y = R12 * mag_world_n + R22 * mag_world_e + R32 * mag_world_d;
  mag_z = R13 * mag_world_n + R23 * mag_world_e + R33 * mag_world_d;

end QuadrotorSIL;
