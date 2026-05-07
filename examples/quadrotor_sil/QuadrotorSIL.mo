// 6-DOF quadrotor SIL plant model.
//
// Inputs:  4 motor angular velocities [rad/s]
// States:  rigid body pose/velocity plus motor speeds
//
// Internal frame matches the cyecca model: local world Z is Up and body axes
// are Forward-Left-Up. The FastDyn FMU wrapper exposes NED/FRD values.
//
// Motor layout (ArduPilot Quad-X output order):
//   1: front-right  CCW   2: rear-left   CCW
//   3: front-left   CW    4: rear-right  CW

model QuadrotorSIL
  extends RigidBody.RigidBody6DOF(
    mass = 2.0,
    g = 9.8,
    ixx = 0.02166666666666667,
    iyy = 0.02166666666666667,
    izz = 0.04000000000000001,
    p_start = {0, 0, ground_z - leg_z + initial_ground_clearance},
    qnorm_gain = 1.0
  );

  // Aerodynamic and actuator parameters
  parameter Real Ct = 8.54858e-6 "Thrust coefficient [N/(rad/s)^2]";
  parameter Real Cm = 0.016 "Rotor torque/thrust ratio [m]";
  parameter Real arm_length = 0.25 "Arm length [m]";
  parameter Real d = arm_length * 0.7071067811865476 "Effective moment arm [m]";
  parameter Real Cl_p = -0.2 "Rolling moment coefficient per roll rate";
  parameter Real Cm_q = -0.2 "Pitching moment coefficient per pitch rate";
  parameter Real Cn_r = -0.1 "Yawing moment coefficient per yaw rate";
  parameter Real S = 0.1 "Reference area [m^2]";
  parameter Real CdA[3] = {0.06, 0.08, 0.12} "Body-axis drag area [m^2]";
  parameter Real linear_drag[3] = {0.12, 0.12, 0.18} "Low-speed body-axis drag [N/(m/s)]";
  parameter Real rho = 1.225 "Air density [kg/m^3]";
  parameter Real mag_world_ned[3] = {0.21, 0.0, 0.45} "Mag field NED [Gauss]";
  parameter Real R_FRD_FLU[3, 3] = [
    1, 0, 0;
    0, -1, 0;
    0, 0, -1
  ] "Body FLU to body FRD transform";
  parameter Real R_NWU_NED[3, 3] = [
    1, 0, 0;
    0, -1, 0;
    0, 0, -1
  ] "World NED to internal N/W/U transform";

  // Ground contact (spring-damper)
  parameter Real ground_k = 3000 "Ground stiffness per contact point [N/m]";
  parameter Real ground_c = 150 "Ground normal damping per contact point [N*s/m]";
  parameter Real ground_tangent_c = 25 "Ground tangential damping per contact point [N*s/m]";
  parameter Real ground_z = 0.0 "World Z coordinate of the ground collision plane [m]";
  parameter Real initial_ground_clearance = 0.02 "Initial landing-leg clearance above the ground plane [m]";
  parameter Real leg_x = 0.17 "Landing contact X offset from CG [m]";
  parameter Real leg_y = 0.17 "Landing contact Y offset from CG [m]";
  parameter Real leg_z = -0.10 "Landing contact Z offset from CG [m]";

  // Motor first-order response
  parameter Real tau_up = 0.0125 "Motor spin-up time constant [s]";
  parameter Real tau_down = 0.025 "Motor spin-down time constant [s]";
  parameter Real motor_tau_eps = 1.0 "Smooth transition width for asymmetric motor lag [rad/s]";
  parameter Real motor_moment_map[3, 4] = [
    -d,   d,   d,  -d;
    -d,   d,  -d,   d;
   -Cm, -Cm,  Cm,  Cm
  ] "Motor thrust to body moment map";
  parameter Real rate_damping[3] = {
    4 * S * arm_length * Cl_p,
    4 * S * arm_length * Cm_q,
    4 * S * arm_length * Cn_r
  } "Body rate damping coefficients";

  model Motor
    parameter Real Ct = 8.54858e-6 "Thrust coefficient [N/(rad/s)^2]";
    parameter Real tau_up = 0.0125 "Motor spin-up time constant [s]";
    parameter Real tau_down = 0.025 "Motor spin-down time constant [s]";
    parameter Real tau_inv_mid = 0.5 * (1.0 / tau_up + 1.0 / tau_down) "Mean inverse motor lag [1/s]";
    parameter Real tau_inv_delta = 0.5 * (1.0 / tau_up - 1.0 / tau_down) "Signed inverse motor lag half-range [1/s]";
    parameter Real tau_eps = 1.0 "Smooth transition width for asymmetric lag [rad/s]";

    input Real omega_cmd(start = 0) "Commanded speed [rad/s]";
    output Real omega(start = 0, fixed = true) "Actual speed [rad/s]";
    output Real thrust "Motor thrust [N]";

  protected
    Real omega_error "Motor speed tracking error [rad/s]";
    Real lag_blend "Smooth lag blend";
    Real tau_inv "Smooth inverse lag [1/s]";

  equation
    omega_error = omega_cmd - omega;
    lag_blend = omega_error / sqrt(omega_error * omega_error + tau_eps * tau_eps);
    tau_inv = tau_inv_mid + tau_inv_delta * lag_blend;
    der(omega) = tau_inv * omega_error;
    thrust = Ct * omega * omega;
  end Motor;

  input Real omega_cmd[4](start = {0, 0, 0, 0}) "Motor commands [rad/s]";

  output Real position[3](start = p_start) "World position [m]";
  output Real velocity[3](start = v_b_start) "World velocity [m/s]";
  output Real quat[4](start = q_start) "Quaternion w,x,y,z";
  output Real omega_m[4](start = {0, 0, 0, 0}) "Motor actual speeds [rad/s]";
  output Real accel[3](start = {0, 0, 0}) "Body FRD accelerometer [m/s^2] (specific force)";
  output Real gyro[3](start = {0, 0, 0}) "Body FRD gyroscope [rad/s]";
  output Real mag[3](start = {0.21, 0, -0.45}) "Body FRD magnetometer [Gauss]";

protected
  Motor motor[4](each Ct = Ct, each tau_up = tau_up, each tau_down = tau_down, each tau_eps = motor_tau_eps);
  Real F_m[4] "Motor thrusts [N]";
  Real T "Total motor thrust [N]";
  Real M_rotor[3] "Rotor moment in body FLU [N*m]";
  Real M_rate[3] "Rate damping moment in body FLU [N*m]";
  Real V "Airspeed magnitude [m/s]";
  Real drag_b[3] "Body drag force [N]";
  Real mag_world_w[3] "Mag field in internal N/W/U world axes [Gauss]";
  Real mag_b_flu[3] "Mag field in body FLU axes [Gauss]";
  Real leg_h_w[4] "Landing contact world Z positions [m]";
  parameter Real leg_r_b[3, 4] = [
    leg_x, -leg_x, leg_x, -leg_x;
    -leg_y, leg_y, leg_y, -leg_y;
    leg_z, leg_z, leg_z, leg_z
  ] "Landing contact offsets in body FLU [m]";
  Real leg_v_b[3, 4] "Landing contact velocities in body [m/s]";
  Real leg_f_w[3, 4] "Landing contact forces in world [N]";
  Real leg_f_b[3, 4] "Landing contact forces in body [N]";
  Real leg_m_b[3, 4] "Landing contact moments in body [N*m]";
  Real F_ground_b[3] "Total ground force in body FLU [N]";
  Real M_ground_b[3] "Total ground moment in body FLU [N*m]";

equation
  motor.omega_cmd = omega_cmd;
  omega_m = motor.omega;
  F_m = motor.thrust;
  T = F_m[1] + F_m[2] + F_m[3] + F_m[4];

  V = sqrt(v_b[1] * v_b[1] + v_b[2] * v_b[2] + v_b[3] * v_b[3] + 1e-12);
  drag_b = -0.5 * rho * V * (CdA .* v_b) - linear_drag .* v_b;

  M_rotor = motor_moment_map * F_m;
  M_rate = rate_damping .* omega;

  for i in 1:4 loop
    leg_v_b[:, i] = v_b + cross(omega, leg_r_b[:, i]);
    leg_h_w[i] = p[3] + R[3, :] * leg_r_b[:, i];
    leg_f_w[1, i] = if leg_h_w[i] < ground_z then -ground_tangent_c * (R[1, :] * leg_v_b[:, i]) else 0;
    leg_f_w[2, i] = if leg_h_w[i] < ground_z then -ground_tangent_c * (R[2, :] * leg_v_b[:, i]) else 0;
    leg_f_w[3, i] = if leg_h_w[i] < ground_z then max(0, ground_k * (ground_z - leg_h_w[i]) - ground_c * (R[3, :] * leg_v_b[:, i])) else 0;
    leg_m_b[:, i] = cross(leg_r_b[:, i], leg_f_b[:, i]);
  end for;

  leg_f_b = transpose(R) * leg_f_w;

  F_ground_b = leg_f_b * {1, 1, 1, 1};
  M_ground_b = leg_m_b * {1, 1, 1, 1};

  M_b = M_rotor + M_rate + M_ground_b;
  F_b = F_ground_b + drag_b + {0, 0, T};

  accel = R_FRD_FLU * a_b;
  gyro = R_FRD_FLU * omega;
  mag_world_w = R_NWU_NED * mag_world_ned;
  mag_b_flu = transpose(R) * mag_world_w;
  mag = R_FRD_FLU * mag_b_flu;

  position = p;
  velocity = v_w;
  quat = q;

end QuadrotorSIL;
