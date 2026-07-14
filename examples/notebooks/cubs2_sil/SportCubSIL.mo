// SPDX-License-Identifier: Apache-2.0
//
// CUBS2 SportCub 6-DOF plant for Rumoca/CMM simulations.
//
// Dynamics source:
// https://github.com/CogniPilot/cubs2/blob/main/cubs2_dynamics/cubs2_dynamics/sportcub.py
//
// The force and moment model follows that Python implementation while CMM's
// RigidBody6DOF remains the single source for rigid-body integration, gravity,
// quaternion kinematics, and the body/world velocity relationship.

function sportCubClip
  input Real value;
  input Real lower;
  input Real upper;
  output Real result;
algorithm
  result := min(max(value, lower), upper);
annotation(
  Inline = true);
end sportCubClip;

model SportCubPlant "CUBS2 SportCub dynamics on CMM RigidBody6DOF"
  parameter Real vehicle_mass(unit = "kg") = 0.065;
  parameter Real gravity(unit = "m/s2") = 9.81;
  parameter Real Jx(unit = "kg.m2") = 8.0e-4;
  parameter Real Jy(unit = "kg.m2") = 1.2e-3;
  parameter Real Jz(unit = "kg.m2") = 1.8e-3;
  parameter Real Jxz(unit = "kg.m2") = 1.0e-4;

  extends RigidBody.RigidBody6DOF(
    mass = vehicle_mass,
    g = gravity,
    ixx = Jx,
    iyy = Jy,
    izz = Jz,
    ixz = Jxz,
    p_start = {0.0, 0.0, 0.1},
    v_b_start = {0.0, 0.0, 0.0},
    qnorm_gain = 1.0
  );

  parameter Real rho(unit = "kg/m3") = 1.225;
  parameter Real S(unit = "m2") = 0.055;
  parameter Real span(unit = "m") = 0.617;
  parameter Real cbar(unit = "m") = 0.09;
  parameter Real wing_incidence(unit = "rad") = 0.10471975511965977;
  parameter Real thr_max(unit = "N") = 0.30;

  parameter Real CL0 = 0.5;
  parameter Real CLa = 4.7;
  parameter Real CD0 = 0.06;
  parameter Real k_ind = 0.09;
  parameter Real CD0_fp = 0.30;
  parameter Real CY_fp = 0.50;
  parameter Real Cm0 = 0.0;
  parameter Real Cma = -0.8;
  parameter Real Cmq = -12.0;
  parameter Real Cmde = 0.3;

  parameter Real CYb = -0.50;
  parameter Real CYda = 0.004;
  parameter Real CYdr = -0.015;
  parameter Real CYp = -0.15;
  parameter Real CYr = 0.20;
  parameter Real Clb = -0.25;
  parameter Real Clp = -0.50;
  parameter Real Clr = 0.15;
  parameter Real Clda = 0.05;
  parameter Real Cldr = 0.006;
  parameter Real Cnb = 0.06;
  parameter Real Cnp = 0.010;
  parameter Real Cnr = -0.15;
  parameter Real Cndr = 0.015;
  parameter Real Cnda = 0.006;

  parameter Real alpha_stall(unit = "rad") = 0.3490658503988659;
  parameter Real blend_width(unit = "rad") = 0.08726646259971647;
  parameter Real max_defl_ail(unit = "rad") = 0.5235987755982988;
  parameter Real max_defl_elev(unit = "rad") = 0.41887902047863906;
  parameter Real max_defl_rud(unit = "rad") = 0.3490658503988659;

  parameter Real disable_aero = 0.0;
  parameter Real disable_gf = 0.0;
  parameter Real ground_wn(unit = "rad/s") = 350.0;
  parameter Real ground_zeta = 0.6;
  parameter Real ground_c_xy(unit = "N.s/m") = 0.05;
  parameter Real ground_mu = 0.15;
  parameter Real ground_max_force_per_wheel(unit = "N") = 20.0;
  parameter Real tailwheel_steer_gain = 0.03;

  parameter Real wheel_x[3] = {0.1, 0.1, -0.4};
  parameter Real wheel_y[3] = {0.1, -0.1, 0.0};
  parameter Real wheel_z[3] = {-0.1, -0.1, 0.0};
  parameter Real eps = 1e-6;

  input Real ail "Aileron command [-1, 1]";
  input Real elev "Elevator command [-1, 1]";
  input Real rud "Rudder command [-1, 1]";
  input Real thr "Throttle command [0, 1]";

  output Real position[3](start = p_start) "World position ENU [m]";
  output Real velocity[3](start = v_b_start) "World velocity ENU [m/s]";
  output Real quat[4](start = q_start) "Quaternion w,x,y,z";
  output Real gyro[3](start = {0, 0, 0}) "Body angular rate FLU [rad/s]";
  output Real up_body[3](start = {0, 0, 1}) "World up expressed in body FLU";
  output Real airspeed(start = 0) "True airspeed [m/s]";
  output Real alpha_deg(start = 0) "Angle of attack [deg]";
  output Real beta_rad(start = 0) "Sideslip [rad]";
  output Real ail_rad(start = 0) "Aileron deflection [rad]";
  output Real elev_rad(start = 0) "Elevator deflection [rad]";
  output Real rud_rad(start = 0) "Rudder deflection [rad]";
  output Real thr_out(start = 0) "Throttle fraction [0, 1]";

protected
  Real U, V_frd, W_frd;
  Real Vt, Vxz, alpha_body, alpha, beta, qbar, sigma;
  Real P_frd, Q_frd, R_frd;
  Real wx1, wx2, wx3, wy1, wy2, wy3, wz1, wz2, wz3;
  Real refx, refz, rdot, wzt1, wzt2, wzt3, nz;
  Real CL_lin, CL_fp, CL, CD_lin, CD_fp, CD, CY_lin, CY_flat, CY;
  Real Cl_aero, Cm_aero, Cn_aero;
  Real FA_frd[3], MA_frd[3];
  Real F_aero[3], F_ground[3], F_thrust[3], M_aero[3], M_ground[3];
  Real ground_k, ground_c_vert;
  Real wh_x_w[3], wh_y_w[3], wh_z_w[3];
  Real wh_vbx[3], wh_vby[3], wh_vbz[3];
  Real wh_vwx[3], wh_vwy[3], wh_vwz[3];
  Real wh_penetration[3], wh_normal_unclamped[3], wh_normal[3];
  Real wh_lateral_x[3], wh_lateral_y[3], wh_lateral_mag[3], wh_lateral_scale[3];
  Real wh_Fw[3, 3], wh_Fb[3, 3], wh_Mb[3, 3];
  Real tailwheel_rud_rad, tailwheel_speed_sq, tailwheel_moment_z;

equation
  U = v_b[1];
  V_frd = -v_b[2];
  W_frd = -v_b[3];
  Vt = sqrt(U * U + V_frd * V_frd + W_frd * W_frd) + eps;
  Vxz = sqrt(U * U + W_frd * W_frd) + eps;
  alpha_body = atan2(W_frd, U);
  alpha = alpha_body + wing_incidence;
  beta = atan2(V_frd, Vxz);
  qbar = 0.5 * rho * Vt * Vt;

  P_frd = omega[1];
  Q_frd = -omega[2];
  R_frd = -omega[3];

  wx1 = U / Vt;
  wx2 = V_frd / Vt;
  wx3 = W_frd / Vt;
  refx = noEvent(if wx3 * wx3 < wx1 * wx1 then 0.0 else 1.0);
  refz = noEvent(if wx3 * wx3 < wx1 * wx1 then 1.0 else 0.0);
  rdot = refx * wx1 + refz * wx3;
  wzt1 = refx - rdot * wx1;
  wzt2 = -rdot * wx2;
  wzt3 = refz - rdot * wx3;
  nz = sqrt(wzt1 * wzt1 + wzt2 * wzt2 + wzt3 * wzt3) + eps;
  wz1 = wzt1 / nz;
  wz2 = wzt2 / nz;
  wz3 = wzt3 / nz;
  wy1 = wz2 * wx3 - wz3 * wx2;
  wy2 = wz3 * wx1 - wz1 * wx3;
  wy3 = wz1 * wx2 - wz2 * wx1;

  ail_rad = sportCubClip(max_defl_ail * ail, -max_defl_ail, max_defl_ail);
  elev_rad = sportCubClip(max_defl_elev * elev, -max_defl_elev, max_defl_elev);
  rud_rad = sportCubClip(-max_defl_rud * rud, -max_defl_rud, max_defl_rud);
  thr_out = sportCubClip(thr, 0.0, 1.0);

  sigma = (1.0 + tanh((alpha - alpha_stall) / blend_width)) / 2.0;
  CL_lin = CL0 + CLa * alpha;
  CL_fp = 2.0 * sin(alpha) * cos(alpha);
  CL = (1.0 - sigma) * CL_lin + sigma * CL_fp;
  CD_lin = CD0 + k_ind * CL_lin * CL_lin;
  CD_fp = CD0_fp + 2.0 * sin(alpha) * sin(alpha);
  CD = (1.0 - sigma) * CD_lin + sigma * CD_fp;
  CY_lin = CYb * beta
           + CYda * ail_rad
           + CYdr * rud_rad
           + CYp * (span / (2.0 * Vt)) * P_frd
           + CYr * (span / (2.0 * Vt)) * R_frd;
  CY_flat = CY_fp * sin(beta) * cos(alpha);
  CY = (1.0 - sigma) * CY_lin + sigma * CY_flat;
  Cl_aero = Clda * ail_rad
            + Cldr * rud_rad
            + Clb * beta
            + Clp * (span / (2.0 * Vt)) * P_frd
            + Clr * (span / (2.0 * Vt)) * R_frd;
  Cm_aero = Cm0 + Cma * alpha + Cmde * elev_rad
            + Cmq * (cbar / (2.0 * Vt)) * Q_frd;
  Cn_aero = Cnb * beta
            + Cndr * rud_rad
            + Cnda * ail_rad
            + Cnp * (span / (2.0 * Vt)) * P_frd
            + Cnr * (span / (2.0 * Vt)) * R_frd;

  FA_frd[1] = qbar * S * (wx1 * (-CD) + wy1 * CY + wz1 * (-CL));
  FA_frd[2] = qbar * S * (wx2 * (-CD) + wy2 * CY + wz2 * (-CL));
  FA_frd[3] = qbar * S * (wx3 * (-CD) + wy3 * CY + wz3 * (-CL));
  MA_frd[1] = qbar * S * span * Cl_aero;
  MA_frd[2] = qbar * S * cbar * Cm_aero;
  MA_frd[3] = qbar * S * span * Cn_aero;

  F_aero = {FA_frd[1], -FA_frd[2], -FA_frd[3]};
  M_aero = {MA_frd[1], -MA_frd[2], -MA_frd[3]};
  F_thrust = {thr_max * thr_out, 0.0, 0.0};

  ground_k = vehicle_mass * ground_wn * ground_wn;
  ground_c_vert = 2.0 * ground_zeta * vehicle_mass * ground_wn;
  tailwheel_rud_rad = max_defl_rud * sportCubClip(rud, -1.0, 1.0);

  for i in 1:3 loop
    wh_x_w[i] = R[1, 1] * wheel_x[i] + R[1, 2] * wheel_y[i] + R[1, 3] * wheel_z[i];
    wh_y_w[i] = R[2, 1] * wheel_x[i] + R[2, 2] * wheel_y[i] + R[2, 3] * wheel_z[i];
    wh_z_w[i] = R[3, 1] * wheel_x[i] + R[3, 2] * wheel_y[i] + R[3, 3] * wheel_z[i];

    wh_vbx[i] = v_b[1] + omega[2] * wheel_z[i] - omega[3] * wheel_y[i];
    wh_vby[i] = v_b[2] + omega[3] * wheel_x[i] - omega[1] * wheel_z[i];
    wh_vbz[i] = v_b[3] + omega[1] * wheel_y[i] - omega[2] * wheel_x[i];

    wh_vwx[i] = R[1, 1] * wh_vbx[i] + R[1, 2] * wh_vby[i] + R[1, 3] * wh_vbz[i];
    wh_vwy[i] = R[2, 1] * wh_vbx[i] + R[2, 2] * wh_vby[i] + R[2, 3] * wh_vbz[i];
    wh_vwz[i] = R[3, 1] * wh_vbx[i] + R[3, 2] * wh_vby[i] + R[3, 3] * wh_vbz[i];

    wh_penetration[i] = -(p[3] + wh_z_w[i]);
    wh_normal_unclamped[i] =
      wh_penetration[i] * ground_k - wh_vwz[i] * ground_c_vert;
    wh_normal[i] =
      sportCubClip(wh_normal_unclamped[i], 0.0, ground_max_force_per_wheel);

    wh_lateral_x[i] = -wh_vwx[i] * ground_c_xy;
    wh_lateral_y[i] = -wh_vwy[i] * ground_c_xy;
    wh_lateral_mag[i] =
      sqrt(wh_lateral_x[i] * wh_lateral_x[i]
           + wh_lateral_y[i] * wh_lateral_y[i]) + 1e-9;
    wh_lateral_scale[i] =
      min(1.0, ground_mu * wh_normal[i] / wh_lateral_mag[i]);

    wh_Fw[1, i] = noEvent(
      if p[3] + wh_z_w[i] < 0.0 then wh_lateral_scale[i] * wh_lateral_x[i] else 0.0);
    wh_Fw[2, i] = noEvent(
      if p[3] + wh_z_w[i] < 0.0 then wh_lateral_scale[i] * wh_lateral_y[i] else 0.0);
    wh_Fw[3, i] = noEvent(if p[3] + wh_z_w[i] < 0.0 then wh_normal[i] else 0.0);

    wh_Fb[1, i] = R[1, 1] * wh_Fw[1, i] + R[2, 1] * wh_Fw[2, i] + R[3, 1] * wh_Fw[3, i];
    wh_Fb[2, i] = R[1, 2] * wh_Fw[1, i] + R[2, 2] * wh_Fw[2, i] + R[3, 2] * wh_Fw[3, i];
    wh_Fb[3, i] = R[1, 3] * wh_Fw[1, i] + R[2, 3] * wh_Fw[2, i] + R[3, 3] * wh_Fw[3, i];

    wh_Mb[1, i] = wheel_y[i] * wh_Fb[3, i] - wheel_z[i] * wh_Fb[2, i];
    wh_Mb[2, i] = wheel_z[i] * wh_Fb[1, i] - wheel_x[i] * wh_Fb[3, i];
    wh_Mb[3, i] = wheel_x[i] * wh_Fb[2, i] - wheel_y[i] * wh_Fb[1, i];
  end for;

  tailwheel_speed_sq = wh_vwx[3] * wh_vwx[3];
  tailwheel_moment_z = noEvent(
    if p[3] + wh_z_w[3] < 0.0 then
      tailwheel_steer_gain * tailwheel_rud_rad * (sqrt(tailwheel_speed_sq + 1.0) - 1.0)
    else
      0.0);

  F_ground = {wh_Fb[1, 1] + wh_Fb[1, 2] + wh_Fb[1, 3],
              wh_Fb[2, 1] + wh_Fb[2, 2] + wh_Fb[2, 3],
              wh_Fb[3, 1] + wh_Fb[3, 2] + wh_Fb[3, 3]};
  M_ground = {wh_Mb[1, 1] + wh_Mb[1, 2] + wh_Mb[1, 3],
              wh_Mb[2, 1] + wh_Mb[2, 2] + wh_Mb[2, 3],
              wh_Mb[3, 1] + wh_Mb[3, 2] + wh_Mb[3, 3] + tailwheel_moment_z};

  F_b = (1.0 - disable_aero) * F_aero
        + (1.0 - disable_gf) * F_ground
        + F_thrust;
  M_b = (1.0 - disable_aero) * M_aero
        + (1.0 - disable_gf) * M_ground;

  gyro = omega;
  up_body = R[3, :];
  airspeed = Vt;
  alpha_deg = alpha * 57.29577951308232;
  beta_rad = beta;
  position = p;
  velocity = v_w;
  quat = q;
end SportCubPlant;
