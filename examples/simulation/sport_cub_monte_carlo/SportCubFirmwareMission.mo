// SPDX-License-Identifier: Apache-2.0
//
// Consolidated Sport Cub mission model for the pure-Python Monte Carlo example.
//
// FixedWingOuterLoop below mirrors the controller source used to generate the
// Zephyr flight application: cerebri_cubs2/src/FixedWingOuterLoop.mo at commit
// e25c108fc2c4762fda8303594b82eb8ddccee0ad (source SHA-256
// 5737ebe5f1fc8d9cca9e28c22a29ed6c4caf0388cd18aabcded9026d8da5368f).
// SportCubPlant and SportCubReceiverStabilizer supply the simulation-only
// physics and the airplane's onboard SAFE receiver behavior around it.
// The controller equations and tuning are unchanged; this example replaces
// the deployed route table with the four-waypoint circuit requested here.
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
  Real F_aero[3], F_thrust[3], M_aero[3];

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

  F_b = F_aero + F_thrust;
  M_b = M_aero;

  gyro = omega;
  up_body = R[3, :];
  airspeed = Vt;
  alpha_deg = alpha * 57.29577951308232;
  beta_rad = beta;
  position = p;
  velocity = v_w;
  quat = q;
end SportCubPlant;

// The HobbyZone SAFE receiver is not part of cerebri_cubs2. This compact
// attitude/rate loop represents the stabilizer selected by the firmware's
// 2000 us stabilizer command, so firmware stick outputs can drive the plant.
model SportCubReceiverStabilizer
  parameter Real phi_sp_max = 0.90;
  parameter Real theta_sp_max = 0.45;
  parameter Real yaw_rate_max = 1.0;
  parameter Real Kp_phi = 5.0;
  parameter Real Kp_theta = 5.0;
  parameter Real p_rate_max = 4.0;
  parameter Real q_rate_max = 2.5;
  parameter Real Kp_p = 0.45;
  parameter Real Ki_p = 0.30;
  parameter Real ilim_p = 1.0;
  parameter Real Kp_q = 0.55;
  parameter Real Ki_q = 0.40;
  parameter Real ilim_q = 1.0;
  parameter Real Kp_r = 0.40;
  parameter Real Ki_r = 0.10;
  parameter Real ilim_r = 0.6;
  parameter Real v_prot_lo(unit = "m/s") = 2.6;
  parameter Real v_prot_hi(unit = "m/s") = 3.6;
  parameter Real dive_slope = 0.06;

  input Real stick_roll(start = 0.0);
  input Real stick_pitch(start = 0.0);
  input Real stick_yaw(start = 0.0);
  input Real stick_throttle(start = 0.0);
  input Real armed(start = 0.0);
  input Real gyro[3](start = {0.0, 0.0, 0.0});
  input Real up_body[3](start = {0.0, 0.0, 1.0});
  input Real airspeed(start = 0.0);

  output Real ail;
  output Real elev;
  output Real rud;
  output Real thr;

protected
  Real phi;
  Real theta;
  Real phi_sp;
  Real theta_sp;
  Real theta_eff;
  Real climb_auth;
  Real p_sp;
  Real q_up_sp;
  Real r_sp;
  Real e_p;
  Real e_q;
  Real e_r;
  Real i_p(start = 0.0, fixed = true);
  Real i_q(start = 0.0, fixed = true);
  Real i_r(start = 0.0, fixed = true);
  Real i_p_c;
  Real i_q_c;
  Real i_r_c;

equation
  phi = atan2(up_body[2], up_body[3]);
  theta = atan2(up_body[1], up_body[3]);
  climb_auth = min(1.0, max(0.0, (airspeed - v_prot_lo) / (v_prot_hi - v_prot_lo)));
  phi_sp = armed * min(phi_sp_max, max(-phi_sp_max, stick_roll * phi_sp_max));
  theta_sp = armed * min(theta_sp_max, max(-theta_sp_max,
    noEvent(if stick_pitch > 0.0 then
      stick_pitch * theta_sp_max * climb_auth
    else
      stick_pitch * theta_sp_max)));
  theta_eff = noEvent(if airspeed < v_prot_lo then
    min(theta_sp, -dive_slope * (v_prot_lo - airspeed))
  else
    theta_sp);

  p_sp = min(p_rate_max, max(-p_rate_max, Kp_phi * (phi_sp - phi)));
  q_up_sp = min(q_rate_max, max(-q_rate_max, Kp_theta * (theta_eff - theta)));
  r_sp = stick_yaw * yaw_rate_max;
  e_p = p_sp - gyro[1];
  e_q = q_up_sp + gyro[2];
  e_r = r_sp - gyro[3];

  der(i_p) = noEvent(if i_p > ilim_p then min(0.0, armed * e_p)
    else if i_p < -ilim_p then max(0.0, armed * e_p) else armed * e_p);
  der(i_q) = noEvent(if i_q > ilim_q then min(0.0, armed * e_q)
    else if i_q < -ilim_q then max(0.0, armed * e_q) else armed * e_q);
  der(i_r) = noEvent(if i_r > ilim_r then min(0.0, armed * e_r)
    else if i_r < -ilim_r then max(0.0, armed * e_r) else armed * e_r);
  i_p_c = min(ilim_p, max(-ilim_p, i_p));
  i_q_c = min(ilim_q, max(-ilim_q, i_q));
  i_r_c = min(ilim_r, max(-ilim_r, i_r));

  ail = Kp_p * e_p + Ki_p * i_p_c;
  elev = Kp_q * e_q + Ki_q * i_q_c;
  rud = Kp_r * e_r + Ki_r * i_r_c;
  thr = armed * stick_throttle;
end SportCubReceiverStabilizer;

function firmwareVectorNorm2
  input Real v[2];
  output Real result;
algorithm
  result := sqrt(v * v);
end firmwareVectorNorm2;

function firmwareVectorNorm3
  input Real v[3];
  output Real result;
algorithm
  result := sqrt(v * v);
end firmwareVectorNorm3;

// This state/tuning block and FixedWingOuterLoop are kept structurally aligned
// with the firmware source. FixedWingOuterLoop owns the sampled update because
// the generated eFMI is a single-rate discrete model.
block FirmwareDiscretePidController
  parameter Integer nAxes = 2;
  parameter Real trim[nAxes] = {0.0, 0.0};
  parameter Real kp[nAxes] = {0.4, 1.2};
  parameter Real ki[nAxes] = {0.4, 0.05};
  parameter Real kd[nAxes] = {0.0, 0.35};
  parameter Real integralMax[nAxes] = {0.5, 0.4};
  parameter Real commandMin[nAxes] = {-1.0, -1.0};
  parameter Real commandMax[nAxes] = {1.0, 1.0};

  discrete Real error[nAxes](each start = 0.0);
  discrete Real derivative[nAxes](each start = 0.0);
  discrete Real feedforward[nAxes](each start = 0.0);
  discrete Real integral[nAxes](each start = 0.0);
  discrete Real command[nAxes](each start = 0.0);
end FirmwareDiscretePidController;

block FirmwareTecsController
  parameter Real mass(unit = "kg") = 0.063;
  parameter Real thrustMax(unit = "N") = 0.30;
  parameter Real trimThrust(unit = "N") = 0.1;
  parameter Real thrustKp = 0.01;
  parameter Real thrustKi = 0.25;
  parameter Real energyRateIntegralMax = 3.0;
  parameter Real pitchKp = 0.075;
  parameter Real pitchKi = 0.216;
  parameter Real distanceIntegralMax = 7.5;
  parameter Real envelopeDrag(unit = "N") = 0.07;
  parameter Real pitchCommandLimit(unit = "rad") = 12.0 * 3.141592653589793 / 180.0;

  discrete Real weight(unit = "N", start = 0.0);
  discrete Real drag(unit = "N", start = 0.0);
  discrete Real desiredSpecificAcceleration(start = 0.0);
  discrete Real energyRateError(start = 0.0);
  discrete Real energyRateIntegral(start = 0.0);
  discrete Real thrustUnsat(unit = "N", start = 0.0);
  discrete Real thrustCommand(unit = "N", start = 0.0);
  discrete Real distanceError(start = 0.0);
  discrete Real distanceIntegral(start = 0.0);
  discrete Real pitchUnsat(unit = "rad", start = 0.0);
  discrete Real pitchCommand(unit = "rad", start = 0.0);
end FirmwareTecsController;

model FixedWingOuterLoop
  constant Real pi = 3.141592653589793;
  constant Real dt(unit = "s") = 0.02;
  constant Integer nStickPids = 2;
  constant Real zero3[3] = {0.0, 0.0, 0.0};
  parameter Real g(unit = "m/s2") = 9.81;

  constant Integer nRoutePoints = 5;
  constant Integer nSegments = nRoutePoints - 1;
  parameter Real waypoints[nRoutePoints, 3] = [
    -4.0,  -5.0,   3.0;
    -3.0,   2.0,   3.0;
    16.20,  2.0,   3.0;
    16.0,  -4.22,  3.0;
    -4.0,  -5.0,  3.0];

  parameter Real filterCutoffHz(unit = "Hz") = 10.0;
  parameter Real vCruise(unit = "m/s") = 4.0;
  parameter Real K_h = 2.0;
  parameter Real K_V = 1.0;
  parameter Real lookaheadTime(unit = "s") = 2.0;
  parameter Real lookaheadMin(unit = "m") = 3.0;
  parameter Real lookaheadMax(unit = "m") = 8.0;
  parameter Real waypointSwitchingDistance(unit = "m") = 3.0;

  FirmwareDiscretePidController stick_pid(nAxes = nStickPids);
  FirmwareTecsController tecs;

  parameter Real K_phi_elev = 1.5;
  parameter Real kChi = 1.20;
  parameter Real phiLim = 30.0 * pi / 180.0;
  parameter Real phiDotLim = 90.0 * pi / 180.0;
  parameter Real chiDeadband = 1.0 * pi / 180.0;
  parameter Real takeoffAltitude(unit = "m") = 0.4;
  parameter Real takeoffElev = 0.15;
  parameter Real stabilizerCmd(unit = "us") = 2000.0;

  input Real position_m[3](each unit = "m");
  input Real euler_rad[3](each unit = "rad");

  discrete output Real aileron(start = 0.0);
  discrete output Real elevator(start = 0.0);
  discrete output Real throttle(start = 0.7);
  discrete output Real rudder(start = 0.0);
  discrete output Real stabilizer(start = 2000.0);
  discrete output Boolean airborne(start = false);
  discrete output Integer current_wp(min = 1, max = 4, start = 1);
  discrete output Real des_v(start = 0.0);
  discrete output Real des_gamma(start = 0.0);
  discrete output Real des_heading(start = 0.0);
  discrete output Real des_a(start = 0.0);
  discrete output Real phi_cmd(start = 0.0);
  discrete output Real chi_err(start = 0.0);
  discrete output Real position_est_m[3](each start = 0.0);
  discrete output Real euler_est_rad[3](each start = 0.0);
  discrete output Real velocity_est_m_s[3](each start = 0.0);
  discrete output Real v_est(start = 0.0);
  discrete output Real gamma_est(start = 0.0);
  discrete output Real vdot_est(start = 0.0);
  discrete output Real euler_rate_est_rad_s[3](each start = 0.0);

protected
  discrete Boolean started(start = false);
  discrete Real prev_position_m[3](each start = 0.0);
  discrete Real prev_euler_rad[3](each start = 0.0);
  discrete Real prev_speed(start = 0.0);
  discrete Real time_s(start = 0.0);
  discrete Real phi_cmd_state(start = 0.0);
  discrete Real alpha;
  discrete Real velocity_new_m_s[3];
  discrete Real euler_rate_new_rad_s[3];
  discrete Real speed_new;
  discrete Real gamma_new;
  discrete Real vdot_new;
  discrete Real next_wp[3];
  discrete Real prev_wp[3];
  discrete Real position_error[3];
  discrete Real horz_dist_err;
  discrete Real path[3];
  discrete Real path_len;
  discrete Real path_angle;
  discrete Real path_unit[2];
  discrete Real path_normal[2];
  discrete Real pose_from_prev[2];
  discrete Real along_track_err_w0;
  discrete Real along_track_err_w1;
  discrete Real cross_track_err;
  discrete Real lookahead_nom;
  discrete Real lookahead_eff;
  discrete Real lookahead_heading;
  discrete Real switch_threshold;
  discrete Real pitch_ned;
  discrete Real err_pitch;
  discrete Real q_turn;
  discrete Real err_q;
  discrete Real nz_excess;
  discrete Real ele_ff_phi;
  discrete Real chi;
  discrete Real chi_dot_des;
  discrete Real phi_des;
  discrete Real dphi_max;
  discrete Real err_yaw;

algorithm
  when sample(0.0, dt) then
    alpha := exp(-2.0 * pi * filterCutoffHz * dt);
    current_wp := pre(current_wp);

    if not pre(started) then
      prev_position_m := position_m;
      prev_euler_rad := euler_rad;
      prev_speed := 0.0;
      position_est_m := position_m;
      euler_est_rad := euler_rad;
      velocity_est_m_s := zero3;
      euler_rate_est_rad_s := zero3;
      v_est := 0.0;
      gamma_est := 0.0;
      vdot_est := 0.0;
      started := true;
    else
      for i in 1:3 loop
        velocity_new_m_s[i] := (position_m[i] - pre(prev_position_m[i])) / dt;
        euler_rate_new_rad_s[i] :=
          atan2(sin(euler_rad[i] - pre(prev_euler_rad[i])),
                cos(euler_rad[i] - pre(prev_euler_rad[i]))) / dt;
      end for;
      speed_new := sqrt(velocity_new_m_s * velocity_new_m_s);
      gamma_new := asin(min(max(velocity_new_m_s[3] / max(speed_new, 1e-5), -1.0), 1.0));
      vdot_new := speed_new - pre(prev_speed);

      for i in 1:3 loop
        position_est_m[i] :=
          alpha * position_m[i] + (1.0 - alpha) * pre(position_est_m[i]);
        euler_est_rad[i] :=
          alpha * euler_rad[i] + (1.0 - alpha) * pre(euler_est_rad[i]);
        velocity_est_m_s[i] :=
          alpha * velocity_new_m_s[i] + (1.0 - alpha) * pre(velocity_est_m_s[i]);
        euler_rate_est_rad_s[i] :=
          alpha * euler_rate_new_rad_s[i]
          + (1.0 - alpha) * pre(euler_rate_est_rad_s[i]);
      end for;
      v_est := alpha * speed_new + (1.0 - alpha) * pre(v_est);
      gamma_est := alpha * gamma_new + (1.0 - alpha) * pre(gamma_est);
      vdot_est := alpha * vdot_new + (1.0 - alpha) * pre(vdot_est);
    end if;

    prev_wp := {waypoints[current_wp, 1],
                waypoints[current_wp, 2],
                waypoints[current_wp, 3]};
    next_wp := {waypoints[current_wp + 1, 1],
                waypoints[current_wp + 1, 2],
                waypoints[current_wp + 1, 3]};
    position_error := next_wp - position_est_m;
    horz_dist_err := sqrt(position_error[1] * position_error[1]
                          + position_error[2] * position_error[2]);
    path := next_wp - prev_wp;
    path_len := max(sqrt(path * path), 1e-6);
    path_angle := atan2(path[2], path[1]);
    path_unit := {path[1], path[2]} / path_len;
    path_normal := {-path_unit[2], path_unit[1]};
    pose_from_prev :=
      {position_est_m[1], position_est_m[2]} - {prev_wp[1], prev_wp[2]};
    along_track_err_w0 := pose_from_prev * path_unit;
    along_track_err_w1 :=
      max(0.0, path_len - min(max(along_track_err_w0, 0.0), path_len));
    cross_track_err := pose_from_prev * path_normal;
    lookahead_nom := min(max(
      sqrt(velocity_est_m_s[1] * velocity_est_m_s[1]
           + velocity_est_m_s[2] * velocity_est_m_s[2]) * lookaheadTime,
      lookaheadMin), lookaheadMax);
    lookahead_eff := max(lookaheadMin, min(lookahead_nom, along_track_err_w1));
    lookahead_heading :=
      path_angle + atan2(-cross_track_err, max(lookahead_eff, 1e-6));

    airborne := pre(airborne) or (position_m[3] > takeoffAltitude);
    time_s := pre(time_s) + dt;

    if not airborne then
      throttle := 1.0;
      elevator := takeoffElev;
      aileron := 0.0;
      rudder := 0.0;
      des_v := 0.0;
      des_gamma := 0.0;
      des_heading := 0.0;
      des_a := 0.0;
    else
      des_v := vCruise;
      des_gamma := min(max(
        K_h * position_error[3] / max(horz_dist_err, lookaheadMin), -0.12), 0.12);
      des_heading := atan2(sin(lookahead_heading), cos(lookahead_heading));
      des_a := K_V * (des_v - abs(v_est));

      tecs.weight := tecs.mass * g;
      tecs.drag := tecs.envelopeDrag;
      tecs.desiredSpecificAcceleration :=
        min(max(des_a, -tecs.drag / tecs.weight),
            (tecs.thrustMax - tecs.drag) / tecs.weight);
      tecs.energyRateError :=
        (des_gamma - gamma_est) + (tecs.desiredSpecificAcceleration - vdot_est) / g;
      tecs.thrustUnsat :=
        tecs.trimThrust
        + tecs.weight * (tecs.thrustKp * (gamma_est + vdot_est / g)
        + tecs.thrustKi * pre(tecs.energyRateIntegral));
      tecs.thrustCommand := min(max(tecs.thrustUnsat, 0.0), tecs.thrustMax);
      if not ((tecs.thrustCommand >= tecs.thrustMax - 1e-9
               and tecs.energyRateError > 0.0)
          or (tecs.thrustCommand <= 1e-9 and tecs.energyRateError < 0.0)) then
        tecs.energyRateIntegral :=
          min(max(pre(tecs.energyRateIntegral) + tecs.energyRateError * dt,
                  -tecs.energyRateIntegralMax), tecs.energyRateIntegralMax);
      else
        tecs.energyRateIntegral := pre(tecs.energyRateIntegral);
      end if;

      tecs.distanceError :=
        (des_gamma - gamma_est) - (tecs.desiredSpecificAcceleration - vdot_est) / g;
      tecs.pitchUnsat :=
        tecs.pitchKi * pre(tecs.distanceIntegral)
        - tecs.pitchKp * (gamma_est - vdot_est / g);
      tecs.pitchCommand :=
        min(max(tecs.pitchUnsat, -tecs.pitchCommandLimit), tecs.pitchCommandLimit);
      if not ((tecs.pitchCommand >= tecs.pitchCommandLimit - 1e-9
               and tecs.distanceError > 0.0)
          or (tecs.pitchCommand <= -tecs.pitchCommandLimit + 1e-9
              and tecs.distanceError < 0.0)) then
        tecs.distanceIntegral :=
          min(max(pre(tecs.distanceIntegral) + tecs.distanceError * dt,
                  -tecs.distanceIntegralMax), tecs.distanceIntegralMax);
      else
        tecs.distanceIntegral := pre(tecs.distanceIntegral);
      end if;

      pitch_ned := -euler_est_rad[2];
      err_pitch := atan2(sin(tecs.pitchCommand - pitch_ned),
                         cos(tecs.pitchCommand - pitch_ned));
      q_turn := sin(euler_est_rad[1]) * cos(pitch_ned)
                * tan(euler_est_rad[1]) * g / max(v_est, 1e-5);
      err_q := atan2(sin(q_turn - euler_rate_est_rad_s[2]),
                     cos(q_turn - euler_rate_est_rad_s[2]));
      nz_excess := 1.0 / max(cos(euler_est_rad[1]), 1e-5) - 1.0;
      ele_ff_phi := K_phi_elev * nz_excess;
      throttle := min(max(tecs.thrustCommand / tecs.thrustMax, 0.0), 1.0);

      chi := atan2(velocity_est_m_s[2], velocity_est_m_s[1]);
      chi_err := -atan2(sin(des_heading - chi), cos(des_heading - chi));
      if abs(chi_err) < chiDeadband then
        chi_err := 0.0;
      end if;
      chi_dot_des := kChi * chi_err;
      phi_des := min(max(atan2(max(v_est, 0.05) * chi_dot_des, g), -phiLim), phiLim);
      dphi_max := phiDotLim * dt;
      phi_des :=
        min(max(phi_des - pre(phi_cmd_state), -dphi_max), dphi_max)
        + pre(phi_cmd_state);
      phi_cmd_state := min(max(phi_des, -phiLim), phiLim);
      phi_cmd := phi_cmd_state;

      err_yaw := atan2(sin(des_heading - euler_est_rad[3]),
                       cos(des_heading - euler_est_rad[3]));
      stick_pid.error := {err_pitch, err_yaw};
      stick_pid.derivative := {err_q, (err_yaw - pre(stick_pid.error[2])) / dt};
      stick_pid.feedforward := {ele_ff_phi, 0.0};

      for i in 1:nStickPids loop
        stick_pid.integral[i] :=
          min(max(pre(stick_pid.integral[i]) + stick_pid.error[i] * dt,
                  -stick_pid.integralMax[i]), stick_pid.integralMax[i]);
        stick_pid.command[i] :=
          min(max(stick_pid.trim[i]
                  + stick_pid.kp[i] * stick_pid.error[i]
                  + stick_pid.ki[i] * stick_pid.integral[i]
                  + stick_pid.kd[i] * stick_pid.derivative[i]
                  + stick_pid.feedforward[i],
                  stick_pid.commandMin[i]), stick_pid.commandMax[i]);
      end for;
      elevator := stick_pid.command[1];
      aileron := stick_pid.command[2];
      rudder := 0.0;

      switch_threshold := waypointSwitchingDistance;
      if along_track_err_w1 < switch_threshold then
        current_wp := if current_wp >= nSegments then 1 else current_wp + 1;
      end if;
    end if;

    stabilizer := stabilizerCmd;
    prev_position_m := position_m;
    prev_euler_rad := euler_rad;
    prev_speed := v_est;
  end when;
end FixedWingOuterLoop;

function sportCubEulerFromQuat
  input Real q[4];
  output Real euler_rad[3] "{roll, pitch, yaw} [rad]";
protected
  Real yawPitchRoll_rad[3];
algorithm
  yawPitchRoll_rad := LieGroups.SO3.EulerB321.from_Quat(q);
  euler_rad := {yawPitchRoll_rad[3], yawPitchRoll_rad[2], yawPitchRoll_rad[1]};
annotation(
  Inline = true);
end sportCubEulerFromQuat;

model SportCubFirmwareMission
  "One closed loop: Sport Cub physics + SAFE receiver + Zephyr controller"
  parameter Real launch_heading(unit = "rad") = 1.4288992721907328
    "heading along the first four-waypoint mission leg";

  SportCubPlant vehicle(
    p_start = {-4.0, -5.0, 3.0},
    v_b_start = {4.0, 0.0, 0.0},
    q_start = {cos(launch_heading / 2.0), 0.0, 0.0, sin(launch_heading / 2.0)});
  SportCubReceiverStabilizer receiver;
  FixedWingOuterLoop firmware;

  output Real time_s;
  output Real x_m;
  output Real y_m;
  output Real altitude_m;
  output Real airspeed_m_s;
  output Real speed_estimate_m_s;
  output Real desired_speed_m_s;
  output Real roll_rad;
  output Real pitch_rad;
  output Real yaw_rad;
  output Real current_waypoint;
  output Real airborne;
  output Real aileron_cmd;
  output Real elevator_cmd;
  output Real throttle_cmd;

protected
  Real euler_rad[3];

equation
  euler_rad = sportCubEulerFromQuat(vehicle.quat);

  firmware.position_m = vehicle.position;
  firmware.euler_rad = euler_rad;

  receiver.armed = 1.0;
  receiver.stick_roll = -firmware.aileron;
  receiver.stick_pitch = firmware.elevator;
  receiver.stick_yaw = firmware.rudder;
  receiver.stick_throttle = firmware.throttle;
  receiver.gyro = vehicle.gyro;
  receiver.up_body = vehicle.up_body;
  receiver.airspeed = vehicle.airspeed;

  vehicle.ail = receiver.ail;
  vehicle.elev = receiver.elev;
  vehicle.rud = receiver.rud;
  vehicle.thr = receiver.thr;

  time_s = time;
  x_m = vehicle.position[1];
  y_m = vehicle.position[2];
  altitude_m = vehicle.position[3];
  airspeed_m_s = vehicle.airspeed;
  speed_estimate_m_s = firmware.v_est;
  desired_speed_m_s = firmware.des_v;
  roll_rad = euler_rad[1];
  pitch_rad = euler_rad[2];
  yaw_rad = euler_rad[3];
  current_waypoint = firmware.current_wp;
  airborne = if firmware.airborne then 1.0 else 0.0;
  aileron_cmd = firmware.aileron;
  elevator_cmd = firmware.elevator;
  throttle_cmd = firmware.throttle;
end SportCubFirmwareMission;
