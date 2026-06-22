// SPDX-License-Identifier: Apache-2.0
//
// Fixed-wing outer-loop autopilot for the HobbyZone Sport Cub S2.
//
// FAITHFUL Modelica transcription of the ROS controller, sample-for-sample:
//   * cub_control/cub_tecs_ros_xtrack.py       (node: LPF estimator, flight mode, takeoff)
//   * controller_cub/tecs_controller_xtrack.py (TECSControl_cub, roll_mode = "direct")
//   * navigation/cross_tracker_lookAhead.py     (XTrack_NAV_lookAhead)
//   * controller_cub/param/cub1.yaml            (gains)
//
// Discrete, fixed-rate (dt = 0.01 s) step controller. The when sample(0, dt)
// clock drives one control cycle per tick. State is held in discrete variables;
// pre(v) is v's value from the previous step. There are no continuous states
// and no zero-crossing events.

package CubControl
  constant Real pi = 3.141592653589793;

  function clamp
    input Real u;
    input Real uMin;
    input Real uMax;
    output Real y;
  algorithm
    y := min(max(u, uMin), uMax);
  end clamp;

  function wrapPi "wrap angle to (-pi, pi]"
    input Real angle;
    output Real wrapped;
  algorithm
    wrapped := atan2(sin(angle), cos(angle));
  end wrapPi;

  model FixedWingOuterLoop
    constant Real dt(unit = "s") = 0.01;
    parameter Real g = 9.81;

    // ── PURT circuit, constant 3 m altitude (node control_point) ────────────
    parameter Integer nWaypoints = 6;
    parameter Real waypoints[nWaypoints, 3] = [
      -4.0,  -5.0,  3.0;
      -3.0,   2.0,  3.0;
      16.20,  2.0,  3.0;
      16.0,  -4.22, 3.0;
      6.88,  -5.1,  3.0;
      -4.0,  -5.0,  3.0];

    // ── estimator / navigation (cross_tracker_lookAhead + node overrides) ───
    parameter Real filterCutoffHz = 10.0;
    parameter Real vCruise = 3.0                 "node speed_cruise";
    parameter Real K_h = 3.0                     "glide-slope gain (get_desired_flight)";
    parameter Real K_V = 1.0                     "des-accel gain (node)";
    parameter Real lookaheadTime = 1.5;
    parameter Real lookaheadMin = 1.0;
    parameter Real lookaheadMax = 5.0;
    parameter Real waypointSwitchingDistance = 4.0 "node override";

    // ── TECS longitudinal (cub1.yaml) ────────────────────────────────────────
    parameter Real mass = 0.057;
    parameter Real thrMax = 7.5;
    parameter Real trimThrust = 3.5;
    parameter Real K_thrustp = 0.01;
    parameter Real K_thrusti = 0.4215;
    parameter Real normEsDotIntegralMax = 7.5;
    parameter Real K_pitchp = 0.075;
    parameter Real K_pitchi = 0.216;
    parameter Real distTermIntegralMax = 7.5;
    parameter Real envelopeDrag = 1.0;
    parameter Real pitchCmdLim = 20.0 * pi / 180.0;

    // ── elevator inner loop (cub1.yaml) ──────────────────────────────────────
    parameter Real trimElev = 0.20;
    parameter Real K_elevp = 0.107;
    parameter Real K_elevi = 0.2107;
    parameter Real K_q = 0.2;
    parameter Real K_phi_elev = 2.5;
    parameter Real pitchIntegralMax = 0.3;

    // ── lateral "direct": yaw-error PID -> aileron (cub1.yaml) ────────────────
    parameter Real trimAil = 0.0;
    parameter Real K_deltap = 0.4;
    parameter Real K_deltai = 0.15;
    parameter Real K_deltad = 0.10;
    parameter Real rIntegralMax = 0.4;

    // ── heading -> bank shaping (computed every step; cub1.yaml) ─────────────
    parameter Real kChi = 1.20;
    parameter Real phiLim = 30.0 * pi / 180.0;
    parameter Real phiDotLim = 90.0 * pi / 180.0;
    parameter Real chiDeadband = 1.0 * pi / 180.0;

    // ── open-loop launch ─────────────────────────────────────────────────────
    parameter Real takeoffAltitude = 0.4         "airborne when z above this";
    parameter Real takeoffElev = 0.15            "open-loop launch pitch-up elevator";

    parameter Real stabilizerCmd = 2000.0        "node joy axes[4] (force onboard stabilizing)";

    // ── inputs: vehicle pose (Euler from quaternion done upstream) ────────────
    input Real x;
    input Real y;
    input Real z;
    input Real roll;
    input Real pitch;
    input Real yaw;

    // ── outputs: AETR + stabilizer + telemetry ───────────────────────────────
    discrete output Real aileron(start = 0.0);
    discrete output Real elevator(start = 0.0);
    discrete output Real throttle(start = 0.7);
    discrete output Real rudder(start = 0.0);
    discrete output Real stabilizer(start = 2000.0);
    discrete output Boolean airborne(start = false);
    discrete output Integer current_wp(start = 1);
    discrete output Real des_v(start = 0.0);
    discrete output Real des_gamma(start = 0.0);
    discrete output Real des_heading(start = 0.0);
    discrete output Real des_a(start = 0.0);
    discrete output Real phi_cmd(start = 0.0);
    discrete output Real chi_err(start = 0.0);
    discrete output Real x_est(start = 0.0);
    discrete output Real y_est(start = 0.0);
    discrete output Real z_est(start = 0.0);
    discrete output Real roll_est(start = 0.0);
    discrete output Real pitch_est(start = 0.0);
    discrete output Real yaw_est(start = 0.0);
    discrete output Real vx_est(start = 0.0);
    discrete output Real vy_est(start = 0.0);
    discrete output Real vz_est(start = 0.0);
    discrete output Real v_est(start = 0.0);
    discrete output Real gamma_est(start = 0.0);
    discrete output Real vdot_est(start = 0.0);
    discrete output Real p_est(start = 0.0);
    discrete output Real q_est(start = 0.0);
    discrete output Real r_est(start = 0.0);

  protected
    discrete Boolean started(start = false);
    discrete Real prev_x(start = 0.0);
    discrete Real prev_y(start = 0.0);
    discrete Real prev_z(start = 0.0);
    discrete Real prev_roll(start = 0.0);
    discrete Real prev_pitch(start = 0.0);
    discrete Real prev_yaw(start = 0.0);
    discrete Real prev_speed(start = 0.0);
    discrete Real time_s(start = 0.0);
    discrete Real err_norm_es_dot_int(start = 0.0);
    discrete Real err_dist_term_int(start = 0.0);
    discrete Real err_pitch_int(start = 0.0);
    discrete Real err_r_int(start = 0.0);
    discrete Real err_r_last(start = 0.0);
    discrete Real phi_cmd_state(start = 0.0);

    discrete Real alpha;
    discrete Real vx_new, vy_new, vz_new, speed_new;
    discrete Real p_new, q_new, r_new, gamma_new, vdot_new;
    discrete Real next_wx, next_wy, next_wz, prev_wx, prev_wy, prev_wz;
    discrete Real x_err, y_err, z_err, horz_dist_err;
    discrete Real path_vect[3], path_len, path_angle;
    discrete Real unit_along_path[2], unit_normal[2], pose_vect[2];
    discrete Real along_track_err_w0, along_track_err_w1, cross_track_err;
    discrete Real lookahead_nom, lookahead_eff, switch_threshold;
    discrete Real weight, drag, r_v_dot;
    discrete Real err_norm_es_dot, thrust_unsat, ref_thrust;
    discrete Real err_dist_term, pitch_unsat, ref_pitch;
    discrete Real pitch_ned, err_pitch, q_turn, err_q, nz_excess, ele_ff_phi;
    discrete Real chi, chi_dot_des, phi_des, dphi_max;
    discrete Real err_yaw, err_r_deriv;

  algorithm
    when sample(0.0, dt) then
      alpha := exp(-2.0 * pi * filterCutoffHz * dt);
      weight := mass * g;

      // first sample: seed prev/est with the current pose
      if not pre(started) then
        prev_x := x; prev_y := y; prev_z := z;
        prev_roll := roll; prev_pitch := pitch; prev_yaw := yaw;
        prev_speed := 0.0;
        x_est := x; y_est := y; z_est := z;
        roll_est := roll; pitch_est := pitch; yaw_est := yaw;
        vx_est := 0.0; vy_est := 0.0; vz_est := 0.0; v_est := 0.0;
        gamma_est := 0.0; vdot_est := 0.0; p_est := 0.0; q_est := 0.0; r_est := 0.0;
        started := true;
      else
        // ── state estimation: finite diff + exponential low-pass (pose_cb) ─────
        vx_new := (x - pre(prev_x)) / dt;
        vy_new := (y - pre(prev_y)) / dt;
        vz_new := (z - pre(prev_z)) / dt;
        speed_new := sqrt(vx_new * vx_new + vy_new * vy_new + vz_new * vz_new);
        p_new := wrapPi(roll - pre(prev_roll)) / dt;
        q_new := wrapPi(pitch - pre(prev_pitch)) / dt;
        r_new := wrapPi(yaw - pre(prev_yaw)) / dt;
        gamma_new := asin(clamp(vz_new / max(speed_new, 1e-5), -1.0, 1.0));
        vdot_new := speed_new - pre(prev_speed);       // prev_speed := previous v_est

        x_est := alpha * x + (1.0 - alpha) * pre(x_est);
        y_est := alpha * y + (1.0 - alpha) * pre(y_est);
        z_est := alpha * z + (1.0 - alpha) * pre(z_est);
        roll_est := alpha * roll + (1.0 - alpha) * pre(roll_est);
        pitch_est := alpha * pitch + (1.0 - alpha) * pre(pitch_est);
        yaw_est := alpha * yaw + (1.0 - alpha) * pre(yaw_est);
        vx_est := alpha * vx_new + (1.0 - alpha) * pre(vx_est);
        vy_est := alpha * vy_new + (1.0 - alpha) * pre(vy_est);
        vz_est := alpha * vz_new + (1.0 - alpha) * pre(vz_est);
        v_est := alpha * speed_new + (1.0 - alpha) * pre(v_est);
        gamma_est := alpha * gamma_new + (1.0 - alpha) * pre(gamma_est);
        vdot_est := alpha * vdot_new + (1.0 - alpha) * pre(vdot_est);
        p_est := alpha * p_new + (1.0 - alpha) * pre(p_est);
        q_est := alpha * q_new + (1.0 - alpha) * pre(q_est);
        r_est := alpha * r_new + (1.0 - alpha) * pre(r_est);
      end if;

        // ── flight mode: LEVEL check, recomputed every step (node) ─────────────
        airborne := z > takeoffAltitude;
        time_s := pre(time_s) + dt;

        if not airborne then
          // ── open-loop launch: full throttle, pitch up ──────────────────────
          throttle := 1.0;
          elevator := takeoffElev;
          aileron := 0.0;
          rudder := 0.0;
          des_v := 0.0; des_gamma := 0.0; des_heading := 0.0; des_a := 0.0;
          current_wp := pre(current_wp);
        else
          current_wp := pre(current_wp);

          // next = waypoints[current_wp]; prev = waypoints[current_wp-1] (origin for wp1)
          next_wx := waypoints[current_wp, 1];
          next_wy := waypoints[current_wp, 2];
          next_wz := waypoints[current_wp, 3];
          if current_wp == 1 then
            prev_wx := 0.0; prev_wy := 0.0; prev_wz := 0.0;
          else
            prev_wx := waypoints[current_wp - 1, 1];
            prev_wy := waypoints[current_wp - 1, 2];
            prev_wz := waypoints[current_wp - 1, 3];
          end if;

          // ── desired speed / flight-path / heading (get_desired_flight) ───────
          x_err := next_wx - x_est;
          y_err := next_wy - y_est;
          z_err := next_wz - z_est;
          horz_dist_err := sqrt(x_err * x_err + y_err * y_err);
          des_v := vCruise;
          des_gamma := if horz_dist_err <= 0.0 then 0.0 else K_h * z_err / horz_dist_err;

          path_vect := {next_wx - prev_wx, next_wy - prev_wy, next_wz - prev_wz};
          path_len := max(sqrt(path_vect[1]^2 + path_vect[2]^2 + path_vect[3]^2), 1e-6);
          path_angle := atan2(path_vect[2], path_vect[1]);
          unit_along_path := {path_vect[1] / path_len, path_vect[2] / path_len};
          unit_normal := {-path_vect[2] / path_len, path_vect[1] / path_len};
          pose_vect := {x_est - prev_wx, y_est - prev_wy};
          along_track_err_w0 := pose_vect[1] * unit_along_path[1] + pose_vect[2] * unit_along_path[2];
          along_track_err_w1 := max(0.0, path_len - clamp(along_track_err_w0, 0.0, path_len));
          cross_track_err := pose_vect[1] * unit_normal[1] + pose_vect[2] * unit_normal[2];
          lookahead_nom := clamp(sqrt(vx_est^2 + vy_est^2) * lookaheadTime, lookaheadMin, lookaheadMax);
          lookahead_eff := min(lookahead_nom, along_track_err_w1);
          des_heading := wrapPi(path_angle + atan2(-cross_track_err, max(lookahead_eff, 1e-6)));
          des_a := K_V * (des_v - abs(v_est));

          // ── TECS: desired thrust + pitch (compute_thrust_pitch) ──────────────
          // command uses the PREVIOUS integral; integral updated (anti-windup) after.
          drag := envelopeDrag;
          r_v_dot := clamp(des_a, -drag / weight, (thrMax - drag) / weight);
          err_norm_es_dot := (des_gamma - gamma_est) + (r_v_dot - vdot_est) / g;
          thrust_unsat := trimThrust + weight * (K_thrustp * (gamma_est + vdot_est / g)
                          + K_thrusti * pre(err_norm_es_dot_int));
          ref_thrust := clamp(thrust_unsat, 0.0, thrMax);
          if not ((ref_thrust >= thrMax - 1e-9 and err_norm_es_dot > 0.0)
                or (ref_thrust <= 1e-9 and err_norm_es_dot < 0.0)) then
            err_norm_es_dot_int := clamp(pre(err_norm_es_dot_int) + err_norm_es_dot * dt,
                                         -normEsDotIntegralMax, normEsDotIntegralMax);
          else
            err_norm_es_dot_int := pre(err_norm_es_dot_int);
          end if;

          err_dist_term := (des_gamma - gamma_est) - (r_v_dot - vdot_est) / g;
          pitch_unsat := K_pitchi * pre(err_dist_term_int) - K_pitchp * (gamma_est - vdot_est / g);
          ref_pitch := clamp(pitch_unsat, -pitchCmdLim, pitchCmdLim);
          if not ((ref_pitch >= pitchCmdLim - 1e-9 and err_dist_term > 0.0)
                or (ref_pitch <= -pitchCmdLim + 1e-9 and err_dist_term < 0.0)) then
            err_dist_term_int := clamp(pre(err_dist_term_int) + err_dist_term * dt,
                                       -distTermIntegralMax, distTermIntegralMax);
          else
            err_dist_term_int := pre(err_dist_term_int);
          end if;

          // ── elevator (compute_control); pitch remapped nose-up-positive (NED) ─
          pitch_ned := -pitch_est;
          err_pitch := wrapPi(ref_pitch - pitch_ned);
          q_turn := sin(roll_est) * cos(pitch_ned) * tan(roll_est) * g / max(v_est, 1e-5);
          err_q := wrapPi(q_turn - q_est);
          nz_excess := 1.0 / max(cos(roll_est), 1e-5) - 1.0;
          ele_ff_phi := K_phi_elev * nz_excess;
          err_pitch_int := clamp(pre(err_pitch_int) + err_pitch * dt, -pitchIntegralMax, pitchIntegralMax);
          elevator := clamp(trimElev + K_elevp * err_pitch + K_elevi * err_pitch_int
                            + K_q * err_q + ele_ff_phi, -1.0, 1.0);
          throttle := clamp(ref_thrust / thrMax, 0.0, 1.0);

          // ── heading -> bank shaping (computed every step; published) ─────────
          chi := atan2(vy_est, vx_est);
          chi_err := -wrapPi(des_heading - chi);
          if abs(chi_err) < chiDeadband then
            chi_err := 0.0;
          end if;
          chi_dot_des := kChi * chi_err;
          phi_des := clamp(atan2(max(v_est, 0.05) * chi_dot_des, g), -phiLim, phiLim);
          dphi_max := phiDotLim * dt;
          phi_des := clamp(phi_des - pre(phi_cmd_state), -dphi_max, dphi_max) + pre(phi_cmd_state);
          phi_cmd_state := clamp(phi_des, -phiLim, phiLim);
          phi_cmd := phi_cmd_state;

          // ── lateral "direct": yaw-error PID -> aileron ───────────────────────
          err_yaw := wrapPi(des_heading - yaw_est);
          err_r_deriv := (err_yaw - pre(err_r_last)) / dt;
          err_r_last := err_yaw;
          err_r_int := clamp(pre(err_r_int) + err_yaw * dt, -rIntegralMax, rIntegralMax);
          aileron := clamp(trimAil + K_deltap * err_yaw + K_deltai * err_r_int
                           + K_deltad * err_r_deriv, -1.0, 1.0);
          rudder := 0.0;

          // ── waypoint advance + circuit loop (check_arrived) ──────────────────
          switch_threshold := max(waypointSwitchingDistance, lookahead_nom);
          if along_track_err_w1 < switch_threshold then
            current_wp := if current_wp >= nWaypoints then 1 else current_wp + 1;
          end if;
        end if;

        stabilizer := stabilizerCmd;

        // history for next sample's finite differences (node end-of-cycle)
        prev_x := x; prev_y := y; prev_z := z;
        prev_roll := roll; prev_pitch := pitch; prev_yaw := yaw;
        prev_speed := v_est;
    end when;
  end FixedWingOuterLoop;
end CubControl;

