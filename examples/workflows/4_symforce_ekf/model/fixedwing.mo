// 6-DOF fixed-wing SIL plant + fly-by-wire controller.
//
// Built on the shared CMM RigidBody / LieGroup packages, exactly like the
// quadrotor example, so the two share the validated rigid-body integrator.
//
//   World frame : Z-up
//   Body frame  : FLU (x forward / nose, y left, z up)
//   Quaternion  : {w, x, y, z} scalar-first, body-to-world
//   Gravity     : handled inside RigidBody6DOF (F_b is the NON-gravity force)
//
// Aerodynamics are built in the conventional FRD / stability axes and then
// rotated into the library's FLU body frame (negate the y, z components).
//
//   FixedWingPlant : aero + thrust -> RigidBody6DOF
//   FixedWingFBW   : attitude-stabilized fly-by-wire (stick -> surfaces)
//   FixedWing      : closed-loop wrapper (plant + controller in one model)

model FixedWingPlant "6-DOF fixed-wing aero plant on RigidBody6DOF"
  // Mass / inertia (small ~1.5 kg UAV, 1.5 m span)
  parameter Real vehicle_mass = 1.5 "Vehicle mass [kg]";
  parameter Real vehicle_ixx = 0.06 "Body inertia xx (roll) [kg*m^2]";
  parameter Real vehicle_iyy = 0.08 "Body inertia yy (pitch) [kg*m^2]";
  parameter Real vehicle_izz = 0.13 "Body inertia zz (yaw) [kg*m^2]";

  // Start at rest on the ground (wheels just touching), ready to take off.
  extends RigidBody.RigidBody6DOF(
    mass = vehicle_mass,
    g = 9.81,
    ixx = vehicle_ixx,
    iyy = vehicle_iyy,
    izz = vehicle_izz,
    p_start = {0, 0, 0.149},
    v_b_start = {0, 0, 0},
    qnorm_gain = 1.0
  );

  // Reference geometry
  parameter Real rho = 1.225 "Air density [kg/m^3]";
  parameter Real S = 0.25 "Wing reference area [m^2]";
  parameter Real span = 1.5 "Wing span [m]";
  parameter Real cbar = 0.167 "Mean aerodynamic chord [m]";
  parameter Real wing_incidence = 0.10472 "Wing incidence angle [rad] (6 deg, cubs2)";
  parameter Real thr_max = 12.0 "Maximum thrust [N]";

  // Tricycle landing gear — contact points in body FLU, matched to the GLB
  // wheels at the visual scale (1 unit = 1 m). Per-wheel spring-damper normal
  // force, light rolling resistance (forward) and firmer lateral grip so it
  // tracks straight on the ground and absorbs landings.
  parameter Real ground_k = 3000 "Gear stiffness per wheel [N/m]";
  parameter Real ground_c = 150 "Gear normal damping per wheel [N*s/m]";
  parameter Real roll_fric = 0.1 "Rolling resistance [N/(m/s)]";
  parameter Real side_fric = 25 "Lateral grip [N/(m/s)]";
  parameter Real wheel_x[3] = {0.164, -0.173, -0.173} "Wheel fwd offsets nose,L,R [m]";
  parameter Real wheel_y[3] = {0.0, 0.185, -0.185} "Wheel left offsets [m]";
  parameter Real wheel_z[3] = {-0.151, -0.151, -0.151} "Wheel down offsets [m]";

  // Longitudinal coefficients (identified SportCub, cubs2)
  parameter Real CL0 = 0.5 "Lift at zero AoA";
  parameter Real CLa = 4.7 "Lift slope [1/rad]";
  parameter Real CD0 = 0.06 "Parasitic drag";
  parameter Real k_ind = 0.09 "Induced drag factor";
  parameter Real CD0_fp = 0.30 "Flat-plate drag (post-stall)";
  parameter Real Cm0 = 0.0 "Pitch moment at alpha=0";
  parameter Real Cma = -0.8 "Pitch stiffness (static stability) [1/rad]";
  parameter Real Cmq = -12.0 "Pitch damping";
  parameter Real Cmde = 0.3 "Elevator pitch effectiveness";

  // Lateral / directional coefficients (identified SportCub, cubs2)
  parameter Real CYb = -0.50 "Sideslip side force [1/rad]";
  parameter Real CYda = 0.004 "Aileron side force";
  parameter Real CYdr = -0.015 "Rudder side force";
  parameter Real CYp = -0.15 "Roll-rate side force";
  parameter Real CYr = 0.20 "Yaw-rate side force";
  parameter Real CY_fp_coef = 0.50 "Flat-plate side force (post-stall)";
  parameter Real Clb = -0.25 "Dihedral effect (roll due to sideslip)";
  parameter Real Clp = -0.50 "Roll damping";
  parameter Real Clr = 0.15 "Yaw-roll coupling";
  parameter Real Clda = 0.05 "Aileron roll effectiveness";
  parameter Real Cldr = 0.006 "Rudder roll";
  parameter Real Cnb = 0.06 "Weathercock stability";
  parameter Real Cnp = 0.010 "Roll-yaw coupling";
  parameter Real Cnr = -0.15 "Yaw damping";
  parameter Real Cndr = 0.015 "Rudder yaw effectiveness";
  parameter Real Cnda = 0.006 "Aileron adverse yaw";

  // Stall blending (flat-plate beyond alpha_stall, cubs2)
  parameter Real alpha_stall = 0.349 "Stall angle [rad] (20 deg)";
  parameter Real blend_width = 0.0873 "Stall blend width [rad] (5 deg)";

  // Control-surface travel (cubs2)
  parameter Real max_defl_ail = 0.5236 "Aileron travel [rad] (30 deg)";
  parameter Real max_defl_elev = 0.4189 "Elevator travel [rad] (24 deg)";
  parameter Real max_defl_rud = 0.349 "Rudder travel [rad] (20 deg)";

  parameter Real eps = 1e-6;

  // Normalized pilot/controller commands
  input Real ail "Aileron command [-1..1]";
  input Real elev "Elevator command [-1..1]";
  input Real rud "Rudder command [-1..1]";
  input Real thr "Throttle command [0..1]";

  // Viewer / controller outputs
  output Real position[3](start = p_start) "World position [m]";
  output Real velocity[3](start = v_b_start) "World velocity [m/s]";
  output Real quat[4](start = q_start) "Quaternion w,x,y,z";
  output Real gyro[3](start = {0, 0, 0}) "Body angular rate FLU [rad/s]";
  output Real up_body[3](start = {0, 0, 1}) "World up expressed in body FLU";
  output Real airspeed(start = 15) "True airspeed [m/s]";
  output Real alpha_deg(start = 0) "Angle of attack [deg]";
  output Real ail_rad(start = 0) "Aileron deflection [rad]";
  output Real elev_rad(start = 0) "Elevator deflection [rad]";
  output Real rud_rad(start = 0) "Rudder deflection [rad]";
  output Real thr_out(start = 0) "Throttle fraction [0..1]";

protected
  Real U, V_frd, W_frd "Body velocity in FRD axes [m/s]";
  Real Vt, Vxz, alpha_body, alpha, beta, qbar, sigma;
  Real P_frd, Q_frd, R_frd "Body rates in FRD axes [rad/s]";
  Real wx1, wx2, wx3, wy1, wy2, wy3, wz1, wz2, wz3 "Wind-frame axes in body FRD";
  Real refx, refz, rdot, wzt1, wzt2, wzt3, nz "Wind-frame Gram-Schmidt temporaries";
  Real CL_lin, CL_fp, CL, CD_lin, CD_fp, CD, CY_lin, CY_fp, CY;
  Real Cl_aero, Cm_aero, Cn_aero;
  Real FA_frd[3] "Aero force in body FRD [N]";
  Real MA_frd[3] "Aero moment in body FRD [N*m]";
  Real F_aero[3], F_thrust[3], M_aero[3] "In body FLU";
  Real wh_h[3], wh_vbx[3], wh_vby[3], wh_vbz[3], wh_vwz[3], wh_Fn[3];
  Real wh_F[3, 3], wh_M[3, 3] "Per-wheel force / moment in body FLU";
  Real F_ground[3], M_ground[3] "Total ground wrench in body FLU";

equation
  // --- FLU body velocity / rates -> FRD aero convention ---
  U = v_b[1];
  V_frd = -v_b[2];
  W_frd = -v_b[3];
  Vt = sqrt(U*U + V_frd*V_frd + W_frd*W_frd) + eps;
  Vxz = sqrt(U*U + W_frd*W_frd) + eps;
  alpha_body = atan2(W_frd, U);
  alpha = alpha_body + wing_incidence;
  beta = atan2(V_frd, Vxz);
  qbar = 0.5*rho*Vt*Vt;
  sigma = (1 + tanh((alpha - alpha_stall)/blend_width))/2;
  P_frd = omega[1];
  Q_frd = -omega[2];
  R_frd = -omega[3];

  // --- wind-frame axes from velocity (branch-free Gram-Schmidt, cubs2) ---
  wx1 = U/Vt; wx2 = V_frd/Vt; wx3 = W_frd/Vt;
  // reference = body axis most perpendicular to velocity (avoids singularity)
  refx = if abs(wx3) < abs(wx1) then 0 else 1;
  refz = if abs(wx3) < abs(wx1) then 1 else 0;
  rdot = refx*wx1 + refz*wx3;
  wzt1 = refx - rdot*wx1;
  wzt2 = -rdot*wx2;
  wzt3 = refz - rdot*wx3;
  nz = sqrt(wzt1*wzt1 + wzt2*wzt2 + wzt3*wzt3) + eps;
  wz1 = wzt1/nz; wz2 = wzt2/nz; wz3 = wzt3/nz;
  wy1 = wz2*wx3 - wz3*wx2;
  wy2 = wz3*wx1 - wz1*wx3;
  wy3 = wz1*wx2 - wz2*wx1;

  // --- surface deflections ---
  ail_rad = max_defl_ail*min(1, max(-1, ail));
  elev_rad = max_defl_elev*min(1, max(-1, elev));
  rud_rad = -max_defl_rud*min(1, max(-1, rud));  // cubs2 polarity (positive cmd -> +yaw)
  thr_out = min(1, max(0, thr));

  // --- aerodynamic coefficients (stability axes, smooth stall blend) ---
  CL_lin = CL0 + CLa*alpha;
  CL_fp = 2*sin(alpha)*cos(alpha);
  CL = (1 - sigma)*CL_lin + sigma*CL_fp;
  CD_lin = CD0 + k_ind*CL_lin*CL_lin;
  CD_fp = CD0_fp + 2*sin(alpha)*sin(alpha);
  CD = (1 - sigma)*CD_lin + sigma*CD_fp;
  CY_lin = CYb*beta + CYda*ail_rad + CYdr*rud_rad + CYp*(span/(2*Vt))*P_frd + CYr*(span/(2*Vt))*R_frd;
  CY_fp = CY_fp_coef*sin(beta)*cos(alpha);
  CY = (1 - sigma)*CY_lin + sigma*CY_fp;
  Cl_aero = Clda*ail_rad + Cldr*rud_rad + Clb*beta + Clp*(span/(2*Vt))*P_frd + Clr*(span/(2*Vt))*R_frd;
  Cm_aero = Cm0 + Cma*alpha + Cmde*elev_rad + Cmq*(cbar/(2*Vt))*Q_frd;
  Cn_aero = Cnb*beta + Cndr*rud_rad + Cnda*ail_rad + Cnp*(span/(2*Vt))*P_frd + Cnr*(span/(2*Vt))*R_frd;

  // --- wind axes -> body FRD via R_b_wind (columns wx, wy, wz),
  //     FA_wind = qbar*S*{-CD, CY, -CL} ---
  FA_frd[1] = qbar*S*(wx1*(-CD) + wy1*CY + wz1*(-CL));
  FA_frd[2] = qbar*S*(wx2*(-CD) + wy2*CY + wz2*(-CL));
  FA_frd[3] = qbar*S*(wx3*(-CD) + wy3*CY + wz3*(-CL));
  MA_frd[1] = qbar*S*span*Cl_aero;
  MA_frd[2] = qbar*S*cbar*Cm_aero;
  MA_frd[3] = qbar*S*span*Cn_aero;

  // --- body FRD -> body FLU (negate y, z) ---
  F_aero = {FA_frd[1], -FA_frd[2], -FA_frd[3]};
  M_aero = {MA_frd[1], -MA_frd[2], -MA_frd[3]};
  F_thrust = {thr_max*thr_out, 0, 0};

  // --- tricycle landing-gear contact (spring-damper per wheel) ---
  // Contact height uses world-up (R[3,:]); normal force acts along world-up
  // expressed in body (= R[3,:]); friction is applied in the body tangential
  // plane (low forward, firm lateral). Moments are r x F about the CG.
  for i in 1:3 loop
    wh_h[i] = p[3] + R[3, 1]*wheel_x[i] + R[3, 2]*wheel_y[i] + R[3, 3]*wheel_z[i];
    wh_vbx[i] = v_b[1] + omega[2]*wheel_z[i] - omega[3]*wheel_y[i];
    wh_vby[i] = v_b[2] + omega[3]*wheel_x[i] - omega[1]*wheel_z[i];
    wh_vbz[i] = v_b[3] + omega[1]*wheel_y[i] - omega[2]*wheel_x[i];
    wh_vwz[i] = R[3, 1]*wh_vbx[i] + R[3, 2]*wh_vby[i] + R[3, 3]*wh_vbz[i];
    wh_Fn[i] = if wh_h[i] < 0 then max(0, ground_k*(-wh_h[i]) - ground_c*wh_vwz[i]) else 0;
    wh_F[1, i] = wh_Fn[i]*R[3, 1] + (if wh_h[i] < 0 then -roll_fric*wh_vbx[i] else 0);
    wh_F[2, i] = wh_Fn[i]*R[3, 2] + (if wh_h[i] < 0 then -side_fric*wh_vby[i] else 0);
    wh_F[3, i] = wh_Fn[i]*R[3, 3];
    wh_M[1, i] = wheel_y[i]*wh_F[3, i] - wheel_z[i]*wh_F[2, i];
    wh_M[2, i] = wheel_z[i]*wh_F[1, i] - wheel_x[i]*wh_F[3, i];
    wh_M[3, i] = wheel_x[i]*wh_F[2, i] - wheel_y[i]*wh_F[1, i];
  end for;
  F_ground = {wh_F[1, 1] + wh_F[1, 2] + wh_F[1, 3],
              wh_F[2, 1] + wh_F[2, 2] + wh_F[2, 3],
              wh_F[3, 1] + wh_F[3, 2] + wh_F[3, 3]};
  M_ground = {wh_M[1, 1] + wh_M[1, 2] + wh_M[1, 3],
              wh_M[2, 1] + wh_M[2, 2] + wh_M[2, 3],
              wh_M[3, 1] + wh_M[3, 2] + wh_M[3, 3]};

  // --- hand non-gravity wrench to the rigid-body integrator ---
  F_b = F_aero + F_thrust + F_ground;
  M_b = M_aero + M_ground;

  // --- viewer outputs ---
  gyro = omega;
  up_body = {R[3, 1], R[3, 2], R[3, 3]};
  airspeed = Vt;
  alpha_deg = alpha*57.29577951308232;
  position = p;
  velocity = v_w;
  quat = q;
end FixedWingPlant;


model FixedWingFBW "Cascaded attitude-hold fly-by-wire (outer attitude -> inner rate)"
  // OUTER loop — the stick commands an attitude RATE that integrates into a
  // held bank / pitch setpoint (rate-command, attitude-hold). Holding a key
  // keeps rolling / pitching; releasing holds the new attitude. A proportional
  // law on the setpoint error produces body-rate setpoints for the inner loop.
  parameter Real roll_cmd_rate = 1.5 "Full-stick roll-setpoint rate [rad/s]";
  parameter Real pitch_cmd_rate = 0.9 "Full-stick pitch-setpoint rate [rad/s]";
  parameter Real phi_sp_max = 0.90 "Bank setpoint limit [rad] (~52 deg)";
  parameter Real theta_sp_max = 0.45 "Pitch setpoint limit [rad] (~26 deg)";
  parameter Real yaw_rate_max = 1.0 "Full-stick commanded yaw rate [rad/s]";
  parameter Real Kp_phi = 5.0 "Bank-error -> roll-rate gain [1/s]";
  parameter Real Kp_theta = 5.0 "Pitch-error -> pitch-rate gain [1/s]";
  parameter Real p_rate_max = 4.0 "Roll-rate setpoint limit [rad/s]";
  parameter Real q_rate_max = 2.5 "Pitch-rate setpoint limit [rad/s]";

  // INNER loop — body-rate PI (anti-windup). The integral supplies the trim
  // surface deflection, so attitude is held with no steady-state droop.
  parameter Real Kp_p = 0.45 "Roll-rate P [cmd/(rad/s)]";
  parameter Real Ki_p = 0.30 "Roll-rate I";
  parameter Real ilim_p = 1.0 "Roll integrator limit";
  parameter Real Kp_q = 0.55 "Pitch-rate P";
  parameter Real Ki_q = 0.40 "Pitch-rate I";
  parameter Real ilim_q = 1.0 "Pitch integrator limit";
  parameter Real Kp_r = 0.40 "Yaw-rate P";
  parameter Real Ki_r = 0.10 "Yaw-rate I";
  parameter Real ilim_r = 0.6 "Yaw integrator limit";

  // Stall protection — nose-up authority fades out as airspeed approaches the
  // stall, and below the floor the controller actively commands nose-down to
  // trade altitude for speed, so the aircraft sinks rather than departing.
  parameter Real v_prot_lo = 6.5 "Airspeed below which nose-up authority -> 0 [m/s]";
  parameter Real v_prot_hi = 9.0 "Airspeed for full nose-up authority [m/s]";
  parameter Real dive_slope = 0.10 "Protective nose-down angle per m/s of speed deficit [rad/(m/s)]";

  input Real stick_roll(start = 0) "Roll stick [-1..1]";
  input Real stick_pitch(start = 0) "Pitch stick [-1..1]";
  input Real stick_yaw(start = 0) "Yaw stick [-1..1]";
  input Real stick_throttle(start = 0) "Throttle stick [0..1]";
  input Real armed(start = 0) "Arm signal [0/1]";
  input Real gyro[3](start = {0, 0, 0}) "Body rate FLU [rad/s]";
  input Real up_body[3](start = {0, 0, 1}) "World up in body FLU";
  input Real airspeed(start = 15) "True airspeed [m/s]";

  output Real ail "Aileron command [-1..1]";
  output Real elev "Elevator command [-1..1]";
  output Real rud "Rudder command [-1..1]";
  output Real thr "Throttle command [0..1]";

protected
  Real phi "Estimated bank angle [rad]";
  Real theta "Estimated pitch angle [rad]";
  Real p_sp "Roll-rate setpoint [rad/s]";
  Real q_up_sp "Nose-up-rate setpoint [rad/s]";
  Real r_sp "Yaw-rate setpoint [rad/s]";
  Real p_meas "Measured roll rate [rad/s]";
  Real q_up "Measured nose-up rate [rad/s]";
  Real r_meas "Measured yaw rate [rad/s]";
  Real e_p, e_q, e_r "Body-rate errors [rad/s]";
  Real i_p(start = 0, fixed = true);
  Real i_q(start = 0, fixed = true);
  Real i_r(start = 0, fixed = true);
  Real i_p_c, i_q_c, i_r_c "Clamped integrator states";
  Real phi_sp(start = 0, fixed = true) "Held bank setpoint [rad]";
  Real theta_sp(start = 0, fixed = true) "Held pitch setpoint [rad]";
  Real rate_phi "Commanded roll-setpoint rate [rad/s]";
  Real rate_theta "Commanded pitch-setpoint rate [rad/s]";
  Real climb_auth "Nose-up authority [0..1], faded near stall";
  Real theta_eff "Pitch setpoint after protective dive [rad]";

equation
  // --- attitude estimate from world-up-in-body (tilt angles, no singularity
  //     for normal flight). up_body = {fwd, left, up}; level -> {0, 0, 1}.
  phi = atan2(up_body[2], up_body[3]);
  theta = atan2(up_body[1], up_body[3]);

  // --- OUTER loop: rate-command into held setpoints, then setpoint error ->
  //     body-rate setpoints. Nose-up rate fades near stall (so the setpoint
  //     can't wind up nose-high); a protective dive overrides the pitch
  //     setpoint when slow and releases itself once speed recovers.
  climb_auth = min(1, max(0, (airspeed - v_prot_lo)/(v_prot_hi - v_prot_lo)));
  rate_phi = armed*stick_roll*roll_cmd_rate;
  rate_theta = armed*(if stick_pitch > 0 then stick_pitch*pitch_cmd_rate*climb_auth else stick_pitch*pitch_cmd_rate);
  der(phi_sp) = if phi_sp > phi_sp_max then min(0, rate_phi) else if phi_sp < -phi_sp_max then max(0, rate_phi) else rate_phi;
  der(theta_sp) = if theta_sp > theta_sp_max then min(0, rate_theta) else if theta_sp < -theta_sp_max then max(0, rate_theta) else rate_theta;

  theta_eff = if airspeed < v_prot_lo then min(theta_sp, -dive_slope*(v_prot_lo - airspeed)) else theta_sp;

  p_sp = min(p_rate_max, max(-p_rate_max, Kp_phi*(phi_sp - phi)));
  q_up_sp = min(q_rate_max, max(-q_rate_max, Kp_theta*(theta_eff - theta)));
  r_sp = stick_yaw*yaw_rate_max;

  // --- measured rates in matching sign convention (measured open-loop):
  //     +aileron -> roll-right -> gyro[1] > 0;  +elevator -> nose-up ->
  //     gyro[2] < 0 (so nose-up rate q_up = -gyro[2]);  +rudder -> gyro[3] > 0.
  p_meas = gyro[1];
  q_up = -gyro[2];
  r_meas = gyro[3];

  e_p = p_sp - p_meas;
  e_q = q_up_sp - q_up;
  e_r = r_sp - r_meas;

  // --- INNER loop: rate PI with conditional anti-windup, gated by arm ---
  der(i_p) = if i_p > ilim_p then min(0, armed*e_p) else if i_p < -ilim_p then max(0, armed*e_p) else armed*e_p;
  der(i_q) = if i_q > ilim_q then min(0, armed*e_q) else if i_q < -ilim_q then max(0, armed*e_q) else armed*e_q;
  der(i_r) = if i_r > ilim_r then min(0, armed*e_r) else if i_r < -ilim_r then max(0, armed*e_r) else armed*e_r;
  i_p_c = min(ilim_p, max(-ilim_p, i_p));
  i_q_c = min(ilim_q, max(-ilim_q, i_q));
  i_r_c = min(ilim_r, max(-ilim_r, i_r));

  ail = Kp_p*e_p + Ki_p*i_p_c;
  elev = Kp_q*e_q + Ki_q*i_q_c;
  rud = Kp_r*e_r + Ki_r*i_r_c;

  // --- manual throttle, gated by arm ---
  thr = armed*stick_throttle;
end FixedWingFBW;


model FixedWing "Closed-loop fixed-wing (plant + FBW controller)"
  input Real stick_roll(start = 0) "Roll stick [-1..1]";
  input Real stick_pitch(start = 0) "Pitch stick [-1..1]";
  input Real stick_yaw(start = 0) "Yaw stick [-1..1]";
  input Real stick_throttle(start = 0) "Throttle stick [0..1]";
  input Real armed(start = 0) "Arm signal [0/1]";

  output Real position[3] "World position [m]";
  output Real velocity[3] "World velocity [m/s]";
  output Real quat[4] "Quaternion w,x,y,z";
  output Real gyro[3] "Body angular rate FLU [rad/s]";
  output Real airspeed "True airspeed [m/s]";
  output Real alpha_deg "Angle of attack [deg]";
  output Real ail_rad "Aileron deflection [rad]";
  output Real elev_rad "Elevator deflection [rad]";
  output Real rud_rad "Rudder deflection [rad]";
  output Real thr_out "Throttle fraction [0..1]";

protected
  FixedWingPlant vehicle;
  FixedWingFBW controller;

equation
  controller.stick_roll = stick_roll;
  controller.stick_pitch = stick_pitch;
  controller.stick_yaw = stick_yaw;
  controller.stick_throttle = stick_throttle;
  controller.armed = armed;
  controller.gyro = vehicle.gyro;
  controller.up_body = vehicle.up_body;
  controller.airspeed = vehicle.airspeed;

  vehicle.ail = controller.ail;
  vehicle.elev = controller.elev;
  vehicle.rud = controller.rud;
  vehicle.thr = controller.thr;

  position = vehicle.position;
  velocity = vehicle.velocity;
  quat = vehicle.quat;
  gyro = vehicle.gyro;
  airspeed = vehicle.airspeed;
  alpha_deg = vehicle.alpha_deg;
  ail_rad = vehicle.ail_rad;
  elev_rad = vehicle.elev_rad;
  rud_rad = vehicle.rud_rad;
  thr_out = vehicle.thr_out;
end FixedWing;
