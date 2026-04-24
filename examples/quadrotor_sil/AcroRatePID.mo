// Acro-mode rate PID controller for a quadrotor in X-configuration.
//
// Mirrors cerebri's rate-mode stack: stick → desired body-rate → PID on
// gyro error → motor mix. Outputs motor speeds [rad/s] directly; no RC
// channel encoding or FB serialization.
//
// Motor index convention (matches cerebri main.c:85 and rumoca's
// QuadrotorSIL.mo signs):
//   m0: front-right, CCW     m1: rear-right, CW
//   m2: rear-left,  CCW      m3: front-left, CW

model AcroRatePID

  // --- Rate setpoint limits (match cerebri main.c:790-796) ---
  parameter Real rate_rp_max = 6.0  "Full-stick roll/pitch rate [rad/s]";
  parameter Real rate_y_max  = 3.5  "Full-stick yaw rate [rad/s]";

  // --- Rate PID gains ---
  // Gains are in rad/s of motor-omega differential per rad/s of rate error
  // (since roll_out/pitch_out/yaw_out feed directly into the motor omega
  // mix). A rough ballpark: for this 2 kg plant with Ct=8.55e-6 and
  // arm=0.25 m, a motor-omega differential of ~100 rad/s produces ~0.3 Nm
  // of body torque, i.e. ~14 rad/s² angular accel. To hit full-stick rate
  // (6 rad/s) in ~50 ms we need ~120 rad/s² peak, i.e. roll_out ~150 at
  // error 6 → Kp ≈ 25.
  parameter Real Kp_rate_rp = 25.0;
  parameter Real Ki_rate_rp = 5.0;
  parameter Real Kd_rate_rp = 0.0;
  parameter Real Kp_rate_y  = 30.0;
  parameter Real Ki_rate_y  = 5.0;
  parameter Real Kd_rate_y  = 0.0;

  // Integral clamps (anti-windup). Scaled with the bigger Kp so the i-term
  // can still push the same motor-omega range (~200 rad/s).
  parameter Real i_lim_rp = 40;
  parameter Real i_lim_y  = 30;

  // --- Throttle scaling ---
  // stick_throttle in [0,1]. At stick = 1 we command omega_hover +
  // throttle_span. throttle_hover sits the drone at ~mass*g/(4*Ct)^0.5.
  // For mass=2 kg, g=9.8, Ct=8.55e-6: hover = sqrt(2*9.8/(4*8.55e-6)) ≈ 757.
  parameter Real omega_hover = 757  "Per-motor omega at stick = 0.5 [rad/s]";
  parameter Real throttle_span = 750 "Per-motor omega range around hover";

  // --- Inputs: pilot commands (from input engine) ---
  input Real stick_roll(start = 0)     "Stick roll [-1..1]";
  input Real stick_pitch(start = 0)    "Stick pitch [-1..1]";
  input Real stick_yaw(start = 0)      "Stick yaw [-1..1]";
  input Real stick_throttle(start = 0) "Stick throttle [0..1]";
  input Real armed(start = 0)          "Arm signal [0 disarmed, 1 armed]";

  // --- Inputs: plant feedback ---
  input Real gyro_x(start = 0) "Body roll rate [rad/s]";
  input Real gyro_y(start = 0) "Body pitch rate [rad/s]";
  input Real gyro_z(start = 0) "Body yaw rate [rad/s]";

  // --- Outputs: per-motor commanded speed [rad/s] ---
  output Real motor_cmd_0;  // FR
  output Real motor_cmd_1;  // RR
  output Real motor_cmd_2;  // RL
  output Real motor_cmd_3;  // FL

  // --- Internal: rate setpoints, errors, integrators ---
  Real rate_sp_x "Roll rate setpoint";
  Real rate_sp_y "Pitch rate setpoint";
  Real rate_sp_z "Yaw rate setpoint";
  Real e_x "Roll rate error";
  Real e_y "Pitch rate error";
  Real e_z "Yaw rate error";
  // Unclamped integral states. We write them as simple integrators and
  // apply the clamp at the readout — avoids if-else inside der(), which
  // trips the stiff solver (same issue that bit the motor model).
  Real i_x(start = 0);
  Real i_y(start = 0);
  Real i_z(start = 0);
  Real i_x_clamped;
  Real i_y_clamped;
  Real i_z_clamped;
  // PID outputs (in motor-omega units).
  Real roll_out  "Roll control effort";
  Real pitch_out "Pitch control effort";
  Real yaw_out   "Yaw control effort";
  Real base_throttle "Per-motor baseline [rad/s]";

equation
  // Direct stick-to-rate: no extra inversions. Our gilrs RightStickY
  // convention already returns negative when pushed forward, which is
  // the negative pitch rate needed for nose-down / forward flight.
  // (Cerebri has an extra -1 in main.c:791 only because its RC pipeline
  // flips pitch via the rc.1 derive — we don't go through rc channels.)
  rate_sp_x = stick_roll  * rate_rp_max;
  rate_sp_y = stick_pitch * rate_rp_max;
  rate_sp_z = stick_yaw   * rate_y_max;

  e_x = rate_sp_x - gyro_x;
  e_y = rate_sp_y - gyro_y;
  e_z = rate_sp_z - gyro_z;

  // When disarmed, freeze integrators (no windup while sticks drift).
  der(i_x) = armed * e_x;
  der(i_y) = armed * e_y;
  der(i_z) = armed * e_z;

  // Clamp by smooth saturation — avoid branchy if-else.
  i_x_clamped = i_lim_rp * tanh(i_x / i_lim_rp);
  i_y_clamped = i_lim_rp * tanh(i_y / i_lim_rp);
  i_z_clamped = i_lim_y  * tanh(i_z / i_lim_y);

  roll_out  = Kp_rate_rp * e_x + Ki_rate_rp * i_x_clamped;
  pitch_out = Kp_rate_rp * e_y + Ki_rate_rp * i_y_clamped;
  yaw_out   = Kp_rate_y  * e_z + Ki_rate_y  * i_z_clamped;

  base_throttle = omega_hover + throttle_span * (stick_throttle - 0.5) * 2;

  // X-mix: same signs as cerebri main.c:97-102.
  // m0 (FR): roll=-1 pitch=+1 yaw=+1
  // m1 (RR): roll=-1 pitch=-1 yaw=-1
  // m2 (RL): roll=+1 pitch=-1 yaw=+1
  // m3 (FL): roll=+1 pitch=+1 yaw=-1
  // Arm gate applied as a 0/1 multiplier so the whole stack collapses
  // to zero omega when disarmed.
  motor_cmd_0 = armed * (base_throttle - roll_out + pitch_out + yaw_out);
  motor_cmd_1 = armed * (base_throttle - roll_out - pitch_out - yaw_out);
  motor_cmd_2 = armed * (base_throttle + roll_out - pitch_out + yaw_out);
  motor_cmd_3 = armed * (base_throttle + roll_out + pitch_out - yaw_out);

end AcroRatePID;
