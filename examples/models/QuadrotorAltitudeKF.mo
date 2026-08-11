model QuadrotorAltitudeKF
  "Discrete-time altitude/climb-rate Kalman filter for a quadrotor.

  Clocked discrete-time Modelica (no auto-discretization, SPEC_0034 D12):
  the linearized hover dynamics at the sample rate are part of the filter
  design. State x = [z; vz], input u = commanded vertical acceleration,
  measurements y = [z_meas; vz_meas] (C = I). The gain solve uses
  Modelica.Math.Matrices.solve, which the GALEC projection maps to the
  solveLinearEquations builtin (D13) - DoStep declares the
  SOLVE_LINEAR_EQUATIONS_FAILED escape (GAL-029). The covariance is
  initialized by a computed initial equation (GAL-028). Exports via
  --target galec / embedded-c-galec / embedded-rust-galec."
  constant Real samplePeriod = 0.02;
  parameter Real q_accel = 0.05 "Process noise (acceleration) variance";
  parameter Real r_alt = 0.04 "Altitude measurement variance";
  parameter Real r_vel = 0.09 "Climb-rate measurement variance";
  parameter Real p0 = 1.0 "Initial covariance";
  input Real u "Vertical acceleration input [m/s2]";
  input Real z_meas "Measured altitude [m]";
  input Real vz_meas "Measured climb rate [m/s]";
  discrete output Real z_hat(start = 0.0) "Estimated altitude [m]";
  discrete output Real vz_hat(start = 0.0) "Estimated climb rate [m/s]";
  discrete Real P[2, 2] "Covariance estimate";
  discrete Real x_pred[2];
  discrete Real P_pred[2, 2];
  discrete Real S[2, 2] "Innovation covariance";
  discrete Real K_row1[2] "Kalman gain row 1";
  discrete Real K_row2[2] "Kalman gain row 2";
protected
  parameter Real A[2, 2] = [1.0, samplePeriod; 0.0, 1.0];
  parameter Real B[2] = {samplePeriod*samplePeriod/2.0, samplePeriod};
  parameter Real Q[2, 2] = [
    q_accel*samplePeriod*samplePeriod, 0.0;
    0.0, q_accel];
  parameter Real R[2, 2] = [r_alt, 0.0; 0.0, r_vel];
initial equation
  P = p0*identity(2);
equation
  when sample(0.0, samplePeriod) then
    // Prediction.
    x_pred = A*{pre(z_hat), pre(vz_hat)} + B*u;
    P_pred = A*pre(P)*transpose(A) + Q;
    // Innovation covariance (C = I).
    S = P_pred + R;
    // Gain: row j of K = solve(S, column j of P_pred) since
    // K = P_pred*inv(S) with symmetric S and P_pred.
    K_row1 = Modelica.Math.Matrices.solve(S, {P_pred[1, 1], P_pred[2, 1]});
    K_row2 = Modelica.Math.Matrices.solve(S, {P_pred[1, 2], P_pred[2, 2]});
    // Measurement update.
    z_hat = x_pred[1] + K_row1[1]*(z_meas - x_pred[1]) + K_row1[2]*(vz_meas - x_pred[2]);
    vz_hat = x_pred[2] + K_row2[1]*(z_meas - x_pred[1]) + K_row2[2]*(vz_meas - x_pred[2]);
    // Covariance update: P = (I - K*C)*P_pred with C = I.
    P = (identity(2) - [K_row1[1], K_row1[2]; K_row2[1], K_row2[2]])*P_pred;
  end when;
end QuadrotorAltitudeKF;
