within Filter;

function kalmanFilterStep "One discrete-time linear Kalman filter step"
  import Modelica.Math.Matrices;
  input Real A[:, :] "State transition matrix";
  input Real B[:, :] "Input matrix";
  input Real C[:, :] "Measurement matrix";
  input Real Q[:, :] "Process noise covariance";
  input Real R[:, :] "Measurement noise covariance";
  input Real u[size(B, 2)] "Control input";
  input Real y[size(C, 1)] "Measurement";
  input Real x_prev[size(A, 1)] "Previous state estimate";
  input Real P_prev[size(A, 1), size(A, 1)] "Previous covariance estimate";
  output Real x_next[size(A, 1)] "Updated state estimate";
  output Real P_next[size(A, 1), size(A, 1)] "Updated covariance estimate";
protected
  Integer n = size(A, 1);
  Integer p = size(C, 1);
  Real x_pred[n];
  Real P_pred[n, n];
  Real S[p, p];
  Real K[n, p];
  Real I[n, n] = identity(n);
  Real innovation[p];
algorithm
  for i in 1:n loop
    for j in 1:n loop
      I[i, j] := if i == j then 1.0 else 0.0;
    end for;
  end for;
  x_pred := A*x_prev + B*u;
  P_pred := A*P_prev*transpose(A) + Q;
  innovation := y - C*x_pred;
  S := C*P_pred*transpose(C) + R;
  K := P_pred*transpose(C)*Matrices.inv(S);
  x_next := x_pred + K*innovation;
  P_next := (I - K*C)*P_pred;
end kalmanFilterStep;

function testKalmanFilterStep
  output Boolean ok;
protected
  constant Real tol = 1e-10;
  constant Real A[1, 1] = [1.0];
  constant Real B[1, 1] = [1.0];
  constant Real C[1, 1] = [1.0];
  constant Real Q[1, 1] = [0.1];
  constant Real R[1, 1] = [0.2];
  constant Real u[1] = {1.0};
  constant Real y[1] = {1.2};
  constant Real x_prev[1] = {0.0};
  constant Real P_prev[1, 1] = [1.0];
  constant Real expected_x = 1.1692307692307693;
  constant Real expected_P = 0.16923076923076924;
  Real x_next[1];
  Real P_next[1, 1];
algorithm
  (x_next, P_next) := kalmanFilterStep(A, B, C, Q, R, u, y, x_prev, P_prev);
  assert(abs(x_next[1] - expected_x) < tol, "kalmanFilterStep test failed: x_next mismatch");
  assert(abs(P_next[1, 1] - expected_P) < tol, "kalmanFilterStep test failed: P_next mismatch");
  ok := true;
end testKalmanFilterStep;

model KalmanFilterStepTest
  Real x;
initial algorithm
  assert(not testKalmanFilterStep(), "kalmanFilterStep unit test failed");
equation
  der(x) = 1;
end KalmanFilterStepTest;