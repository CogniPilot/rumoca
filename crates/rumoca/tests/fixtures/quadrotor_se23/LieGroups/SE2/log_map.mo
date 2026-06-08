within LieGroups.SE2;
function log_map "Logarithmic map: SE(2) -> se(2)"
  input Real X[3] "Group element {px, py, theta}";
  output Real xi[3] "Lie algebra {vx, vy, omega}";
protected
  Real theta;
  Real a "sin(theta)/theta";
  Real b "(1-cos(theta))/theta";
  Real det_V;
  constant Real eps = 1e-8;
algorithm
  theta := X[3];

  if abs(theta) < eps then
    a := 1.0 - theta^2 / 6.0;
    b := theta / 2.0 - theta^3 / 24.0;
  else
    a := sin(theta) / theta;
    b := (1.0 - cos(theta)) / theta;
  end if;

  // V_inv = {{a, b}, {-b, a}} / (a^2 + b^2)
  det_V := a^2 + b^2;
  xi[1] := (a*X[1] + b*X[2]) / det_V;
  xi[2] := (-b*X[1] + a*X[2]) / det_V;
  xi[3] := theta;
end log_map;
