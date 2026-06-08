within LieGroups.SO3.Dcm;
function log_map "Logarithmic map: DCM -> so(3) rotation vector"
  input Real R[3,3] "Rotation matrix";
  output Real v[3] "Rotation vector";
protected
  Real tr;
  Real cos_theta;
  Real theta;
  Real A "theta / (2*sin(theta))";
  constant Real eps = 1e-8;
algorithm
  tr := R[1,1] + R[2,2] + R[3,3];
  cos_theta := min(max((tr - 1.0) / 2.0, -1.0), 1.0);
  theta := acos(cos_theta);

  if abs(theta) < eps then
    // Near identity: A ~ 0.5 + theta^2/12
    A := 0.5 + theta^2 / 12.0;
  else
    A := theta / (2.0 * sin(theta));
  end if;

  // v = A * vee(R - R^T)
  v[1] := A * (R[3,2] - R[2,3]);
  v[2] := A * (R[1,3] - R[3,1]);
  v[3] := A * (R[2,1] - R[1,2]);
end log_map;
