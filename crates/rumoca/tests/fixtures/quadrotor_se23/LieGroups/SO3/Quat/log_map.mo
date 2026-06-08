within LieGroups.SO3.Quat;
function log_map "Logarithmic map: unit quaternion -> so(3) rotation vector"
  input Real q[4] "Unit quaternion {w,x,y,z}";
  output Real v[3] "Rotation vector (axis * angle)";
protected
  Real q_n[4];
  Real qw;
  Real vec_norm;
  Real theta;
  Real A;
  constant Real eps = 1e-10;
algorithm
  // Normalize input
  q_n := LieGroups.SO3.Quat.normalize(q);

  // Ensure positive scalar part (shortest path). Written as an if-EXPRESSION
  // (not a no-else if-statement) so AD/both-branch compilers evaluate it correctly.
  q_n := if q_n[1] < 0 then -q_n else q_n;

  qw := min(max(q_n[1], -1.0), 1.0);
  vec_norm := sqrt(q_n[2]^2 + q_n[3]^2 + q_n[4]^2);

  if vec_norm < eps then
    // Near identity: theta ~ 2*||qvec||, so v ~ 2*qvec
    v[1] := 2.0 * q_n[2];
    v[2] := 2.0 * q_n[3];
    v[3] := 2.0 * q_n[4];
  else
    // theta = 2*atan2(||qvec||, qw): well-conditioned for all angles
    // (acos(qw) loses precision near identity, where qw -> 1).
    theta := 2.0 * atan2(vec_norm, qw);
    // NaN-safe: this branch is only selected for vec_norm >= eps, so max(vec_norm, eps)
    // is exact when taken, but keeps A finite under both-branch/AD evaluation at vec_norm = 0.
    A := theta / max(vec_norm, eps);
    v[1] := A * q_n[2];
    v[2] := A * q_n[3];
    v[3] := A * q_n[4];
  end if;
end log_map;
