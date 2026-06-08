within LieGroups.SE3.Quat;
function inverse "SE(3) inverse"
  input Real X[7] "{p, q}";
  output Real X_inv[7] "{p_inv, q_inv}";
protected
  Real q_inv[4];
  Real p_rot[3];
algorithm
  q_inv := LieGroups.SO3.Quat.inverse(X[4:7]);
  // p_inv = -R^T * p = -R(q_inv) * p
  p_rot := LieGroups.SO3.Quat.rotate(q_inv, X[1:3]);
  X_inv[1] := -p_rot[1];
  X_inv[2] := -p_rot[2];
  X_inv[3] := -p_rot[3];
  X_inv[4] := q_inv[1];
  X_inv[5] := q_inv[2];
  X_inv[6] := q_inv[3];
  X_inv[7] := q_inv[4];
end inverse;
