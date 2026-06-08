within LieGroups.SE23.Quat;
function inverse "SE_2(3) inverse"
  input Real X[10] "{p, v, q}";
  output Real X_inv[10] "{p_inv, v_inv, q_inv}";
protected
  Real q_inv[4];
  Real p_rot[3], v_rot[3];
algorithm
  q_inv := LieGroups.SO3.Quat.inverse(X[7:10]);

  // p_inv = -R^{-1} * p
  p_rot := LieGroups.SO3.Quat.rotate(q_inv, X[1:3]);
  X_inv[1] := -p_rot[1];
  X_inv[2] := -p_rot[2];
  X_inv[3] := -p_rot[3];

  // v_inv = -R^{-1} * v
  v_rot := LieGroups.SO3.Quat.rotate(q_inv, X[4:6]);
  X_inv[4] := -v_rot[1];
  X_inv[5] := -v_rot[2];
  X_inv[6] := -v_rot[3];

  X_inv[7] := q_inv[1];
  X_inv[8] := q_inv[2];
  X_inv[9] := q_inv[3];
  X_inv[10] := q_inv[4];
end inverse;
