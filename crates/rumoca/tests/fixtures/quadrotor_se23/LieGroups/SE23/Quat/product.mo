within LieGroups.SE23.Quat;
function product "SE_2(3) group product"
  input Real X1[10] "{p1, v1, q1}";
  input Real X2[10] "{p2, v2, q2}";
  output Real X[10] "{p, v, q}";
protected
  Real p_rot[3], v_rot[3];
  Real q_prod[4];
algorithm
  // p = R(q1)*p2 + p1
  p_rot := LieGroups.SO3.Quat.rotate(X1[7:10], X2[1:3]);
  X[1] := p_rot[1] + X1[1];
  X[2] := p_rot[2] + X1[2];
  X[3] := p_rot[3] + X1[3];

  // v = R(q1)*v2 + v1
  v_rot := LieGroups.SO3.Quat.rotate(X1[7:10], X2[4:6]);
  X[4] := v_rot[1] + X1[4];
  X[5] := v_rot[2] + X1[5];
  X[6] := v_rot[3] + X1[6];

  // q = q1 * q2
  q_prod := LieGroups.SO3.Quat.product(X1[7:10], X2[7:10]);
  X[7] := q_prod[1];
  X[8] := q_prod[2];
  X[9] := q_prod[3];
  X[10] := q_prod[4];
end product;
