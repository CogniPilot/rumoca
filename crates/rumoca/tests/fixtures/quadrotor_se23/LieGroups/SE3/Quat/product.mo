within LieGroups.SE3.Quat;
function product "SE(3) group product"
  input Real X1[7] "{p1, q1} = {px,py,pz, qw,qx,qy,qz}";
  input Real X2[7] "{p2, q2}";
  output Real X[7] "{p, q}";
protected
  Real p_rot[3];
  Real q_prod[4];
algorithm
  // p = R(q1)*p2 + p1
  p_rot := LieGroups.SO3.Quat.rotate(X1[4:7], X2[1:3]);
  X[1] := p_rot[1] + X1[1];
  X[2] := p_rot[2] + X1[2];
  X[3] := p_rot[3] + X1[3];

  // q = q1 * q2
  q_prod := LieGroups.SO3.Quat.product(X1[4:7], X2[4:7]);
  X[4] := q_prod[1];
  X[5] := q_prod[2];
  X[6] := q_prod[3];
  X[7] := q_prod[4];
end product;
