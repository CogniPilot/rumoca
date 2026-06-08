within LieGroups.SE3.Quat;
function exp_map "Exponential map: se(3) -> SE(3)"
  input Real xi[6] "Lie algebra {vx,vy,vz, omega_x,omega_y,omega_z}";
  output Real X[7] "Group element {px,py,pz, qw,qx,qy,qz}";
protected
  Real v[3] "Translational component";
  Real omega[3] "Rotational component";
  Real J[3,3] "SO(3) left Jacobian";
  Real q[4] "Rotation quaternion";
algorithm
  v := xi[1:3];
  omega := xi[4:6];

  // p = J_l(omega) * v
  J := LieGroups.SO3.Quat.left_jacobian(omega);
  X[1] := J[1,1]*v[1] + J[1,2]*v[2] + J[1,3]*v[3];
  X[2] := J[2,1]*v[1] + J[2,2]*v[2] + J[2,3]*v[3];
  X[3] := J[3,1]*v[1] + J[3,2]*v[2] + J[3,3]*v[3];

  // q = exp_SO3(omega)
  q := LieGroups.SO3.Quat.exp_map(omega);
  X[4] := q[1];
  X[5] := q[2];
  X[6] := q[3];
  X[7] := q[4];
end exp_map;
