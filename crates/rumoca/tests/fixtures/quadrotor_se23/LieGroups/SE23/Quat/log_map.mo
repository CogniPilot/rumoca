within LieGroups.SE23.Quat;
function log_map "Logarithmic map: SE_2(3) -> se_2(3)"
  input Real X[10] "Group element {p, v, q}";
  output Real xi[9] "Lie algebra {vb, ab, omega}";
protected
  Real omega[3];
  Real theta_sq;
  Real J_inv[3,3];
  constant Real eps = 1e-8;
algorithm
  // omega = log_SO3(q)
  omega := LieGroups.SO3.Quat.log_map(X[7:10]);
  xi[7] := omega[1];
  xi[8] := omega[2];
  xi[9] := omega[3];

  theta_sq := omega[1]^2 + omega[2]^2 + omega[3]^2;

  if theta_sq < eps then
    // Near identity: V_inv ~ I
    // vb = p, ab = v
    xi[1] := X[1]; xi[2] := X[2]; xi[3] := X[3];
    xi[4] := X[4]; xi[5] := X[5]; xi[6] := X[6];
  else
    J_inv := LieGroups.SO3.Quat.left_jacobian_inv(omega);
    // vb = J_l^{-1} * p
    xi[1] := J_inv[1,1]*X[1] + J_inv[1,2]*X[2] + J_inv[1,3]*X[3];
    xi[2] := J_inv[2,1]*X[1] + J_inv[2,2]*X[2] + J_inv[2,3]*X[3];
    xi[3] := J_inv[3,1]*X[1] + J_inv[3,2]*X[2] + J_inv[3,3]*X[3];
    // ab = J_l^{-1} * v
    xi[4] := J_inv[1,1]*X[4] + J_inv[1,2]*X[5] + J_inv[1,3]*X[6];
    xi[5] := J_inv[2,1]*X[4] + J_inv[2,2]*X[5] + J_inv[2,3]*X[6];
    xi[6] := J_inv[3,1]*X[4] + J_inv[3,2]*X[5] + J_inv[3,3]*X[6];
  end if;
end log_map;
