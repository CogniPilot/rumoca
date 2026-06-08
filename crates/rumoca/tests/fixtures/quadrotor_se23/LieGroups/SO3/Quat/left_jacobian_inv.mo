within LieGroups.SO3.Quat;
function left_jacobian_inv "Inverse left Jacobian J_l^{-1}(v) of SO(3)"
  input Real v[3] "Rotation vector";
  output Real J_inv[3,3] "3x3 inverse left Jacobian matrix";
protected
  Real theta_sq;
  Real C "1/theta^2 + sin(theta)/(2*theta*(cos(theta)-1))";
  Real theta;
  Real S[3,3] "Skew-symmetric [v]x";
  Real S2[3,3] "[v]x * [v]x";
  constant Real eps = 1e-8;
algorithm
  theta_sq := v[1]^2 + v[2]^2 + v[3]^2;

  // Skew-symmetric matrix [v]x
  S := {{0, -v[3], v[2]},
        {v[3], 0, -v[1]},
        {-v[2], v[1], 0}};

  // [v]x^2
  S2 := {{-(v[2]^2 + v[3]^2), v[1]*v[2], v[1]*v[3]},
         {v[1]*v[2], -(v[1]^2 + v[3]^2), v[2]*v[3]},
         {v[1]*v[3], v[2]*v[3], -(v[1]^2 + v[2]^2)}};

  if theta_sq < eps then
    // Taylor series: 1/t^2 + sin(t)/(2t(cos(t)-1)) ~ 1/12 + t^2/720
    C := 1.0/12.0 + theta_sq / 720.0;
  else
    // NaN-safe: max(theta_sq, eps) is exact when this branch is taken (theta_sq >= eps)
    // and keeps the closed form finite under both-branch/AD evaluation at theta_sq = 0.
    theta := sqrt(max(theta_sq, eps));
    C := 1.0 / (theta * theta) + sin(theta) / (2.0 * theta * (cos(theta) - 1.0));
  end if;

  // J_l^{-1} = I - 0.5 * [v]x + C * [v]x^2
  for i in 1:3 loop
    for j in 1:3 loop
      J_inv[i,j] := (if i == j then 1.0 else 0.0) - 0.5 * S[i,j] + C * S2[i,j];
    end for;
  end for;

  annotation(Documentation(info="<html>
    <p>Inverse left Jacobian of SO(3): J_l^{-1}(v) = I - ½[v]× + C·[v]×²</p>
    <p>where C = 1/θ² + sin(θ)/(2θ(cos(θ)-1))</p>
    <p>Used in the SE(3) logarithm map: V(ω)^{-1} recovers translation from the group element.</p>
  </html>"));
end left_jacobian_inv;
