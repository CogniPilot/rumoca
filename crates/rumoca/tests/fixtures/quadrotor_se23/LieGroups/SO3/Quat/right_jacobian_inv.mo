within LieGroups.SO3.Quat;
function right_jacobian_inv "Inverse right Jacobian J_r^{-1}(v) of SO(3)"
  input Real v[3] "Rotation vector";
  output Real J_inv[3,3] "3x3 inverse right Jacobian matrix";
protected
  Real theta_sq;
  Real C "1/theta^2 + sin(theta)/(2*theta*(cos(theta)-1))";
  Real theta;
  Real S[3,3] "Skew-symmetric [v]x";
  Real S2[3,3] "[v]x * [v]x";
  constant Real eps = 1e-8;
algorithm
  theta_sq := v[1]^2 + v[2]^2 + v[3]^2;

  S := {{0, -v[3], v[2]},
        {v[3], 0, -v[1]},
        {-v[2], v[1], 0}};

  S2 := {{-(v[2]^2 + v[3]^2), v[1]*v[2], v[1]*v[3]},
         {v[1]*v[2], -(v[1]^2 + v[3]^2), v[2]*v[3]},
         {v[1]*v[3], v[2]*v[3], -(v[1]^2 + v[2]^2)}};

  if theta_sq < eps then
    C := 1.0/12.0 + theta_sq / 720.0;
  else
    theta := sqrt(theta_sq);
    C := 1.0 / theta_sq + sin(theta) / (2.0 * theta * (cos(theta) - 1.0));
  end if;

  // J_r^{-1} = I + 0.5 * [v]x + C * [v]x^2  (note: plus sign on 0.5 term vs left inv)
  for i in 1:3 loop
    for j in 1:3 loop
      J_inv[i,j] := (if i == j then 1.0 else 0.0) + 0.5 * S[i,j] + C * S2[i,j];
    end for;
  end for;

  annotation(Documentation(info="<html>
    <p>Inverse right Jacobian of SO(3): J_r^{-1}(v) = I + ½[v]× + C·[v]×²</p>
    <p>Related to inverse left Jacobian by: J_r^{-1}(v) = J_l^{-1}(-v)</p>
  </html>"));
end right_jacobian_inv;
