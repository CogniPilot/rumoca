within LieGroups.SO3.Quat;
function right_jacobian "Right Jacobian J_r(v) of SO(3)"
  input Real v[3] "Rotation vector";
  output Real J[3,3] "3x3 right Jacobian matrix";
protected
  Real theta_sq;
  Real A "(1 - cos(theta)) / theta^2";
  Real B "(theta - sin(theta)) / theta^3";
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
    A := 0.5 - theta_sq / 24.0;
    B := 1.0/6.0 - theta_sq / 120.0;
  else
    theta := sqrt(theta_sq);
    A := (1.0 - cos(theta)) / theta_sq;
    B := (theta - sin(theta)) / (theta_sq * theta);
  end if;

  // J_r = I - A * [v]x + B * [v]x^2  (note: minus sign on A term vs left Jacobian)
  for i in 1:3 loop
    for j in 1:3 loop
      J[i,j] := (if i == j then 1.0 else 0.0) - A * S[i,j] + B * S2[i,j];
    end for;
  end for;

  annotation(Documentation(info="<html>
    <p>Right Jacobian of SO(3): J_r(v) = I - ((1-cos θ)/θ²)[v]× + ((θ-sin θ)/θ³)[v]×²</p>
    <p>Related to left Jacobian by: J_r(v) = J_l(-v)</p>
  </html>"));
end right_jacobian;
