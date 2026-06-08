within LieGroups.SO3.Dcm;
function exp_map "Exponential map: so(3) vector -> DCM via Rodriguez formula"
  input Real v[3] "Rotation vector (axis * angle)";
  output Real R[3,3] "Rotation matrix";
protected
  Real theta_sq, theta;
  Real A "sin(theta)/theta";
  Real B "(1 - cos(theta))/theta^2";
  Real S[3,3] "Skew [v]x";
  Real S2[3,3] "[v]x^2";
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
    A := 1.0 - theta_sq / 6.0;
    B := 0.5 - theta_sq / 24.0;
  else
    theta := sqrt(theta_sq);
    A := sin(theta) / theta;
    B := (1.0 - cos(theta)) / theta_sq;
  end if;

  // R = I + A*[v]x + B*[v]x^2
  for i in 1:3 loop
    for j in 1:3 loop
      R[i,j] := (if i == j then 1.0 else 0.0) + A * S[i,j] + B * S2[i,j];
    end for;
  end for;
end exp_map;
