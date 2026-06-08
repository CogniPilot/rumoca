within LieGroups.SO3.Quat;
function left_jacobian "Left Jacobian J_l(v) of SO(3)"
  input Real v[3] "Rotation vector";
  output Real J[3,3] "3x3 left Jacobian matrix";
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

  // Skew-symmetric matrix [v]x
  S := {{0, -v[3], v[2]},
        {v[3], 0, -v[1]},
        {-v[2], v[1], 0}};

  // [v]x^2
  S2 := {{-(v[2]^2 + v[3]^2), v[1]*v[2], v[1]*v[3]},
         {v[1]*v[2], -(v[1]^2 + v[3]^2), v[2]*v[3]},
         {v[1]*v[3], v[2]*v[3], -(v[1]^2 + v[2]^2)}};

  if theta_sq < eps then
    // Taylor series: (1-cos t)/t^2 ~ 1/2 - t^2/24
    A := 0.5 - theta_sq / 24.0;
    // Taylor series: (t-sin t)/t^3 ~ 1/6 - t^2/120
    B := 1.0/6.0 - theta_sq / 120.0;
  else
    // NaN-safe denominator: this branch is only selected for theta_sq >= eps, so
    // max(theta_sq, eps) is exact when taken, but keeps the closed form finite when
    // an AD/both-branch evaluator (e.g. rumoca) evaluates it at theta_sq = 0.
    theta := sqrt(max(theta_sq, eps));
    A := (1.0 - cos(theta)) / (theta * theta);
    B := (theta - sin(theta)) / (theta * theta * theta);
  end if;

  // J_l = I + A * [v]x + B * [v]x^2
  for i in 1:3 loop
    for j in 1:3 loop
      J[i,j] := (if i == j then 1.0 else 0.0) + A * S[i,j] + B * S2[i,j];
    end for;
  end for;

  annotation(Documentation(info="<html>
    <p>Left Jacobian of SO(3): J_l(v) = I + ((1-cos θ)/θ²)[v]× + ((θ-sin θ)/θ³)[v]×²</p>
    <p>Relates body-frame perturbations to group perturbations:
    Exp((v + δv)^) ≈ Exp(v^) * Exp((J_l(v) δv)^)</p>
    <p>Used in the V(ω) matrix for SE(3) exponential map.</p>
  </html>"));
end left_jacobian;
