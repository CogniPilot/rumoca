within LieGroups.SE23.Quat;
function left_jacobian "Forward left Jacobian J_l of SE_2(3) (9x9)"
  input Real xi[9] "Lie algebra {vb, ab, omega}";
  output Real J[9,9] "9x9 left Jacobian";
protected
  Real Jl[3,3] "SO(3) left Jacobian of omega";
  Real Qv[3,3] "Q block for velocity-column part vb";
  Real Qa[3,3] "Q block for acceleration-column part ab";
  Real omega[3];
algorithm
  omega := xi[7:9];
  Jl := LieGroups.SO3.Quat.left_jacobian(omega);
  Qv := LieGroups.SE3.Quat.left_Q(xi[1:3], omega);
  Qa := LieGroups.SE3.Quat.left_Q(xi[4:6], omega);

  // Block structure (ordering {vb, ab, omega}):
  // J = {{Jl, 0, Qv}, {0, Jl, Qa}, {0, 0, Jl}}
  for i in 1:9 loop
    for j in 1:9 loop
      J[i,j] := 0.0;
    end for;
  end for;
  for i in 1:3 loop
    for j in 1:3 loop
      J[i,j]     := Jl[i,j];   J[i,j+6]   := Qv[i,j];
      J[i+3,j+3] := Jl[i,j];   J[i+3,j+6] := Qa[i,j];
      J[i+6,j+6] := Jl[i,j];
    end for;
  end for;

  annotation(Documentation(info="<html>
    <p>Forward left Jacobian of SE_2(3), matching cyecca <code>se23.left_jacobian</code>.</p>
    <p>Used by log-linear dynamic-inversion control: n = nbar - J_l(xi) BK xi.</p>
  </html>"));
end left_jacobian;
