within LieGroups.SE23.Quat;
function small_adjoint "Small adjoint ad_xi for se_2(3)"
  input Real xi[9] "Lie algebra {vb, ab, omega}";
  output Real ad[9,9] "9x9 small adjoint matrix";
protected
  Real vx[3,3] "Skew [vb]x";
  Real ax[3,3] "Skew [ab]x";
  Real wx[3,3] "Skew [omega]x";
algorithm
  vx := LieGroups.SO3.Quat.wedge(xi[1:3]);
  ax := LieGroups.SO3.Quat.wedge(xi[4:6]);
  wx := LieGroups.SO3.Quat.wedge(xi[7:9]);

  // ad = {{[omega]x, 0, [vb]x}, {0, [omega]x, [ab]x}, {0, 0, [omega]x}}
  for i in 1:3 loop
    for j in 1:3 loop
      ad[i,j]     := wx[i,j];   ad[i,j+3]   := 0;        ad[i,j+6]   := vx[i,j];
      ad[i+3,j]   := 0;         ad[i+3,j+3] := wx[i,j];  ad[i+3,j+6] := ax[i,j];
      ad[i+6,j]   := 0;         ad[i+6,j+3] := 0;         ad[i+6,j+6] := wx[i,j];
    end for;
  end for;
end small_adjoint;
