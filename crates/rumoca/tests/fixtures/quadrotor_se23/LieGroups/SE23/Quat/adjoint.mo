within LieGroups.SE23.Quat;
function adjoint "Adjoint Ad_X for SE_2(3)"
  input Real X[10] "{p, v, q}";
  output Real Ad[9,9] "9x9 adjoint matrix";
protected
  Real R[3,3];
  Real px[3,3] "Skew [p]x";
  Real vx[3,3] "Skew [v]x";
  Real pR[3,3] "[p]x * R";
  Real vR[3,3] "[v]x * R";
algorithm
  R := LieGroups.SO3.Quat.to_DCM(X[7:10]);
  px := LieGroups.SO3.Quat.wedge(X[1:3]);
  vx := LieGroups.SO3.Quat.wedge(X[4:6]);

  // [p]x * R and [v]x * R
  for i in 1:3 loop
    for j in 1:3 loop
      pR[i,j] := px[i,1]*R[1,j] + px[i,2]*R[2,j] + px[i,3]*R[3,j];
      vR[i,j] := vx[i,1]*R[1,j] + vx[i,2]*R[2,j] + vx[i,3]*R[3,j];
    end for;
  end for;

  // Ad = {{R, 0, [p]x*R}, {0, R, [v]x*R}, {0, 0, R}}
  for i in 1:3 loop
    for j in 1:3 loop
      Ad[i,j]     := R[i,j];    Ad[i,j+3]   := 0;        Ad[i,j+6]   := pR[i,j];
      Ad[i+3,j]   := 0;         Ad[i+3,j+3] := R[i,j];   Ad[i+3,j+6] := vR[i,j];
      Ad[i+6,j]   := 0;         Ad[i+6,j+3] := 0;         Ad[i+6,j+6] := R[i,j];
    end for;
  end for;
end adjoint;
