within LieGroups.SE3.Quat;
function adjoint "Adjoint Ad_X for SE(3), translation-first ordering"
  input Real X[7] "{px,py,pz, qw,qx,qy,qz}";
  output Real Ad[6,6] "6x6 adjoint matrix";
protected
  Real R[3,3] "Rotation matrix";
  Real px[3,3] "Skew [p]x";
  Real pR[3,3] "[p]x * R";
algorithm
  R := LieGroups.SO3.Quat.to_DCM(X[4:7]);

  // Skew of position
  px := LieGroups.SO3.Quat.wedge(X[1:3]);

  // [p]x * R
  for i in 1:3 loop
    for j in 1:3 loop
      pR[i,j] := px[i,1]*R[1,j] + px[i,2]*R[2,j] + px[i,3]*R[3,j];
    end for;
  end for;

  // Ad = {{R, [p]x*R}, {0, R}}
  for i in 1:3 loop
    for j in 1:3 loop
      Ad[i,j] := R[i,j];
      Ad[i,j+3] := pR[i,j];
      Ad[i+3,j] := 0;
      Ad[i+3,j+3] := R[i,j];
    end for;
  end for;
end adjoint;
