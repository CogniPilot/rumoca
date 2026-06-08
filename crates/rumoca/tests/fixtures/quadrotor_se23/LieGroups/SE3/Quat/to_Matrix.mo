within LieGroups.SE3.Quat;
function to_Matrix "Convert SE(3) element to 4x4 homogeneous matrix"
  input Real X[7] "{px,py,pz, qw,qx,qy,qz}";
  output Real M[4,4] "4x4 matrix {{R, p}, {0, 1}}";
protected
  Real R[3,3];
algorithm
  R := LieGroups.SO3.Quat.to_DCM(X[4:7]);
  for i in 1:3 loop
    for j in 1:3 loop
      M[i,j] := R[i,j];
    end for;
    M[i,4] := X[i];
  end for;
  M[4,1] := 0; M[4,2] := 0; M[4,3] := 0; M[4,4] := 1;
end to_Matrix;
