within LieGroups.SE23.Quat;
function to_Matrix "Convert SE_2(3) element to 5x5 matrix"
  input Real X[10] "{p, v, q}";
  output Real M[5,5] "5x5 matrix {{R, v, p}, {0, I_2}}";
protected
  Real R[3,3];
algorithm
  R := LieGroups.SO3.Quat.to_DCM(X[7:10]);
  for i in 1:3 loop
    for j in 1:3 loop
      M[i,j] := R[i,j];
    end for;
    M[i,4] := X[i+3];  // velocity column
    M[i,5] := X[i];    // position column
  end for;
  M[4,1] := 0; M[4,2] := 0; M[4,3] := 0; M[4,4] := 1; M[4,5] := 0;
  M[5,1] := 0; M[5,2] := 0; M[5,3] := 0; M[5,4] := 0; M[5,5] := 1;
end to_Matrix;
