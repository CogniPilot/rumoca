within LieGroups.SO3.Quat;
function wedge "Wedge map: so(3) vector -> 3x3 skew-symmetric matrix (xi^wedge)"
  input Real v[3] "Rotation vector";
  output Real S[3,3] "Skew-symmetric matrix [v]x";
algorithm
  S[1,1] :=  0;     S[1,2] := -v[3];  S[1,3] :=  v[2];
  S[2,1] :=  v[3];  S[2,2] :=  0;     S[2,3] := -v[1];
  S[3,1] := -v[2];  S[3,2] :=  v[1];  S[3,3] :=  0;
end wedge;
