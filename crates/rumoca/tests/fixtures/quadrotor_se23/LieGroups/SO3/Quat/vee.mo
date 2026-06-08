within LieGroups.SO3.Quat;
function vee "Vee map: 3x3 skew-symmetric matrix -> so(3) vector (S^vee)"
  input Real S[3,3] "Skew-symmetric matrix";
  output Real v[3] "Rotation vector";
algorithm
  v[1] := S[3,2];
  v[2] := S[1,3];
  v[3] := S[2,1];
end vee;
