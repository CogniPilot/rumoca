within LieGroups.SE2;
function wedge "Wedge map: se(2) vector -> 3x3 matrix (xi^wedge)"
  input Real xi[3] "Lie algebra {vx, vy, omega}";
  output Real M[3,3] "3x3 Lie algebra matrix";
algorithm
  M[1,1] := 0;       M[1,2] := -xi[3];  M[1,3] := xi[1];
  M[2,1] := xi[3];   M[2,2] := 0;       M[2,3] := xi[2];
  M[3,1] := 0;       M[3,2] := 0;       M[3,3] := 0;
end wedge;
