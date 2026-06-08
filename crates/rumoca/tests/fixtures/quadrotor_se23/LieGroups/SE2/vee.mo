within LieGroups.SE2;
function vee "Vee map: 3x3 se(2) matrix -> vector (M^vee)"
  input Real M[3,3] "3x3 Lie algebra matrix";
  output Real xi[3] "Lie algebra {vx, vy, omega}";
algorithm
  xi[1] := M[1,3];
  xi[2] := M[2,3];
  xi[3] := M[2,1];
end vee;
