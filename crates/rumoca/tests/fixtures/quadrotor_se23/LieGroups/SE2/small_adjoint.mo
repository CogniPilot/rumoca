within LieGroups.SE2;
function small_adjoint "Small adjoint ad_xi for se(2), translation-first ordering"
  input Real xi[3] "Lie algebra {vx, vy, omega}";
  output Real ad[3,3] "3x3 small adjoint matrix";
algorithm
  // ad_xi = {{omega^wedge, [vy, -vx]^T}, {0, 0, 0}}
  // where omega^wedge (2D) = {{0, -omega}, {omega, 0}}
  ad[1,1] := 0;        ad[1,2] := -xi[3];  ad[1,3] := xi[2];
  ad[2,1] := xi[3];    ad[2,2] := 0;       ad[2,3] := -xi[1];
  ad[3,1] := 0;        ad[3,2] := 0;       ad[3,3] := 0;
end small_adjoint;
