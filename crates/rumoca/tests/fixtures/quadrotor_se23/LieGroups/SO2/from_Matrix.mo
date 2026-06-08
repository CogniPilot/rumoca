within LieGroups.SO2;
function from_Matrix "Extract angle from 2x2 rotation matrix"
  input Real R[2,2] "2x2 rotation matrix";
  output Real theta "Rotation angle [rad]";
algorithm
  theta := atan2(R[2,1], R[1,1]);
end from_Matrix;
