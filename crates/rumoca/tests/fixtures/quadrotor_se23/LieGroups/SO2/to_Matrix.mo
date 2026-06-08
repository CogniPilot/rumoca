within LieGroups.SO2;
function to_Matrix "Convert angle to 2x2 rotation matrix"
  input Real theta "Rotation angle [rad]";
  output Real R[2,2] "2x2 rotation matrix";
protected
  Real c, s;
algorithm
  c := cos(theta);
  s := sin(theta);
  R := {{c, -s}, {s, c}};
end to_Matrix;
