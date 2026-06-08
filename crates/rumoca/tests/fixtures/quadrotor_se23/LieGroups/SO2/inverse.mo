within LieGroups.SO2;
function inverse "SO(2) inverse: negate angle"
  input Real a "Rotation angle [rad]";
  output Real a_inv "Inverse angle [rad]";
algorithm
  a_inv := -a;
end inverse;
