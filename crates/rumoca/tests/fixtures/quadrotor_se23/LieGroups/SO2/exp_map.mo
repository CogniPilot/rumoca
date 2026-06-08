within LieGroups.SO2;
function exp_map "Exponential map: so(2) -> SO(2) (identity map)"
  input Real omega "Angular velocity [rad]";
  output Real theta "Rotation angle [rad]";
algorithm
  theta := omega;
end exp_map;
