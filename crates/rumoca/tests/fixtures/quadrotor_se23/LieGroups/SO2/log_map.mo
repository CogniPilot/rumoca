within LieGroups.SO2;
function log_map "Logarithmic map: SO(2) -> so(2) (identity map)"
  input Real theta "Rotation angle [rad]";
  output Real omega "Angular velocity [rad]";
algorithm
  omega := theta;
end log_map;
