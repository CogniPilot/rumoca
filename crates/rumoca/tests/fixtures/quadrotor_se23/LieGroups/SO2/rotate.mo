within LieGroups.SO2;
function rotate "Rotate a 2D vector by angle theta"
  input Real theta "Rotation angle [rad]";
  input Real v[2] "Input vector";
  output Real v_out[2] "Rotated vector";
protected
  Real c, s;
algorithm
  c := cos(theta);
  s := sin(theta);
  v_out[1] := c*v[1] - s*v[2];
  v_out[2] := s*v[1] + c*v[2];
end rotate;
