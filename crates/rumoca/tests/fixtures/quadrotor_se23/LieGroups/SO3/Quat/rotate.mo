within LieGroups.SO3.Quat;
function rotate "Rotate a 3-vector by a quaternion: v' = q @ v"
  input Real q[4] "Unit quaternion {w,x,y,z}";
  input Real v[3] "Input vector";
  output Real v_out[3] "Rotated vector";
protected
  Real R[3,3];
algorithm
  R := LieGroups.SO3.Quat.to_DCM(q);
  v_out[1] := R[1,1]*v[1] + R[1,2]*v[2] + R[1,3]*v[3];
  v_out[2] := R[2,1]*v[1] + R[2,2]*v[2] + R[2,3]*v[3];
  v_out[3] := R[3,1]*v[1] + R[3,2]*v[2] + R[3,3]*v[3];
end rotate;
