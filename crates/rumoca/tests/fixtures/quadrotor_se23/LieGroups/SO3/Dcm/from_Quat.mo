within LieGroups.SO3.Dcm;
function from_Quat "Convert quaternion to DCM (alias for Quat.to_DCM)"
  input Real q[4] "Quaternion {w,x,y,z}";
  output Real R[3,3] "Rotation matrix";
algorithm
  R := LieGroups.SO3.Quat.to_DCM(q);
end from_Quat;
