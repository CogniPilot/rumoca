within LieGroups.SO3.Dcm;
function to_Quat "Convert DCM to quaternion (alias for Quat.from_DCM)"
  input Real R[3,3] "Rotation matrix";
  output Real q[4] "Quaternion {w,x,y,z}";
algorithm
  q := LieGroups.SO3.Quat.from_DCM(R);
end to_Quat;
