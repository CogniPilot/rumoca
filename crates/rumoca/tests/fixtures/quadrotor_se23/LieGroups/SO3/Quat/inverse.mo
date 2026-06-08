within LieGroups.SO3.Quat;
function inverse "Quaternion inverse (conjugate for unit quaternions)"
  input Real q[4] "{w,x,y,z}";
  output Real q_inv[4] "{w,-x,-y,-z}";
algorithm
  q_inv[1] :=  q[1];
  q_inv[2] := -q[2];
  q_inv[3] := -q[3];
  q_inv[4] := -q[4];
end inverse;
