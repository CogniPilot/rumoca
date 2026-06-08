within LieGroups.SO3.EulerB321;
function to_Quat "Convert Euler B321 angles to quaternion via half-angle formula"
  input Real euler[3] "{yaw (psi), pitch (theta), roll (phi)} [rad]";
  output Real q[4] "Unit quaternion {w,x,y,z}";
protected
  Real cy, sy, cp, sp, cr, sr;
algorithm
  cy := cos(euler[1] / 2.0);
  sy := sin(euler[1] / 2.0);
  cp := cos(euler[2] / 2.0);
  sp := sin(euler[2] / 2.0);
  cr := cos(euler[3] / 2.0);
  sr := sin(euler[3] / 2.0);

  // q = qz(yaw) * qy(pitch) * qx(roll)
  q[1] := cy*cp*cr + sy*sp*sr;
  q[2] := cy*cp*sr - sy*sp*cr;
  q[3] := cy*sp*cr + sy*cp*sr;
  q[4] := sy*cp*cr - cy*sp*sr;
end to_Quat;
