within LieGroups.SO3.EulerB321;
function from_Quat "Convert quaternion to Euler B321 angles {yaw, pitch, roll}"
  input Real q[4] "Unit quaternion {w,x,y,z}";
  output Real euler[3] "{yaw (psi), pitch (theta), roll (phi)} [rad]";
protected
  Real a, b, c, d;
  Real sinp;
algorithm
  a := q[1]; b := q[2]; c := q[3]; d := q[4];

  // Pitch: theta = asin(2*(ac - bd))
  sinp := 2.0*(a*c - d*b);
  sinp := min(max(sinp, -1.0), 1.0);

  if abs(sinp) > 0.9999 then
    // Gimbal lock: pitch near +/- 90 deg
    euler[2] := asin(sinp);
    euler[3] := 0.0;
    euler[1] := atan2(2.0*(b*c + a*d), 1.0 - 2.0*(c*c + d*d));
  else
    euler[1] := atan2(2.0*(a*d + b*c), 1.0 - 2.0*(c*c + d*d));
    euler[2] := asin(sinp);
    euler[3] := atan2(2.0*(a*b + c*d), 1.0 - 2.0*(b*b + c*c));
  end if;
end from_Quat;
