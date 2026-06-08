within LieGroups.SO3.Quat;
function kinematics "Quaternion kinematics: q_dot = 0.5 * q * [0, omega]"
  input Real q[4] "Unit quaternion {w,x,y,z}";
  input Real omega[3] "Body-frame angular velocity {p,q,r}";
  output Real q_dot[4] "Time derivative of quaternion";
algorithm
  q_dot[1] := 0.5 * (-q[2]*omega[1] - q[3]*omega[2] - q[4]*omega[3]);
  q_dot[2] := 0.5 * ( q[1]*omega[1] - q[4]*omega[2] + q[3]*omega[3]);
  q_dot[3] := 0.5 * ( q[4]*omega[1] + q[1]*omega[2] - q[2]*omega[3]);
  q_dot[4] := 0.5 * (-q[3]*omega[1] + q[2]*omega[2] + q[1]*omega[3]);
end kinematics;
