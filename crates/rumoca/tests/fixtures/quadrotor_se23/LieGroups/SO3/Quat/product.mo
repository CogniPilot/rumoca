within LieGroups.SO3.Quat;
function product "Hamilton quaternion product q * p"
  input Real q[4] "Left quaternion {w,x,y,z}";
  input Real p[4] "Right quaternion {w,x,y,z}";
  output Real r[4] "Result quaternion {w,x,y,z}";
algorithm
  r[1] := q[1]*p[1] - q[2]*p[2] - q[3]*p[3] - q[4]*p[4];
  r[2] := q[2]*p[1] + q[1]*p[2] - q[4]*p[3] + q[3]*p[4];
  r[3] := q[3]*p[1] + q[4]*p[2] + q[1]*p[3] - q[2]*p[4];
  r[4] := q[4]*p[1] - q[3]*p[2] + q[2]*p[3] + q[1]*p[4];
end product;
