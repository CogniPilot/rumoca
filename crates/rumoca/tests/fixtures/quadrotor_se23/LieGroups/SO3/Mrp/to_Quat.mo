within LieGroups.SO3.Mrp;
function to_Quat "Convert MRP to quaternion"
  input Real r[3] "MRP parameters";
  output Real q[4] "Quaternion {w,x,y,z}";
protected
  Real n_sq;
  Real den;
algorithm
  n_sq := r[1]^2 + r[2]^2 + r[3]^2;
  den := 1.0 + n_sq;
  q[1] := (1.0 - n_sq) / den;
  q[2] := 2.0*r[1] / den;
  q[3] := 2.0*r[2] / den;
  q[4] := 2.0*r[3] / den;
end to_Quat;
