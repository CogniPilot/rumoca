within LieGroups.SO3.Mrp;
function from_Quat "Convert quaternion to MRP"
  input Real q[4] "Quaternion {w,x,y,z}";
  output Real r[3] "MRP parameters";
protected
  Real q_n[4];
  Real den;
  constant Real eps = 1e-10;
algorithm
  // Ensure positive scalar part
  if q[1] < 0 then
    q_n := -q;
  else
    q_n := q;
  end if;

  den := max(1.0 + q_n[1], eps);
  r[1] := q_n[2] / den;
  r[2] := q_n[3] / den;
  r[3] := q_n[4] / den;
end from_Quat;
