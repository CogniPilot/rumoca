within LieGroups.SO3.Mrp;
function inverse "MRP inverse: -r"
  input Real r[3];
  output Real r_inv[3];
algorithm
  r_inv := -r;
end inverse;
