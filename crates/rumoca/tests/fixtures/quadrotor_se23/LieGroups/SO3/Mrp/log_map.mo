within LieGroups.SO3.Mrp;
function log_map "Logarithmic map: MRP -> so(3) rotation vector"
  input Real r[3] "MRP parameters";
  output Real v[3] "Rotation vector";
protected
  Real n_sq, n;
  Real A "4*atan(||r||) / ||r||";
  constant Real eps = 1e-10;
algorithm
  n_sq := r[1]^2 + r[2]^2 + r[3]^2;

  if n_sq < eps then
    // Taylor: 4*atan(n)/n ~ 4 - 4*n^2/3
    A := 4.0 - 4.0*n_sq/3.0;
  else
    n := sqrt(n_sq);
    A := 4.0 * atan(n) / n;
  end if;

  v := A * r;
end log_map;
