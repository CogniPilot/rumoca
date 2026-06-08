within LieGroups.SO3.Mrp;
function exp_map "Exponential map: so(3) vector -> MRP"
  input Real v[3] "Rotation vector (axis * angle)";
  output Real r[3] "MRP parameters";
protected
  Real theta_sq, theta;
  Real A "tan(theta/4) / theta";
  Real n_sq;
  constant Real eps = 1e-8;
algorithm
  theta_sq := v[1]^2 + v[2]^2 + v[3]^2;

  if theta_sq < eps then
    // Taylor: tan(t/4)/t ~ 1/4 + t^2/192
    A := 0.25 + theta_sq / 192.0;
  else
    theta := sqrt(theta_sq);
    A := tan(theta / 4.0) / theta;
  end if;

  r := A * v;

  // Shadow switch if ||r|| > 1. If-EXPRESSION (not a no-else if-statement) so
  // AD/both-branch compilers evaluate the condition correctly.
  n_sq := r[1]^2 + r[2]^2 + r[3]^2;
  r := if n_sq > 1.0 then LieGroups.SO3.Mrp.shadow(r) else r;
end exp_map;
