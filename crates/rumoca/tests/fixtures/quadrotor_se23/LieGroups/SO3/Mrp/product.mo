within LieGroups.SO3.Mrp;
function product "MRP composition"
  input Real a[3] "Left MRP";
  input Real b[3] "Right MRP";
  output Real r[3] "Result MRP";
protected
  Real na_sq, nb_sq, dot_ab, denom;
  Real cross_ba[3];
  Real n_sq;
algorithm
  na_sq := a[1]^2 + a[2]^2 + a[3]^2;
  nb_sq := b[1]^2 + b[2]^2 + b[3]^2;
  dot_ab := a[1]*b[1] + a[2]*b[2] + a[3]*b[3];

  // cross(b, a)
  cross_ba := {b[2]*a[3] - b[3]*a[2],
               b[3]*a[1] - b[1]*a[3],
               b[1]*a[2] - b[2]*a[1]};

  denom := 1.0 + na_sq*nb_sq - 2.0*dot_ab;
  if abs(denom) < 1e-10 then
    denom := 1e-10;
  end if;

  r := ((1.0 - na_sq)*b + (1.0 - nb_sq)*a - 2.0*cross_ba) / denom;

  // Shadow switch if ||r|| > 1. If-EXPRESSION (not a no-else if-statement) so
  // AD/both-branch compilers evaluate the condition correctly.
  n_sq := r[1]^2 + r[2]^2 + r[3]^2;
  r := if n_sq > 1.0 then LieGroups.SO3.Mrp.shadow(r) else r;
end product;
