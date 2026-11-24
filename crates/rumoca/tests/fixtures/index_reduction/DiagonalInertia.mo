model DiagonalInertia
  parameter Real mass = 2;
  Real v[2](each start=0, each fixed=true);
  Real u[2,2](each start=0, each fixed=true);
equation
  (1.5 * mass) * der(v) = {3, -6};
  for b in 1:2 loop
    der(u[b,:]) * (2 * mass) = {b, -2*b};
  end for;
end DiagonalInertia;
