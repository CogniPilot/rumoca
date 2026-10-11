model C13
  parameter Integer nx = 3;
  Real m[nx];
  Real y(start = 1, fixed = true);
equation
  for k in 1:nx loop
    m[k] = mod(k - 1, nx);
  end for;
  der(y) = -m[3] * y;
end C13;
