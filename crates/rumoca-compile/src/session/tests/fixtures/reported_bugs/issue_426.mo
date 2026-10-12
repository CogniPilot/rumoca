model G1
  parameter Integer n = 4;
  Real u[n](each start = 1.0, each fixed = true);
equation
  for i in 1:n loop
    der(u[i]) = (if i > 1 then u[i - 1] else 0) - u[i];
  end for;
end G1;
