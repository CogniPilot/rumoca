model SelectorGather2
  parameter Integer n = 8;
  Real h[n, n + 1] = {{sin(time*j + k) for k in 1:n + 1} for j in 1:n};
  Real y[n, n] = {{h[j, if j < i then j + 1 else j] for j in 1:n} for i in 1:n};
end SelectorGather2;
