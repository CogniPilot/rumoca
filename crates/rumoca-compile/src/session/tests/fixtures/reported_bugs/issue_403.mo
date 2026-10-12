model GuardedOperands
  input Integer k(start = 4);
  Real y;
  function guarded
    input Real a[3];
    input Integer k;
    input Integer n;
    output Real y;
  algorithm
    y := if k <= n and a[k] > 5 then 1 else 0;
  end guarded;
equation
  y = guarded({1, 2, 9}, k, 3);
end GuardedOperands;
