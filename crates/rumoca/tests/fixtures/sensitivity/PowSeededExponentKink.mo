package PowSeededExponentKink
  function power "a raised to n"
    input Real a;
    input Real n;
    output Real p;
  algorithm
    p := a ^ n;
  end power;

  model Loop
    Real i(start = 0, fixed = true);
    Real x;
    Real n(start = 1);
  equation
    der(i) = max(time - 0.5, 0) - 0.01 * x;
    x + 0.05 * x ^ n + 0.05 * power(x, n) = i;
    n = 1 + 0.1 * x;
  end Loop;
end PowSeededExponentKink;
