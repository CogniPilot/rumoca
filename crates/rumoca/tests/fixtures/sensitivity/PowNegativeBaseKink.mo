package PowNegativeBaseKink
  function maskedRoot "Square root of a - 1, masked to zero below one"
    input Real a;
    output Real r;
  algorithm
    r := max(0, (a - 1) ^ 0.5);
  end maskedRoot;

  model Loop
    Real i(start = 0, fixed = true);
    Real x;
    Real z;
  equation
    der(i) = max(time - 0.5, 0) - 0.01 * x;
    x + 0.1 * z ^ 3 + noEvent(max(0, (x - 1) ^ 0.5)) = i;
    z + 0.1 * x ^ 3 + maskedRoot(z) = i;
  end Loop;
end PowNegativeBaseKink;
