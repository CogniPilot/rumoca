package Atan2OriginKink
  function angle "Planar angle of (a, b)"
    input Real a;
    input Real b;
    output Real phi;
  algorithm
    phi := atan2(a, b);
  end angle;

  model Loop
    Real i(start = 0, fixed = true);
    Real x;
    Real z;
  equation
    der(i) = max(time - 0.5, 0) - 0.01 * x;
    x + 0.1 * x * atan2(x, z) = i;
    z + 0.1 * z * angle(z, x) = 2 * i;
  end Loop;
end Atan2OriginKink;
