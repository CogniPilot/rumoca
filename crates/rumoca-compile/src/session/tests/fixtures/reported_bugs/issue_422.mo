model U4
  parameter Real xc[3] = {0.1, 0.5, 0.9};
  parameter Real s[3] = mk(xc);
  Real y(start = 1, fixed = true);
  function mk
    input Real xc[:];
    output Real s[size(xc, 1)];
  algorithm
    for i in 1:size(xc, 1) loop
      s[i] := 2 * xc[i];
    end for;
  end mk;
equation
  der(y) = -s[2] * y;
end U4;
