package P
  function f
    input Real u;
    output Real y;
  algorithm
    y := u * u;
  end f;
  model Q
    Real y;
  equation
    y = f(time);
  end Q;
end P;
