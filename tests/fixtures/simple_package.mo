within;

package SimplePackage
  function square
    input Real x;
    output Real y;
  algorithm
    y := x * x;
  end square;

  model UseSquare
    Real x(start = 0);
    Real y;
  equation
    der(x) = 1;
    y = square(x);
  end UseSquare;
end SimplePackage;
