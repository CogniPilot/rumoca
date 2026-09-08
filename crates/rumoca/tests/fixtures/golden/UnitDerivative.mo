model UnitDerivative
  Real x(start = 2.0, fixed = true);
equation
  der(x) = 1.0;
end UnitDerivative;
