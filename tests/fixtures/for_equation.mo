model ForEquation
  Real x[3];
  Real v[3];
equation
  for i in 1:3 loop
    der(x[i]) = v[i];
  end for;
end ForEquation;
