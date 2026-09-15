model ProjectedInitialCoordinate
  Real x[2](each stateSelect=StateSelect.always, each start=0);
  Real u;
  Real rate;
  Real pin(start=1, fixed=true);
  Real second(start=2, fixed=true);
equation
  der(x) = -x;
  u = x[1] + 2*x[2];
  pin = u;
  second = x[2];
  der(u) = rate;
end ProjectedInitialCoordinate;
