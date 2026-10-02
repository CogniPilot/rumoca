model ImaginaryConstraint
  Real x(stateSelect = StateSelect.prefer, start = 1);
  Real y;
equation
  x*x = -1 + 0*time;
  der(x) = y;
  annotation(experiment(StopTime = 1));
end ImaginaryConstraint;
