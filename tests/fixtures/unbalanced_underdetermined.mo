// Under-determined model: more unknowns than equations
// This model has 2 unknowns but only 1 equation
model UnbalancedUnderdetermined
  Real x(start = 0);
  Real y(start = 1); // Extra unknown with no equation
equation
  der(x) = 1;
end UnbalancedUnderdetermined;
