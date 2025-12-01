// Over-determined model: more equations than unknowns
// This model has 2 equations but only 1 unknown
model UnbalancedOverdetermined
  Real x(start=0);
equation
  der(x) = 1;
  x = 2 * time;  // Extra equation makes it over-determined
end UnbalancedOverdetermined;
