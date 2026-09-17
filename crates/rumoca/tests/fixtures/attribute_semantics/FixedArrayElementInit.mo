model FixedArrayElementInit
  // MLS 3.6 section 4.8 / 4.8.6: an attribute modifier on an array component is
  // itself an array of the component's dimensions, and each element governs the
  // corresponding component element independently. Element x[1] is fixed, so it
  // is initialized from its start value; x[2] and x[3] are free and are owned by
  // the explicit initial equations below.
  Real x[3](start = {2, 1, 0}, fixed = {true, false, false});
initial equation
  x[2] = 5;
  x[3] = 7;
equation
  der(x[1]) = 0;
  der(x[2]) = 0;
  der(x[3]) = 0;
end FixedArrayElementInit;
