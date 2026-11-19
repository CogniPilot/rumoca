model DynamicTensorGather
  input Integer selection = 2;
  Real states[6](start={1, 2, 3, 4, 5, 6}, each fixed=true);
  output Real picked = states[selection];
equation
  der(states) = -states;
end DynamicTensorGather;
