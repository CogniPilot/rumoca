function checkOrder
  input Boolean valid;
  input Integer index;
  output Real y;
protected
  Real values[1] = {1};
algorithm
  assert(valid, "first authored assertion");
  assert(valid, "second authored assertion");
  y := values[index];
end checkOrder;

model AssertionFaultOrder
  input Boolean valid = false;
  input Integer index = 1;
  Real y;
equation
  y = checkOrder(valid, index);
end AssertionFaultOrder;
