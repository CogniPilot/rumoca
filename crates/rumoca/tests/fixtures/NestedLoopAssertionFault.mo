function nestedLoopCheck
  input Boolean valid;
  input Integer index;
  output Real y;
protected
  Real values[1] = {1};
algorithm
  for i in 1:3 loop
    assert(valid, "nested first " + String(i));
  end for;
  assert(valid, "later assertion");
  y := values[index];
end nestedLoopCheck;

function nestedLoopCaller
  input Boolean valid;
  input Integer index;
  output Real y;
algorithm
  y := nestedLoopCheck(valid, index);
end nestedLoopCaller;

model NestedLoopAssertionFault
  input Boolean valid(start=false) = false;
  input Integer index(start=1) = 1;
  Real y;
equation
  y = nestedLoopCaller(valid, index);
end NestedLoopAssertionFault;
