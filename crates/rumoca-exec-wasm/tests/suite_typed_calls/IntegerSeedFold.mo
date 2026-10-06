// Separate historical characterization; do not change Integer policy here.
function IntegerSeedFold
  input Integer values[pixelCount];
  input Integer initialValue;
  output Integer total;
protected
  constant Integer pixelCount = 14400;
algorithm
  total := initialValue;
  for coordinate in 1:pixelCount loop
    total := total + values[coordinate];
  end for;
end IntegerSeedFold;

model NativeIntegerSeedFold
  constant Integer pixelCount = 14400;
  input Integer values[pixelCount] = fill(0,pixelCount);
  input Integer initialValue = 0;
  output Real total;
equation
  total = IntegerSeedFold(values,initialValue);
end NativeIntegerSeedFold;
