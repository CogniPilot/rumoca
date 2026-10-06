// Full image-domain source fixture; native entry owns the complete ordered fold.
function OrderedFrameFold
  input Real values[pixelCount];
  input Real initialValue;
  input Real finalValue;
  output Real total;
  output Real checkedFinal;
protected
  constant Integer pixelCount = 14400;
  parameter Real gain = 1.0;
algorithm
  total := initialValue;
  for coordinate in 1:pixelCount loop
    total := total + gain*values[coordinate];
  end for;
  checkedFinal := integer(finalValue);
end OrderedFrameFold;

model NativeOrderedFrameFold
  constant Integer pixelCount = 14400;
  input Real values[pixelCount] = fill(0.0,pixelCount);
  input Real initialValue = -0.0;
  input Real finalValue = 0.0;
  output Real total;
  output Real checkedFinal;
equation
  (total,checkedFinal) = OrderedFrameFold(values,initialValue,finalValue);
end NativeOrderedFrameFold;
