function OrderedTensorFrameFold
  input Real values[pixelCount];
  input Real initialValue;
  input Real finalValue;
  output Real moments[3];
  output Real checkedFinal;
protected
  constant Integer pixelCount = 14400;
  parameter Real gain = 1.0;
algorithm
  moments := {initialValue,initialValue,-0.0};
  for coordinate in 1:pixelCount loop
    moments[2] := moments[1];
    moments[1] := moments[1]+gain*values[coordinate];
    moments[3] := moments[3]+values[coordinate];
  end for;
  checkedFinal := integer(finalValue);
end OrderedTensorFrameFold;

model NativeOrderedTensorFrameFold
  constant Integer pixelCount = 14400;
  input Real values[pixelCount] = fill(0.0,pixelCount);
  input Real initialValue = -0.0;
  input Real finalValue = 0.0;
  output Real moments[3];
  output Real checkedFinal;
equation
  (moments,checkedFinal) = OrderedTensorFrameFold(values,initialValue,finalValue);
end NativeOrderedTensorFrameFold;
