function QuotientLoop
  input Real values[:];
  output Real total;
protected
  Integer offset;
  Integer quotient;
algorithm
  total := 0.0; offset := 0; quotient := 0;
  for index in 1:size(values,1) loop
    offset := index-1;
    quotient := div(offset,7);
    total := total+values[quotient+1];
  end for;
end QuotientLoop;
model QuotientLoopControl
  input Real values[49] = fill(1.0,49);
  output Real total;
equation
  total = QuotientLoop(values);
end QuotientLoopControl;
