function pairedGetterWork
  input Real x;
  input Boolean valid;
  input Integer index;
  output Real a;
  output Real b;
protected
  Real values[1] = {x};
  Real total = 0;
algorithm
  assert(valid, "first authored getter fault");
  for k in 1:64 loop
    total := total + sin(x+k);
  end for;
  a := values[index] + total;
  b := 2*a;
end pairedGetterWork;

model BatchGetter
  input Real x(start=1);
  input Boolean valid(start=true);
  input Integer index(start=1);
  output Real a;
  output Real b;
equation
  (a,b) = pairedGetterWork(x,valid,index);
end BatchGetter;
