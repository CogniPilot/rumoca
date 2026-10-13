
function ReturnFeature
  input Boolean first;
  input Real samples[1];
  input Integer k;
  output Real result;
algorithm
  result := 0;
  if first then
    result := 1;
    return;
  elseif samples[k] > 0 then
    result := 2;
    return;
  end if;
  result := 3;
end ReturnFeature;
model ReturnPredicates
  input Boolean first = true;
  input Real samples[1] = {5};
  input Integer k = 1;
  output Real result;
equation
  result = ReturnFeature(first, samples, k);
end ReturnPredicates;
