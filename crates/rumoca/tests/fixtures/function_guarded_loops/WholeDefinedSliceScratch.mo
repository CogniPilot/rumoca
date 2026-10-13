package SliceScratch
  function Sum
    input Real values[:];
    input Boolean requested;
    output Real y;
  protected
    Real J[2,3];
  algorithm
    y := 0;
    for i in 1:size(values,1) loop
      if requested and values[i] > 0 then
        J := zeros(2,3);
        J[:,1:2] := identity(2);
        J[:,3] := {values[i],2*values[i]};
        y := y + sum(J);
      end if;
    end for;
  end Sum;
end SliceScratch;
model WholeDefinedSliceScratch
  input Real values[2] = {1,2};
  input Boolean requested = true;
  Real y = SliceScratch.Sum(values,requested);
end WholeDefinedSliceScratch;
