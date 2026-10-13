package Lazy
  function Body
    input Real values[2];
    input Boolean enabled;
    output Real y;
  protected
    Real scratch[2];
  algorithm
    if enabled then scratch := values; end if;
    y := if enabled then sum(scratch) else -1;
  end Body;
  function Ordered
    input Real values[2];
    input Boolean enabled;
    output Real y;
  protected
    Real scratch[2];
  algorithm
    if not enabled then scratch := values; end if;
    y := if enabled then 100 elseif scratch[1]>0 then sum(scratch) else -1;
  end Ordered;
  function Inactive
    input Real values[2];
    input Boolean enabled;
    input Integer probe;
    output Real y;
  algorithm
    y := if enabled then sum(values) else values[probe];
  end Inactive;
  function Fold
    input Real values[2];
    input Boolean enabled;
    output Real y;
  protected
    Real scratch[2];
    Real step;
  algorithm
    y := 0;
    for pass in 1:2 loop
      if enabled then scratch := values; end if;
      step := if enabled then sum(scratch) else -1;
      y := y+step;
    end for;
  end Fold;
  function Update
    input Real values[2];
    input Boolean flags[2];
    input Integer conditionIndex;
    input Integer sourceIndex;
    input Integer destinationIndex;
    output Real y;
  protected
    Real target[2];
  algorithm
    target := values;
    target[destinationIndex] := if flags[conditionIndex] then values[sourceIndex] else 0;
    y := sum(target);
  end Update;
end Lazy;
model LazyAssignment
  input Real values[2] = {1,2};
  input Boolean enabled = true;
  input Integer probe = 0;
  Real bodyResult = Lazy.Body(values,enabled);
  Real orderedResult = Lazy.Ordered(values,enabled);
  Real inactiveResult = Lazy.Inactive(values,enabled,probe);
  Real foldResult = Lazy.Fold(values,enabled);
end LazyAssignment;
model LazyIndexOrder
  input Real values[2] = {1,2};
  input Boolean flags[2] = {true,false};
  input Integer conditionIndex = 1;
  input Integer sourceIndex = 1;
  input Integer destinationIndex = 2;
  Real y = Lazy.Update(values,flags,conditionIndex,sourceIndex,destinationIndex);
end LazyIndexOrder;
model LazyBodyOnly
  input Real values[2] = {1,2};
  input Boolean enabled = true;
  Real y = Lazy.Body(values,enabled);
end LazyBodyOnly;
model LazyEmptyFold
  input Real values[2] = {1,2};
  input Boolean enabled = true;
  Real y = Lazy.Fold(values,enabled);
end LazyEmptyFold;
model LazyOrderedOnly
  input Real values[2] = {1,2};
  input Boolean enabled = true;
  Real y = Lazy.Ordered(values,enabled);
end LazyOrderedOnly;
