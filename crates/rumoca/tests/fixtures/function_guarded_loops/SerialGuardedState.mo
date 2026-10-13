package SerialGuardFlow
  record Catalog
    Integer nextId;
    Real values[2];
  end Catalog;
  function Score
    input Real values[2];
    input Boolean requested;
    output Real y;
  protected
    Catalog previous;
    Boolean valid;
    Integer oldest;
    Integer currentId;
    Integer anchorSlot;
  algorithm
    previous := Catalog(3,values);
    valid := requested;
    y := 0;
    if valid then
      oldest := max(1,previous.nextId-2);
      currentId := previous.nextId-1;
      anchorSlot := mod(oldest-1,2)+1;
      valid := previous.values[anchorSlot] > 0 and currentId >= oldest;
    end if;
    if valid then
      oldest := max(oldest,currentId-1);
      anchorSlot := mod(oldest-1,2)+1;
      for axis in 1:2 loop
        if valid then
          y := y + previous.values[axis] + oldest;
        end if;
      end for;
    end if;
  end Score;
end SerialGuardFlow;
model SerialGuardedState
  input Real values[2] = {1,2};
  input Boolean requested = true;
  Real y = SerialGuardFlow.Score(values,requested);
end SerialGuardedState;
