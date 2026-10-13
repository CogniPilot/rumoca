package SlotFlow
  function Score
    input Real values[2];
    input Integer slots[2];
    input Boolean requested;
    output Real y;
  protected
    Integer currentSlot;
    Integer referenceSlot;
  algorithm
    y := 0;
    if requested then
      currentSlot := slots[1];
      referenceSlot := slots[2];
    end if;
    for node in 1:2 loop
      if requested then
        for axis in 1:1 loop
          y := y + values[if node == 1 then currentSlot else referenceSlot];
        end for;
      end if;
    end for;
    if requested then
      y := y + currentSlot + referenceSlot;
    end if;
  end Score;
end SlotFlow;
model SelectedSlots
  input Real values[2] = {1,2};
  input Integer slots[2] = {1,2};
  input Boolean requested = true;
  Real y = SlotFlow.Score(values,slots,requested);
end SelectedSlots;
