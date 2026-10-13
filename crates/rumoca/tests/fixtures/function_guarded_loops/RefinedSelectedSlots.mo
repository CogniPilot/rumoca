package RefinedSlotFlow
  function Score
    input Real values[2];
    input Integer slots[2];
    input Boolean requested;
    output Real y;
  protected
    Boolean valid;
    Integer currentSlot;
    Integer referenceSlot;
  algorithm
    y := 0;
    valid := requested;
    if valid then
      currentSlot := slots[1];
      referenceSlot := slots[2];
      valid := values[1] > 0 and values[2] > 0;
    end if;
    if valid then
      valid := values[1] <= 10 and values[2] <= 10;
    end if;
    if valid then
      for node in 1:2 loop
        for axis in 1:1 loop
          valid := valid and abs(values[node]
            - values[if node == 1 then currentSlot else referenceSlot]) <= 1e-6;
          y := y + values[if node == 1 then currentSlot else referenceSlot];
        end for;
      end for;
      if valid then
        y := y + currentSlot + referenceSlot;
      end if;
    end if;
  end Score;
end RefinedSlotFlow;
model RefinedSelectedSlots
  input Real values[2] = {1,2};
  input Integer slots[2] = {1,2};
  input Boolean requested = true;
  Real y = RefinedSlotFlow.Score(values,slots,requested);
end RefinedSelectedSlots;
