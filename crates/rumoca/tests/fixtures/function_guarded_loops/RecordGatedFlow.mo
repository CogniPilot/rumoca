package RecordGate
  record State
    Real position[3];
    Real referencePosition[3];
  end State;
  function Correct
    input Real position[3];
    input Real referencePosition[3];
    input Boolean requested;
    input Boolean accepted;
    output Real y;
  protected
    State previous;
    State candidate;
    Boolean valid;
  algorithm
    previous.position := position;
    previous.referencePosition := referencePosition;
    y := 0;
    valid := requested;
    if valid then
      candidate := previous;
      candidate.position := previous.position + ones(3);
      candidate.referencePosition := previous.referencePosition + 2*ones(3);
      valid := accepted;
      for i in 1:3 loop
        valid := valid and abs(candidate.position[i]) <= 100;
      end for;
    end if;
    if valid then
      valid := accepted;
      if valid then
        for i in 1:3 loop
          valid := valid and abs(candidate.referencePosition[i]-candidate.position[i]) <= 1;
        end for;
      end if;
    end if;
    if valid then
      y := sum(candidate.referencePosition);
    end if;
  end Correct;
end RecordGate;
model RecordGatedFlow
  input Real position[3] = {1,2,3};
  input Real referencePosition[3] = {1,2,3};
  input Boolean requested = true;
  input Boolean accepted = true;
  Real y = RecordGate.Correct(position,referencePosition,requested,accepted);
end RecordGatedFlow;
