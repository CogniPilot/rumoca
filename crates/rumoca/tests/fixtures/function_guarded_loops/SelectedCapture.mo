package CaptureFlow
  record Problem
    Integer nodeCount;
    Real positions[2];
  end Problem;
  function Score
    input Real values[2];
    input Boolean requested;
    output Real y;
  protected
    Problem problem;
  algorithm
    y := 0;
    if requested then
      problem := Problem(2,values);
    end if;
    for node in 1:2 loop
      if requested then
        if node <= problem.nodeCount then
          y := y + problem.positions[node];
          problem.positions[node] := values[node];
        end if;
      end if;
    end for;
  end Score;
end CaptureFlow;
model SelectedCapture
  input Real values[2] = {1,2};
  input Boolean requested = true;
  Real y = CaptureFlow.Score(values,requested);
end SelectedCapture;
