package AssignedScaleFlow
  function Factor
    input Real scale;
    output Real factor;
    output Boolean accepted;
  algorithm
    factor := scale;
    accepted := scale <= 10;
  end Factor;
  function Build
    input Real values[2];
    input Boolean requested;
    output Real y;
  protected
    Real paddingScale;
    Real factor;
    Boolean factorValid;
    Boolean valid;
    Integer node;
  algorithm
    y := 0;
    valid := requested;
    if valid then
      for pair in 1:2 loop
        paddingScale := 0;
        for row in 1:2 loop
          node := pair+row-1;
          if node <= 2 then
            if values[node] > 0 then
              paddingScale := max(paddingScale,abs(values[node]));
            end if;
          end if;
        end for;
        if paddingScale == 0 then paddingScale := 1; end if;
        for row in 1:2 loop
          y := y + paddingScale;
        end for;
        (factor,factorValid) := Factor(paddingScale);
        valid := valid and factorValid;
      end for;
    end if;
  end Build;
end AssignedScaleFlow;
model AssignedLoopExitScale
  input Real values[2] = {1,2};
  input Boolean requested = true;
  Real y = AssignedScaleFlow.Build(values,requested);
end AssignedLoopExitScale;
