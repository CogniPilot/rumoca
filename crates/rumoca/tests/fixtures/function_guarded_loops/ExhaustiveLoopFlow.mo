package ExhaustiveFlow
  function Pair
    input Real x;
    output Boolean valid;
    output Real squared;
  algorithm
    valid := x >= 0;
    squared := x*x;
  end Pair;
  function Search
    input Real values[3];
    input Boolean usePair;
    input Boolean enabled;
    input Integer trials;
    output Real y;
  protected
    Boolean residualValid;
    Real squaredResidual;
  algorithm
    y := 0;
    if enabled then
      for hypothesis in 1:trials loop
        for point in 1:3 loop
          if usePair then
            (residualValid,squaredResidual) := Pair(values[point]);
          else
            residualValid := true;
            squaredResidual := 0;
            for axis in 1:2 loop
              squaredResidual := squaredResidual + values[point]*values[point];
            end for;
          end if;
          if residualValid then
            y := y + squaredResidual;
          end if;
        end for;
      end for;
    end if;
  end Search;
end ExhaustiveFlow;
model ExhaustiveLoopFlow
  input Real values[3] = {2,-1,3};
  input Boolean usePair = true;
  input Boolean enabled = true;
  input Integer trials = 1;
  Real y = ExhaustiveFlow.Search(values,usePair,enabled,min(2,max(0,trials)));
end ExhaustiveLoopFlow;
