function guardedWholeScratch
  input Boolean selected[3];
  output Real result[3];
protected
  Real color[3];
algorithm
  result := zeros(3);
  for i in 1:3 loop
    if selected[i] then
      for row in 0:1 loop
        for column in 0:1 loop
          color := {i+row+column, 0.0, 0.0};
          result[i] := result[i]+color[1];
        end for;
      end for;
    end if;
  end for;
end guardedWholeScratch;

model GuardedWholeLoopScratch
  input Boolean selected[3] = {true,false,true};
  Real state[3](each start=0.0, each fixed=true);
equation
  der(state) = guardedWholeScratch(selected);
end GuardedWholeLoopScratch;
