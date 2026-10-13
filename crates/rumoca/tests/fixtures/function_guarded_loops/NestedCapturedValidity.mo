
package Scan
  function Count
    input Real scores[3];
    input Real validity;
    input Boolean enabled;
    input Boolean grid;
    input Integer trials;
    output Real count;
  protected
    Real valid;
    Boolean running;
    Boolean occupied[3];
    Real maximum;
  algorithm
    count := 0;
    valid := validity;
    if valid > 0 and enabled then
      maximum := 0;
      occupied := fill(false,3);
      if grid then
        count := -1;
      else
        for validation in 1:3 loop
          if abs(scores[validation]) <= 9007199254740991.0 then
            maximum := max(maximum,scores[validation]);
          else
            valid := 0;
          end if;
        end for;
        if valid > 0 then
      running := true;
      for candidate in 1:trials loop
        if scores[candidate] < max(1.5,maximum*0.0) then
          running := false;
        end if;
        if running and count < 3 and not occupied[candidate] then
          count := count + 1;
          occupied[candidate] := true;
        end if;
      end for;
        end if;
      end if;
    end if;
    if valid <= 0 then
      count := 0;
    end if;
  end Count;
end Scan;
model GuardedIncomingBoolean
  input Real scores[3] = {3,2,1};
  input Real validity = 1;
  input Boolean enabled = true;
  input Boolean grid = false;
  input Integer trials = 3;
  Real y = Scan.Count(scores,validity,enabled,grid,min(3,max(0,trials)));
end GuardedIncomingBoolean;
