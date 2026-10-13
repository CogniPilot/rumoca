package CarriedRecord
  record Edge
    Boolean enabled;
    Real weight;
  end Edge;
  record State
    Edge edges[3];
  end State;
  function Scan
    input Real weights[3];
    input Boolean flags[3];
    input Boolean requested;
    input Boolean reset;
    input Integer limit;
    output Real y;
  protected
    State working;
    Boolean valid;
  algorithm
    y := -1;
    valid := requested;
    if valid then
      if reset then
        working := State({Edge(false,0),Edge(false,0),Edge(false,0)});
      else
        working := State({Edge(flags[1],weights[1]),Edge(flags[2],weights[2]),Edge(flags[3],weights[3])});
      end if;
    end if;
    if valid then
      for slot in 1:min(3,max(0,limit)) loop
        if working.edges[slot].enabled then
          working.edges[slot] := Edge(false,working.edges[slot].weight+1);
        end if;
      end for;
      y := working.edges[1].weight+working.edges[2].weight+working.edges[3].weight;
    end if;
  end Scan;
end CarriedRecord;
model RecordLoopEntry
  input Real weights[3] = {1,2,3};
  input Boolean flags[3] = {true,false,true};
  input Boolean requested = true;
  input Boolean reset = false;
  input Integer limit = 3;
  Real y = CarriedRecord.Scan(weights,flags,requested,reset,limit);
end RecordLoopEntry;
