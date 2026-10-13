package GuardedTuple
  record Summary
    Real matrix[2,2];
    Real score;
  end Summary;
  function Pair
    input Real values[2,2];
    output Real next[2,2];
    output Real score;
  algorithm
    next := 2*values;
    score := sum(values);
  end Pair;
  function Body
    input Real values[2,2];
    input Boolean requested;
    output Real y;
  protected
    Real scratch[2,2];
    Real score;
    Integer child;
  algorithm
    y := -1;
    child := 1;
    if requested then
      scratch := values;
      child := 2;
    end if;
    if child > 1 then
      (scratch,score) := Pair(scratch);
      y := sum(scratch)+score;
    end if;
  end Body;
  function Loop
    input Real values[2,2];
    input Boolean requested;
    output Real y;
  protected
    Real scratch[2,2];
    Real score;
    Integer child;
  algorithm
    y := 0;
    for pass in 1:2 loop
      child := 1;
      if requested then
        scratch := values;
        child := 2;
      end if;
      if child > 1 then
        (scratch,score) := Pair(scratch);
        y := y+sum(scratch)+score;
      end if;
    end for;
  end Loop;
  function Fold
    input Real values[2,2];
    output Real y;
  protected
    Real scratch[2,2];
    Real score;
  algorithm
    scratch := values;
    score := 0;
    for pass in 1:2 loop
      (scratch,score) := Pair(scratch);
    end for;
    y := sum(scratch)+score;
  end Fold;
  function Record
    input Real values[2,2];
    input Boolean requested;
    output Real y;
  protected
    Real scratch[2,2];
    Summary result;
    Integer child;
  algorithm
    y := -1;
    child := 1;
    if requested then
      scratch := values;
      child := 2;
    end if;
    if child > 1 then
      (result.matrix,result.score) := Pair(scratch);
      y := sum(result.matrix)+result.score;
    end if;
  end Record;
  function Omitted
    input Real values[2,2];
    input Boolean requested;
    output Real y;
  protected
    Real scratch[2,2];
    Real score;
    Integer child;
  algorithm
    y := -1;
    child := 1;
    if requested then
      scratch := values;
      child := 2;
    end if;
    if child > 1 then
      (,score) := Pair(scratch);
      y := score;
    end if;
  end Omitted;
end GuardedTuple;
model GuardedTupleReads
  input Real values[2,2] = {{1,2},{3,4}};
  input Boolean requested = true;
  Real bodyResult = GuardedTuple.Body(values,requested);
  Real loopResult = GuardedTuple.Loop(values,requested);
  Real foldResult = GuardedTuple.Fold(values);
  Real recordResult = GuardedTuple.Record(values,requested);
  Real omittedResult = GuardedTuple.Omitted(values,requested);
end GuardedTupleReads;
model TupleOmitted
  input Real values[2,2] = {{1,2},{3,4}};
  input Boolean requested = true;
  Real y = GuardedTuple.Omitted(values,requested);
end TupleOmitted;
