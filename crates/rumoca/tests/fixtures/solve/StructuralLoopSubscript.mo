model StructuralLoopSubscript
  function precedes
    input Integer lhs;
    input Integer rhs;
    output Boolean result;
  algorithm
    result := lhs < rhs;
    annotation(Inline=false);
  end precedes;
  parameter Integer n = 3;
  parameter Integer sweeps = 4;
  Real drive = 1 + time;
  Real history[n, sweeps + 1];
  output Real result[n];
equation
  for i in 1:n loop
    history[i, 1] = 0;
    result[i] = history[i, sweeps + 1];
  end for;
  for k in 1:sweeps loop
    for i in 1:n loop
      history[i, k + 1] = drive - 0.1 * sum(
        history[j, if precedes(j, i) then k + 1 else k] for j in 1:n);
    end for;
  end for;
end StructuralLoopSubscript;
