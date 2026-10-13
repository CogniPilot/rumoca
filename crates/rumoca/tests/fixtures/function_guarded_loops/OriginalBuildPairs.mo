package OriginalPairFlow
  constant Integer nodeCapacity = 128;
  constant Integer edgeCapacity = 256;
  constant Integer poseDimension = 6;
  constant Integer pairCapacity = 64;
  constant Integer pairDimension = 12;
  record Pairs
    Real factor[pairCapacity,pairDimension,pairDimension];
    Boolean active[nodeCapacity];
    Boolean valid;
  end Pairs;
  function EmptyPairs
    output Pairs pairs;
  algorithm
    pairs.factor := zeros(pairCapacity,pairDimension,pairDimension);
    pairs.active := fill(false,nodeCapacity); pairs.valid := false;
  end EmptyPairs;
  function FactorPair
    input Real A[pairDimension,pairDimension];
    output Real L[pairDimension,pairDimension]; output Boolean valid;
  protected Real scale; Real value;
  algorithm
    L := zeros(pairDimension,pairDimension); scale := 0.0; value := 0.0; valid := true;
    for i in 1:pairDimension loop scale := max(scale,abs(A[i,i])); end for;
    valid := scale > 1e-12 and scale <= 1e18;
    for i in 1:pairDimension loop
      for j in 1:pairDimension loop
        valid := valid and abs(A[i,j]) <= 1e18 and abs(A[i,j]-A[j,i]) <= 1e-10*max(1.0,scale);
        value := A[i,j];
        for k in 1:pairDimension loop
          if j <= i and k < j then value := value-L[i,k]*L[j,k]; end if;
        end for;
        if j <= i then
          if i == j then
            valid := valid and value > 1e-12*scale and value <= 1e18;
            L[i,j] := sqrt(if value > 1e-12*scale and value <= 1e18 then value else 1.0);
          else L[i,j] := value/L[j,j]; end if;
        end if;
      end for;
    end for;
    if not valid then L := zeros(pairDimension,pairDimension); end if;
  end FactorPair;
  function BuildPairs
    input Real nodeMask[nodeCapacity]; input Real edgeMask[edgeCapacity];
    input Integer source[edgeCapacity]; input Integer target[edgeCapacity];
    input Real Ji[edgeCapacity,poseDimension,poseDimension];
    input Real Jj[edgeCapacity,poseDimension,poseDimension];
    input Real information[edgeCapacity,poseDimension,poseDimension];
    output Pairs pairs;
  protected
    Real blocks[pairCapacity,pairDimension,pairDimension]; Real normalBlock[poseDimension,poseDimension];
    Real left[poseDimension,poseDimension]; Real right[poseDimension,poseDimension];
    Real L[pairDimension,pairDimension]; Real paddingScale; Boolean factorValid;
    Integer a; Integer b; Integer pa; Integer pb; Integer oa; Integer ob; Integer node;
  algorithm
    pairs := EmptyPairs(); pairs.valid := true;
    blocks := zeros(pairCapacity,pairDimension,pairDimension); normalBlock := zeros(poseDimension,poseDimension);
    left := normalBlock; right := normalBlock; L := zeros(pairDimension,pairDimension); factorValid := false;
    paddingScale := 1.0; a := 1; b := 1; pa := 1; pb := 1; oa := 0; ob := 0; node := 1;
    for i in 1:nodeCapacity loop
      pairs.valid := pairs.valid and (nodeMask[i] == 0.0 or nodeMask[i] == 1.0);
      pairs.active[i] := i > 1 and nodeMask[i] == 1.0;
    end for;
    for edge in 1:edgeCapacity loop
      pairs.valid := pairs.valid and (edgeMask[edge] == 0.0 or edgeMask[edge] == 1.0);
      if pairs.valid and edgeMask[edge] == 1.0 then
        a := source[edge]; b := target[edge];
        pairs.valid := a >= 1 and a <= nodeCapacity and b >= 1 and b <= nodeCapacity and a <> b;
        if pairs.valid then pairs.valid := nodeMask[a] == 1.0 and nodeMask[b] == 1.0; end if;
        if pairs.valid then
          for sideA in 1:2 loop
            for sideB in 1:2 loop
              a := if sideA == 1 then source[edge] else target[edge];
              b := if sideB == 1 then source[edge] else target[edge];
              if a > 1 and b > 1 then
                pa := div(a-2,2)+1; pb := div(b-2,2)+1;
                if pa == pb then
                  oa := mod(a-2,2)*poseDimension; ob := mod(b-2,2)*poseDimension;
                  left := if sideA == 1 then Ji[edge,:,:] else Jj[edge,:,:];
                  right := if sideB == 1 then Ji[edge,:,:] else Jj[edge,:,:];
                  normalBlock := transpose(left)*information[edge,:,:]*right;
                  for i in 1:poseDimension loop for j in 1:poseDimension loop
                    blocks[pa,oa+i,ob+j] := blocks[pa,oa+i,ob+j]+normalBlock[i,j];
                  end for; end for;
                end if;
              end if;
            end for;
          end for;
        end if;
      end if;
    end for;
    if pairs.valid then
      for pair in 1:pairCapacity loop
        paddingScale := 0.0;
        for row in 1:pairDimension loop
          node := 2+2*(pair-1)+div(row-1,poseDimension);
          if node <= nodeCapacity then
            if pairs.active[node] then paddingScale := max(paddingScale,abs(blocks[pair,row,row])); end if;
          end if;
        end for;
        if paddingScale == 0.0 then paddingScale := 1.0; end if;
        for row in 1:pairDimension loop
          node := 2+2*(pair-1)+div(row-1,poseDimension);
          // Inactive and out-of-range padding never read numerical input cells.
          if node > nodeCapacity then blocks[pair,row,row] := paddingScale;
          elseif not pairs.active[node] then blocks[pair,row,row] := paddingScale; end if;
        end for;
        (L,factorValid) := FactorPair(blocks[pair,:,:]); pairs.valid := pairs.valid and factorValid;
        pairs.factor[pair,:,:] := L;
      end for;
    end if;
    if not pairs.valid then pairs := EmptyPairs(); end if;
  end BuildPairs;
end OriginalPairFlow;
model OriginalBuildPairs
  input Real values[2] = {1,2};
  input Boolean requested = true;
  OriginalPairFlow.Pairs pairs;
  Real y;
  Real activeCount;
  Real validValue;
equation
  pairs = OriginalPairFlow.BuildPairs(
    if requested then {if node <= 3 then 1.0 else 0.0 for node in 1:OriginalPairFlow.nodeCapacity}
      else zeros(OriginalPairFlow.nodeCapacity),
    if requested then {if edge <= 2 then 1.0 else 0.0 for edge in 1:OriginalPairFlow.edgeCapacity}
      else zeros(OriginalPairFlow.edgeCapacity),
    {if edge == 1 then 2 else 3 for edge in 1:OriginalPairFlow.edgeCapacity},
    fill(1,OriginalPairFlow.edgeCapacity),
    {{{if row == column then (if edge == 1 then values[1] else values[2]) else 0.0
        for column in 1:OriginalPairFlow.poseDimension}
        for row in 1:OriginalPairFlow.poseDimension}
        for edge in 1:OriginalPairFlow.edgeCapacity},
    zeros(OriginalPairFlow.edgeCapacity,OriginalPairFlow.poseDimension,OriginalPairFlow.poseDimension),
    {{{if row == column then 1.0 else 0.0
        for column in 1:OriginalPairFlow.poseDimension}
        for row in 1:OriginalPairFlow.poseDimension}
        for edge in 1:OriginalPairFlow.edgeCapacity});
  y = sum(pairs.factor);
  activeCount = sum(if pairs.active[node] then 1.0 else 0.0 for node in 1:OriginalPairFlow.nodeCapacity);
  validValue = if pairs.valid then 1.0 else 0.0;
end OriginalBuildPairs;
