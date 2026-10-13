package PartialBase
  function Body
    input Real values[2];
    input Boolean requested;
    output Real y;
  protected
    Real gain[2];
  algorithm
    y := 0;
    if requested then gain := values; end if;
    if requested then
      gain[1] := 7;
      y := sum(gain);
    end if;
  end Body;
  function Fold
    input Real values[2];
    input Boolean requested;
    output Real y;
  protected
    Real gain[2];
  algorithm
    y := 0;
    if requested then gain := values; end if;
    for pass in 1:2 loop
      if requested then
        gain[1] := 7;
        y := y + sum(gain);
      end if;
    end for;
  end Fold;
end PartialBase;
model PartialBaseBody
  input Real values[2] = {1,2};
  input Boolean requested = true;
  Real y = PartialBase.Body(values,requested);
end PartialBaseBody;
model PartialBaseFold
  input Real values[2] = {1,2};
  input Boolean requested = true;
  Real y = PartialBase.Fold(values,requested);
end PartialBaseFold;
