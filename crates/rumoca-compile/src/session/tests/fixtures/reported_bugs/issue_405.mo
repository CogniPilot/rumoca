model TopLevel
  input Real u;
  output Real z;
equation
  z = if u > 3 then 1 else 0;
end TopLevel;
