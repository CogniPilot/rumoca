within LieGroups.SO2;
function product "SO(2) group product: angle addition"
  input Real a "Left rotation angle [rad]";
  input Real b "Right rotation angle [rad]";
  output Real c "Result rotation angle [rad]";
algorithm
  c := a + b;
end product;
