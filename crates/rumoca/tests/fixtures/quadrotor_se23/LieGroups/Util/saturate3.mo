within LieGroups.Util;
function saturate3 "Clamp 3-vector element-wise"
  input Real x[3];
  input Real x_min[3];
  input Real x_max[3];
  output Real y[3];
algorithm
  for i in 1:3 loop
    y[i] := if x[i] > x_max[i] then x_max[i]
            elseif x[i] < x_min[i] then x_min[i]
            else x[i];
  end for;
end saturate3;
