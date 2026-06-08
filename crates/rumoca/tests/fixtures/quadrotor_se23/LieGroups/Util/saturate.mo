within LieGroups.Util;
function saturate "Clamp scalar to [x_min, x_max]"
  input Real x;
  input Real x_min;
  input Real x_max;
  output Real y;
algorithm
  y := if x > x_max then x_max elseif x < x_min then x_min else x;
end saturate;
