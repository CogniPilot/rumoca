within LieGroups.Util;
function angle_wrap "Wrap angle to [-pi, pi]"
  input Real theta;
  output Real wrapped;
protected
  constant Real pi = 3.1415926535897932384626433832795;
algorithm
  wrapped := theta - 2*pi * floor((theta + pi) / (2*pi));
end angle_wrap;
