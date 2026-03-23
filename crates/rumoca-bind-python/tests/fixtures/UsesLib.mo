model UsesLib
  parameter Real gain = Lib.Source.defaultGain;
  Real x(start = 1);
equation
  der(x) = -gain * x;
end UsesLib;
