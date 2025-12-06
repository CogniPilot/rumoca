model MSLPIDSimple
  import Modelica.Blocks.Continuous.PID;
  PID pid(k = 2.0, Ti = 1.0, Td = 0.5, Nd = 10);
  Real x;
equation
  der(x) = x + pid.y;
  pid.u = x;
end MSLPIDSimple;
