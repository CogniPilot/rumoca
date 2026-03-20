model UsesModelica
  import Modelica.Blocks;
  Real x(start = 1);
equation
  der(x) = -x;
end UsesModelica;
