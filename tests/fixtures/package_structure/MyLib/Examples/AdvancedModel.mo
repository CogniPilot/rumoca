within MyLib.Examples;

model AdvancedModel "A more advanced test model"
  import MyLib.Functions.triple;
  Real x(start = 0.5);
  Real y(start = 1.0);
equation
  der(x) = triple(y);
  der(y) = -x;
end AdvancedModel;
