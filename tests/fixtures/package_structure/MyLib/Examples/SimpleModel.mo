within MyLib.Examples;

model SimpleModel "A simple test model"
  import MyLib.Functions.double;
  Real x(start = 1.0);
equation
  der(x) = double(x);
end SimpleModel;
