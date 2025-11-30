function sincos
  input Real x;
  output Real s;
  output Real c;
algorithm
  s := sin(x);
  c := cos(x);
end sincos;

model MultiOutputTest
  Real x(start = 0);
  Real s;
  Real c;
equation
  der(x) = 1;
  (s, c) = sincos(x);
end MultiOutputTest;
