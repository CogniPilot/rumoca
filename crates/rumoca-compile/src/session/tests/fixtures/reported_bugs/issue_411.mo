model IntegerPrecision
  input Integer n(start = 0);
  output Integer y;
equation
  y = mod(9007199254740993 + n, 7);
end IntegerPrecision;
