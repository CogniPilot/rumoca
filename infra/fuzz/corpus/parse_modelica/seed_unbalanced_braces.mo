// rumoca-lint: ignore-file (intentionally broken parser test fixture)
model UnbalancedBraces
  Real x(start = {1, 2;
equation
  x = 1;
end UnbalancedBraces;