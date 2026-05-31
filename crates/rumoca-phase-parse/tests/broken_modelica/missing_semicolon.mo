// rumoca-lint: ignore-file (intentionally broken parser test fixture)
model MissingSemicolon
  Real x
equation
  der(x) = -x;
end MissingSemicolon;