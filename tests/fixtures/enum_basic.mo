// Basic enumeration test
model EnumBasic
  State currentState(start = State.Off);
  Real value;
  type State
  end State;
equation
  value = if currentState == State.On then 1.0 else 0.0;
end EnumBasic;
