// Test case for issue #11: Enumeration Type Resolution
// An enumeration defined in one class should be accessible when referenced
// from an external model using the class name prefix.
model SwitchController
  SwitchState state(start = SwitchState.Off);
  Real voltage;
  type SwitchState
  end SwitchState;
equation
  voltage = if state == SwitchState.On then 12.0 else 0.0;
end SwitchController;

model ExternalEnumUser
  // Reference the enum type from another class
  SwitchController.SwitchState externalState(start = SwitchController.SwitchState.Off);
  Real output_voltage;
equation
  output_voltage = if externalState == SwitchController.SwitchState.On then 5.0 else 0.0;
end ExternalEnumUser;
