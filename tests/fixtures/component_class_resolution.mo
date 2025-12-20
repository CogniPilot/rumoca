// Test case for issue #9: Component Class Not Found
// Component types defined in the same file or package should be resolvable.
record SignalData
  Real value;
  Real timestamp;
end SignalData;

model SignalProcessor
  SignalData inputSignal; // Uses record type from same file
  SignalData outputSignal;
  Real gain = 2.0;
equation
  outputSignal.value = gain * inputSignal.value;
  outputSignal.timestamp = inputSignal.timestamp;
end SignalProcessor;

model CompositeSystem
  SignalProcessor processor; // Uses model type from same file
  Real systemOutput;
equation
  systemOutput = processor.outputSignal.value;
end CompositeSystem;
