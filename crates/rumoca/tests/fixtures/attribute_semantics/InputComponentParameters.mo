model InputComponentParameters
  model Signal
    constant Real offset = 1;
    parameter Real gain = 3;
    input Real u = 2 * gain + offset;
    output Real y = u;
  end Signal;
  input Signal signal;
  output Real y = signal.y;
end InputComponentParameters;
