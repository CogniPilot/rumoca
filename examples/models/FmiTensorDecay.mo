model FmiTensorDecay
  output Real x[2](each start = 1.0);
equation
  der(x) = {-0.5 * x[1], -x[2]};
end FmiTensorDecay;
