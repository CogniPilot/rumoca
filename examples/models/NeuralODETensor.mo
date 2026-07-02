model NeuralODETensor
  final parameter Integer nState = 2 "Latent state dimension";
  parameter Integer nHidden(min = 2) = 32
    "Hidden width; nHidden=314 gives about 100k trainable parameters";
  final parameter Integer trainableParams =
    nHidden * nState + nHidden
    + nHidden * nHidden + nHidden
    + nState * nHidden + nState;
  parameter Real omega = 2.2 "Base spiral angular rate";
  parameter Real damping = 0.18 "Base spiral contraction rate";
  parameter Real hiddenScale = 0.35 / sqrt(nState);
  parameter Real recurrentScale = 0.04 / sqrt(nHidden);
  parameter Real outputScale = 0.04 / sqrt(nHidden);

  parameter Real W1[nHidden, nState] =
    {{if i == 1 and j == 1 then 1.0
      elseif i == 2 and j == 2 then 1.0
      else hiddenScale * (sin(0.013 * i + 0.37 * j)
                        + 0.5 * cos(0.017 * i - 0.11 * j))
      for j in 1:nState} for i in 1:nHidden};
  parameter Real b1[nHidden] =
    {if i <= 2 then 0.0 else 0.05 * sin(0.019 * i) for i in 1:nHidden};
  parameter Real Wmid[nHidden, nHidden] =
    {{if i == j then 1.0
      else recurrentScale * (sin(0.011 * i + 0.007 * j)
                           + 0.25 * cos(0.019 * i - 0.013 * j))
      for j in 1:nHidden} for i in 1:nHidden};
  parameter Real bmid[nHidden] =
    {if i <= 2 then 0.0 else 0.02 * cos(0.021 * i) for i in 1:nHidden};
  parameter Real W2[nState, nHidden] =
    {{if j == 1 then (if i == 1 then -damping else omega)
      elseif j == 2 then (if i == 1 then -omega else -damping)
      else outputScale * (cos(0.023 * j + 0.29 * i)
                        - 0.4 * sin(0.005 * j - 0.13 * i))
      for j in 1:nHidden} for i in 1:nState};
  parameter Real b2[nState] = {0.0 for i in 1:nState};

  Real x[nState](start = {1.0, 0.0}, each fixed = true) "ODE state";
  Real z1[nHidden] "First affine layer output before bias";
  Real a1[nHidden] "First hidden activation";
  Real z2[nHidden] "Second affine layer output before bias";
  Real a2[nHidden] "Second hidden activation";
  output Real dx[nState] "Neural ODE right-hand side";
  output Real radius "State radius";
equation
  z1 = W1 * x;
  for i in 1:nHidden loop
    a1[i] = tanh(z1[i] + b1[i]);
  end for;
  z2 = Wmid * a1;
  for i in 1:nHidden loop
    a2[i] = tanh(z2[i] + bmid[i]);
  end for;
  dx = W2 * a2 + b2;
  der(x) = W2 * a2 + b2;
  radius = sqrt(x[1] * x[1] + x[2] * x[2]);
  annotation(experiment(StopTime = 8.0, Interval = 0.02, Solver = "rk-like"));
end NeuralODETensor;

model NeuralODETensor100k
  extends NeuralODETensor(nHidden = 314);
  annotation(experiment(StopTime = 0.2, Interval = 0.02, Solver = "rk-like"));
end NeuralODETensor100k;
