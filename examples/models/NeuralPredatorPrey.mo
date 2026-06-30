model NeuralPredatorPrey
  final parameter Integer nState = 2 "prey, predator";
  final parameter Integer nInput = 3 "normalized prey, predator, seasonal forcing";
  parameter Integer nHidden(min = 3) = 24
    "Hidden width; nHidden=313 gives about 100k trainable parameters";
  final parameter Integer trainableParams =
    nHidden * nInput + nHidden
    + nHidden * nHidden + nHidden
    + nState * nHidden + nState;
  parameter Real preyEquilibrium = 1.0;
  parameter Real predatorEquilibrium = 1.0;
  parameter Real cycleGain = 1.25 "Predator-prey coupling around equilibrium";
  parameter Real seasonalGain = 0.18 "Seasonal perturbation strength";
  parameter Real seasonRate = 0.45 "Seasonal forcing frequency";
  parameter Real inputScale = 0.8;
  parameter Real hiddenScale = 0.05 / sqrt(nHidden);
  parameter Real recurrentScale = 0.035 / sqrt(nHidden);
  parameter Real outputScale = 0.03 / sqrt(nHidden);

  parameter Real W1[nHidden, nInput] =
    {{if i == j then inputScale
      else hiddenScale * (sin(0.031 * i + 0.17 * j)
                        + 0.35 * cos(0.019 * i - 0.11 * j))
      for j in 1:nInput} for i in 1:nHidden};
  parameter Real b1[nHidden] =
    {if i <= nInput then 0.0 else 0.03 * sin(0.07 * i) for i in 1:nHidden};
  parameter Real Wmid[nHidden, nHidden] =
    {{if i == j then 1.0
      else recurrentScale * (sin(0.009 * i + 0.013 * j)
                           - 0.25 * cos(0.021 * i - 0.005 * j))
      for j in 1:nHidden} for i in 1:nHidden};
  parameter Real bmid[nHidden] =
    {if i <= nInput then 0.0 else 0.02 * cos(0.041 * i) for i in 1:nHidden};
  parameter Real W2[nState, nHidden] =
    {{if j == 1 then (if i == 2 then cycleGain else 0.0)
      elseif j == 2 then (if i == 1 then -cycleGain else 0.0)
      elseif j == 3 then (if i == 1 then seasonalGain else -0.5 * seasonalGain)
      else outputScale * (cos(0.029 * j + 0.23 * i)
                        - 0.4 * sin(0.017 * j - 0.09 * i))
      for j in 1:nHidden} for i in 1:nState};
  parameter Real b2[nState] = {0.0 for i in 1:nState};

  Real population[nState](start = {1.35, 0.72}, each fixed = true)
    "Normalized prey and predator populations";
  Real features[nInput] "Network inputs";
  Real z1[nHidden] "First affine layer output";
  Real a1[nHidden] "First hidden activation";
  Real z2[nHidden] "Second affine layer output";
  Real a2[nHidden] "Second hidden activation";
  output Real growth[nState] "Neural per-capita growth rates";
  output Real interactionIndex "Product of normalized populations";
equation
  features[1] = population[1] / preyEquilibrium - 1.0;
  features[2] = population[2] / predatorEquilibrium - 1.0;
  features[3] = sin(seasonRate * time);
  z1 = W1 * features;
  for i in 1:nHidden loop
    a1[i] = tanh(z1[i] + b1[i]);
  end for;
  z2 = Wmid * a1;
  for i in 1:nHidden loop
    a2[i] = tanh(z2[i] + bmid[i]);
  end for;
  growth = W2 * a2 + b2;
  der(population[1]) = population[1] * growth[1];
  der(population[2]) = population[2] * growth[2];
  interactionIndex = population[1] * population[2];
  annotation(experiment(StopTime = 24.0, Interval = 0.04, Solver = "rk-like"));
end NeuralPredatorPrey;
