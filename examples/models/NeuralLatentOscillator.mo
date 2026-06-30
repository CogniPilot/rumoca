model NeuralLatentOscillator
  final parameter Integer nLatent = 8 "Continuous latent state dimension";
  final parameter Integer nFeature = nLatent + 2 "Latent state plus time features";
  final parameter Integer nObserved = 3 "Decoded observable channels";
  parameter Integer nHidden(min = nFeature) = 128
    "Hidden width; nHidden=256 gives 71435 trainable parameters";
  final parameter Integer trainableParams =
    nHidden * nFeature + nHidden
    + nHidden * nHidden + nHidden
    + nLatent * nHidden + nLatent
    + nObserved * nHidden + nObserved;
  parameter Real residualGain = 0.08 "Scale for learned latent residual";
  parameter Real damping = 0.035 "Baseline latent damping";
  parameter Real featureScale = 0.75 / sqrt(nFeature);
  parameter Real recurrentScale = 0.025 / sqrt(nHidden);
  parameter Real dynScale = 0.045 / sqrt(nHidden);
  parameter Real obsScale = 0.18 / sqrt(nHidden);
  parameter Real freq[nLatent / 2] = {0.55, 0.85, 1.25, 1.7};

  parameter Real W1[nHidden, nFeature] =
    {{if i == j then featureScale
      else featureScale * 0.12 * (sin(0.017 * i + 0.13 * j)
                                + 0.3 * cos(0.011 * i - 0.19 * j))
      for j in 1:nFeature} for i in 1:nHidden};
  parameter Real b1[nHidden] =
    {if i <= nFeature then 0.0 else 0.025 * sin(0.041 * i) for i in 1:nHidden};
  parameter Real Wmid[nHidden, nHidden] =
    {{if i == j then 1.0
      else recurrentScale * (sin(0.007 * i + 0.011 * j)
                           - 0.25 * cos(0.013 * i - 0.017 * j))
      for j in 1:nHidden} for i in 1:nHidden};
  parameter Real bmid[nHidden] =
    {if i <= nFeature then 0.0 else 0.015 * cos(0.029 * i) for i in 1:nHidden};
  parameter Real Wdyn[nLatent, nHidden] =
    {{if j == i then 0.65
      else dynScale * (cos(0.031 * j + 0.23 * i)
                     - 0.4 * sin(0.017 * j - 0.09 * i))
      for j in 1:nHidden} for i in 1:nLatent};
  parameter Real bdyn[nLatent] = {0.0 for i in 1:nLatent};
  parameter Real Wobs[nObserved, nHidden] =
    {{if j == i then 1.0
      elseif j == i + 4 then 0.45
      else obsScale * (sin(0.021 * j + 0.31 * i)
                     + 0.2 * cos(0.009 * j - 0.17 * i))
      for j in 1:nHidden} for i in 1:nObserved};
  parameter Real bobs[nObserved] = {0.0 for i in 1:nObserved};

  Real latent[nLatent](start = {1.0, 0.0, 0.45, 0.25, -0.35, 0.8, 0.2, -0.55},
                       each fixed = true) "Continuous latent state";
  Real features[nFeature] "Neural vector-field inputs";
  Real z1[nHidden] "First affine layer output";
  Real a1[nHidden] "First hidden activation";
  Real z2[nHidden] "Recurrent affine layer output";
  Real a2[nHidden] "Second hidden activation";
  Real latentResidual[nLatent] "Learned latent derivative residual";
  output Real observed[nObserved] "Decoded observable signal";
  output Real latentEnergy "Squared latent-state norm";
equation
  for i in 1:nLatent loop
    features[i] = latent[i];
  end for;
  features[nLatent + 1] = sin(0.35 * time);
  features[nLatent + 2] = cos(0.35 * time);
  z1 = W1 * features;
  for i in 1:nHidden loop
    a1[i] = tanh(z1[i] + b1[i]);
  end for;
  z2 = Wmid * a1;
  for i in 1:nHidden loop
    a2[i] = tanh(z2[i] + bmid[i]);
  end for;
  latentResidual = Wdyn * a2 + bdyn;
  observed = Wobs * a2 + bobs;
  for k in 1:nLatent / 2 loop
    der(latent[2 * k - 1]) =
      -damping * latent[2 * k - 1] - freq[k] * latent[2 * k]
      + residualGain * latentResidual[2 * k - 1];
    der(latent[2 * k]) =
      freq[k] * latent[2 * k - 1] - damping * latent[2 * k]
      + residualGain * latentResidual[2 * k];
  end for;
  latentEnergy = sum(latent[i] * latent[i] for i in 1:nLatent);
  annotation(experiment(StopTime = 16.0, Interval = 0.04, Solver = "rk-like"));
end NeuralLatentOscillator;
