model NeuralODEBackprop
  final parameter Integer nState = 2 "Spiral state dimension";
  parameter Integer nHidden(min = 2) = 12
    "Hidden width; nHidden=314 gives about 100k trainable states";
  parameter Integer nBatch(min = 2) = 8 "Streaming training batch size";
  final parameter Integer trainableParams =
    nHidden * nState + nHidden
    + nHidden * nHidden + nHidden
    + nState * nHidden + nState;
  parameter Real learningRate = 0.35 "Continuous-time gradient descent gain";
  parameter Real weightDecay = 1.0e-4 "Small L2 regularization";
  parameter Real omega = 2.2 "Target spiral angular rate";
  parameter Real damping = 0.18 "Target spiral contraction rate";
  parameter Real dataRate = 0.55 "Rotates the training batch over time";
  parameter Real pi = 3.141592653589793;
  parameter Real inputScale = 0.6 / sqrt(nState);
  parameter Real recurrentScale = 0.22 / sqrt(nHidden);
  parameter Real outputScale = 0.12 / sqrt(nHidden);

  Real W1[nHidden, nState](
    start = {{if i == j then inputScale
      else 0.10 * inputScale * (sin(0.17 * i + 0.31 * j)
                              + 0.5 * cos(0.11 * i - 0.23 * j))
      for j in 1:nState} for i in 1:nHidden},
    fixed = true) "Trainable first-layer weights";
  Real b1[nHidden](
    start = {0.02 * sin(0.19 * i) for i in 1:nHidden},
    fixed = true) "Trainable first-layer bias";
  Real Wmid[nHidden, nHidden](
    start = {{if i == j then recurrentScale
      else 0.04 * recurrentScale * (sin(0.07 * i + 0.13 * j)
                                  - 0.3 * cos(0.05 * i - 0.17 * j))
      for j in 1:nHidden} for i in 1:nHidden},
    fixed = true) "Trainable hidden-layer weights";
  Real bmid[nHidden](
    start = {0.01 * cos(0.17 * i) for i in 1:nHidden},
    fixed = true) "Trainable hidden-layer bias";
  Real W2[nState, nHidden](
    start = {{outputScale * (sin(0.23 * i + 0.07 * j)
                           - 0.4 * cos(0.13 * i - 0.11 * j))
      for j in 1:nHidden} for i in 1:nState},
    fixed = true) "Trainable output weights";
  Real b2[nState](start = {0.0 for i in 1:nState}, fixed = true)
    "Trainable output bias";

  Real angle[nBatch] "Training sample phase";
  Real sampleX[nState, nBatch] "Mini-batch state samples";
  Real targetDX[nState, nBatch] "Target spiral vector field";
  Real z1[nHidden, nBatch] "First affine layer output";
  Real a1[nHidden, nBatch] "First hidden activation";
  Real z2[nHidden, nBatch] "Second affine layer output";
  Real a2[nHidden, nBatch] "Second hidden activation";
  Real pred[nState, nBatch] "Network vector-field prediction before bias";
  Real error[nState, nBatch] "Prediction error";

  Real delta2[nHidden, nBatch] "Backprop signal at second hidden layer";
  Real delta1[nHidden, nBatch] "Backprop signal at first hidden layer";
  Real gradW1[nHidden, nState] "Gradient of loss with respect to W1";
  Real gradb1[nHidden] "Gradient of loss with respect to b1";
  Real gradWmid[nHidden, nHidden] "Gradient of loss with respect to Wmid";
  Real gradbmid[nHidden] "Gradient of loss with respect to bmid";
  Real gradW2[nState, nHidden] "Gradient of loss with respect to W2";
  Real gradb2[nState] "Gradient of loss with respect to b2";

  output Real loss "Mean mini-batch squared vector-field loss";
  output Real rmsError "Root-mean-square vector-field error";
  output Real learnedDX[nState] "Prediction for the first training sample";
  output Real targetFirst[nState] "Target derivative for the first training sample";
equation
  for b in 1:nBatch loop
    angle[b] = dataRate * time + 2.0 * pi * (b - 1) / nBatch;
    sampleX[1, b] = cos(angle[b]);
    sampleX[2, b] = sin(angle[b]);
    targetDX[1, b] = -damping * sampleX[1, b] - omega * sampleX[2, b];
    targetDX[2, b] = omega * sampleX[1, b] - damping * sampleX[2, b];
  end for;

  z1 = W1 * sampleX;
  for i in 1:nHidden loop
    for b in 1:nBatch loop
      a1[i, b] = tanh(z1[i, b] + b1[i]);
    end for;
  end for;

  z2 = Wmid * a1;
  for i in 1:nHidden loop
    for b in 1:nBatch loop
      a2[i, b] = tanh(z2[i, b] + bmid[i]);
    end for;
  end for;

  pred = W2 * a2;
  for j in 1:nState loop
    for b in 1:nBatch loop
      error[j, b] = pred[j, b] + b2[j] - targetDX[j, b];
    end for;
  end for;

  loss = 0.5 * sum(sum(error[j, b] * error[j, b] for j in 1:nState)
                   for b in 1:nBatch) / nBatch;
  rmsError = sqrt(2.0 * loss / nState);

  for j in 1:nState loop
    learnedDX[j] = pred[j, 1] + b2[j];
    targetFirst[j] = targetDX[j, 1];
    gradb2[j] = sum(error[j, b] for b in 1:nBatch) / nBatch;
    der(b2[j]) = -learningRate * (gradb2[j] + weightDecay * b2[j]);
    for i in 1:nHidden loop
      gradW2[j, i] = sum(error[j, b] * a2[i, b] for b in 1:nBatch) / nBatch;
      der(W2[j, i]) = -learningRate * (gradW2[j, i] + weightDecay * W2[j, i]);
    end for;
  end for;

  for i in 1:nHidden loop
    for b in 1:nBatch loop
      delta2[i, b] =
        sum(W2[j, i] * error[j, b] for j in 1:nState)
        * (1.0 - a2[i, b] * a2[i, b]);
    end for;
  end for;

  for i in 1:nHidden loop
    gradbmid[i] = sum(delta2[i, b] for b in 1:nBatch) / nBatch;
    der(bmid[i]) = -learningRate * (gradbmid[i] + weightDecay * bmid[i]);
    for k in 1:nHidden loop
      gradWmid[i, k] = sum(delta2[i, b] * a1[k, b] for b in 1:nBatch)
        / nBatch;
      der(Wmid[i, k]) =
        -learningRate * (gradWmid[i, k] + weightDecay * Wmid[i, k]);
    end for;
  end for;

  for i in 1:nHidden loop
    for b in 1:nBatch loop
      delta1[i, b] =
        sum(Wmid[k, i] * delta2[k, b] for k in 1:nHidden)
        * (1.0 - a1[i, b] * a1[i, b]);
    end for;
  end for;

  for i in 1:nHidden loop
    gradb1[i] = sum(delta1[i, b] for b in 1:nBatch) / nBatch;
    der(b1[i]) = -learningRate * (gradb1[i] + weightDecay * b1[i]);
    for j in 1:nState loop
      gradW1[i, j] = sum(delta1[i, b] * sampleX[j, b] for b in 1:nBatch)
        / nBatch;
      der(W1[i, j]) = -learningRate * (gradW1[i, j] + weightDecay * W1[i, j]);
    end for;
  end for;

  annotation(experiment(StopTime = 8.0, Interval = 0.05, Solver = "rk-like"));
end NeuralODEBackprop;

model NeuralODEBackprop1k
  extends NeuralODEBackprop(nHidden = 30);
  annotation(experiment(StopTime = 0.2, Interval = 0.02, Solver = "rk-like"));
end NeuralODEBackprop1k;
