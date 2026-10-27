within;
package NeuralNetwork "Small neural-network functions shared by the RL examples"
  function dense "Affine layer: each row of W produces one output"
    input Real W[:, :];
    input Real b[size(W, 1)];
    input Real x[size(W, 2)];
    output Real y[size(W, 1)];
  algorithm
    for i in 1:size(W, 1) loop
      y[i] := b[i];
      for j in 1:size(W, 2) loop
        y[i] := y[i] + W[i, j]*x[j];
      end for;
    end for;
  end dense;

  function activate "Smooth bounded activation, element by element"
    input Real x[:];
    output Real y[size(x, 1)];
  algorithm
    for i in 1:size(x, 1) loop
      y[i] := tanh(x[i]);
    end for;
  end activate;

  function clip "Elementwise observation bound before inference"
    input Real x[:];
    input Real limit;
    output Real y[size(x,1)];
  algorithm
    for i in 1:size(x,1) loop
      y[i] := max(-limit,min(limit,x[i]));
    end for;
  end clip;

  function forward "Two tanh hidden layers and a linear output layer"
    input Real W1[:, :];
    input Real b1[size(W1, 1)];
    input Real W2[:, size(W1, 1)];
    input Real b2[size(W2, 1)];
    input Real W3[:, size(W2, 1)];
    input Real b3[size(W3, 1)];
    input Real x[size(W1, 2)];
    output Real y[size(W3, 1)];
  protected
    Real h1[size(W1, 1)];
    Real h2[size(W2, 1)];
  algorithm
    h1 := activate(dense(W1, b1, x));
    h2 := activate(dense(W2, b2, h1));
    y := dense(W3, b3, h2);
  end forward;

  function normal "Box-Muller sample with a small, explicit integer RNG state"
    input Integer seed;
    output Real value;
    output Integer nextSeed;
  protected
    Integer first;
    Real u1;
    Real u2;
  algorithm
    // Park-Miller products stay exactly representable in a 53-bit number.
    first := mod(16807*seed, 2147483647);
    nextSeed := mod(16807*first, 2147483647);
    u1 := first/2147483647.0;
    u2 := nextSeed/2147483647.0;
    value := sqrt(-2*log(u1))*cos(2*3.141592653589793*u2);
  end normal;

  function perturb "Sample a candidate policy around the best retained weights"
    input Real best[:];
    input Real scale;
    input Integer seed;
    output Real candidate[size(best, 1)];
    output Integer nextSeed;
  protected
    Real noise;
  algorithm
    nextSeed := seed;
    for i in 1:size(best, 1) loop
      (noise, nextSeed) := normal(nextSeed);
      candidate[i] := best[i] + scale*noise;
    end for;
  end perturb;
end NeuralNetwork;
