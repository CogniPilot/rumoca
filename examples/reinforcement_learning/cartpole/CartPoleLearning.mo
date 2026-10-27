model CartPoleLearning "A tiny policy search written entirely in Modelica"
  parameter Real dt = 0.02 "Controller sample period [s]";
  parameter Real searchScale = 0.75 "Candidate-weight exploration";
  parameter Integer seed = 1 "Positive Park-Miller random seed";
  input Real bumpRequest = 0 "Unique signed request: each change triggers one bump";
  output Real appliedBump(start=0, fixed=true) "Actual signed 20 N bump [N]";
  output Real agentForce(start=0, fixed=true) "Neural policy command [N]";
  parameter Real initialAngle = 0.03 "Initial pole tilt [rad]";
  CartPole plant(initialAngle=initialAngle,
    force=max(-30,min(30,agentForce+appliedBump)));
  output Real x;
  output Real v;
  output Real theta;
  output Real omega;
  output Integer episode(start=1, fixed=true);
  output Integer episodeSteps(start=0, fixed=true);
  output Integer lastSteps(start=0, fixed=true);
  output Integer bestSteps(start=0, fixed=true);
  output Boolean trained(start=false, fixed=true);
  output Real weights[4](each start=0, each fixed=true) "Current linear-neuron weights";
  output Real best[4](each start=0, each fixed=true) "Retained highest-scoring policy";
protected
  Integer rng(start=seed, fixed=true);
  Integer bumpTicks(start=0, fixed=true);
  Real lastRequest(start=0, fixed=true);
  Real observation[4](start={0,0,initialAngle/0.20943951023931956,0}, each fixed=true);
  Real outputValue[1](each start=0, each fixed=true);
  Boolean ended(start=false, fixed=true);
  Boolean improved(start=false, fixed=true);
equation
  x=plant.x;
  v=plant.v;
  theta=plant.theta;
  omega=plant.omega;
  // A reset is a declarative event; the learner has already retained its score.
  when ended then
    reinit(plant.x,0);
    reinit(plant.v,0);
    reinit(plant.theta,initialAngle);
    reinit(plant.omega,0);
  end when;
algorithm
  when sample(dt,dt) then
    // Time the disturbance in the plant clock, independent of display pacing.
    bumpTicks := if pre(trained) and bumpRequest<>pre(lastRequest)
      and bumpRequest<>0 then 15 else max(0,pre(bumpTicks)-1);
    appliedBump := if bumpTicks>0 then
      (if bumpRequest>0 then 20 else -20) else 0;
    lastRequest := bumpRequest;
    // A sampled observation gives the controller an explicit one-sample delay.
    // pre(observation) still belongs to the old episode if this tick resets it.
    observation := {x/2.4,v/2,theta/0.20943951023931956,omega/2};
    ended := abs(pre(observation[1]))>1 or abs(pre(observation[3]))>1
      or (not pre(trained) and pre(episodeSteps)+1>=500);
    improved := ended and pre(episodeSteps)+1>pre(bestSteps);
    best := if improved then pre(weights) else pre(best);
    bestSteps := if improved then pre(episodeSteps)+1 else pre(bestSteps);
    trained := pre(trained) or bestSteps>=500;
    lastSteps := if ended then pre(episodeSteps)+1 else pre(lastSteps);
    episode := pre(episode)+(if ended then 1 else 0);
    episodeSteps := if ended then 0 else pre(episodeSteps)+1;
    if ended then
      // Keep an elite policy. Before success, try a Gaussian mutation around it.
      if trained then
        weights := best;
        rng := pre(rng);
      else
        (weights,rng) := NeuralNetwork.perturb(best,searchScale,pre(rng));
      end if;
      observation := {0,0,initialAngle/0.20943951023931956,0};
    else
      weights := pre(weights);
      rng := pre(rng);
    end if;
    // A single tanh neuron is enough here; larger models reuse forward().
    outputValue := NeuralNetwork.activate(NeuralNetwork.dense(
      {weights},{0},if ended then observation else pre(observation)));
    agentForce := 10*outputValue[1];
  end when;
  annotation(experiment(StopTime=1000,Interval=0.02,Solver="rk-like"));
end CartPoleLearning;
