model CartPoleSwingUp "Learn to swing up from hanging down, then balance"
  parameter Real initialPosition = 0 "Cart start [m]";
  parameter Real initialVelocity = 0 "Cart start [m/s]";
  parameter Real maxTravel = 2 "Episode rail limit for cart centre [m]";
  parameter Real dt = 0.02 "Policy sample period [s]";
  parameter Real initialAngle = 3.131592653589793 "Almost straight down [rad]";
  parameter Integer randomSeeds[7] = {16807,370783594,1666332895,532314182,528280723,233727855,2077330278};
  input Real bumpRequest = 0 "Unique signed disturbance request";
  output Real appliedBump(start=0,fixed=true);
  output Real agentForce(start=0,fixed=true);
  output Real x;
  output Real v;
  output Real theta;
  output Real omega;
  output Integer railHits(start=0,fixed=true);
  output Real lastRailPosition(start=0,fixed=true);
  output Integer episode(start=1,fixed=true);
  output Integer episodeSteps(start=0,fixed=true);
  output Integer uprightSteps(start=0,fixed=true);
  output Integer maxUprightSteps(start=0,fixed=true);
  output Integer generation;
  output Integer evaluations;
  output Real episodeScore(start=0,fixed=true);
  output Real lastScore(start=0,fixed=true);
  output Real bestScore(start=-1e9,fixed=true);
  output Boolean trained;
  output Real weights[7] "Learned gains on explicit physics-informed features";
  output Real features[7](each start=0,each fixed=true);
  output Real energy(start=-1,fixed=true);
  output Real gate(start=0,fixed=true);
  CartPole plant(initialAngle=initialAngle,x(start=initialPosition),v(start=initialVelocity),
    force=max(-30,min(30,agentForce+appliedBump)));
  PolicySearch.Learner learner(randomSeeds=randomSeeds,
    ended=ended,reached=reached,score=score);
protected
  Integer consumedRailHits(start=0,fixed=true);
  Integer bumpTicks(start=0,fixed=true);
  Real lastRequest(start=0,fixed=true);
  Integer stableCount(start=0,fixed=true);
  Real observation[5](start={0,0,sin(initialAngle),cos(initialAngle),0},each fixed=true);
  Real score(start=0,fixed=true);
  Real outputValue[1](each start=0,each fixed=true);
  Boolean ended(start=false,fixed=true);
  Boolean reached(start=false,fixed=true);
equation
  x=plant.x;
  v=plant.v;
  theta=plant.theta;
  omega=plant.omega;
  trained=learner.trained;
  evaluations=learner.evaluations;
  generation=learner.generation;
  weights=learner.weights;
  // A continuous boundary event ends the physical rollout at the rail limit.
  // Its count survives until the sampled learner consumes the failed episode.
  when abs(plant.x)>maxTravel then
    railHits = pre(railHits)+1;
    lastRailPosition = pre(plant.x);
  end when;
  when {ended,abs(plant.x)>maxTravel} then
    reinit(plant.x,0);
    reinit(plant.v,0);
    reinit(plant.theta,initialAngle);
    reinit(plant.omega,0);
  end when;
algorithm
  when sample(dt,dt) then
    bumpTicks := if pre(trained) and bumpRequest<>pre(lastRequest)
      and bumpRequest<>0 then 15 else max(0,pre(bumpTicks)-1);
    appliedBump := if bumpTicks>0 then (if bumpRequest>0 then 20 else -20) else 0;
    lastRequest := bumpRequest;
    observation := {x/2.4,v/4,sin(theta),cos(theta),omega/4};
    stableCount := if pre(observation[4])>0.98 and abs(pre(observation[5]))<0.25
      and abs(pre(observation[1]))<0.5 then pre(uprightSteps)+1 else 0;
    maxUprightSteps := max(pre(maxUprightSteps),stableCount);
    reached := stableCount>=250;
    // Dense energy/height feedback, with a rail-failure cost: crashing early
    // must not beat the zero-force baseline merely by shortening an episode.
    score := pre(episodeScore)+dt*(pre(observation[4])
      + (if stableCount>0 then 8 else 0)
      -0.1*(pre(observation[4])+16*pre(observation[5])^2/29.4-1)^2
      -0.05*pre(observation[1])^2-0.0001*(pre(agentForce)/10)^2)
      -(if pre(railHits)>pre(consumedRailHits) then 50 else 0);
    ended := pre(railHits)>pre(consumedRailHits)
      or (not pre(trained) and (pre(episodeSteps)+1>=1000 or reached));
    consumedRailHits := railHits;
    uprightSteps := if ended then 0 else stableCount;
    lastScore := if ended then score else pre(lastScore);
    episodeScore := if ended then 0 else score;
    bestScore := if ended then max(pre(bestScore),score) else pre(bestScore);
    episode := pre(episode)+(if ended then 1 else 0);
    episodeSteps := if ended then 0 else pre(episodeSteps)+1;
    if ended then
      observation := {0,0,sin(initialAngle),cos(initialAngle),0};
    end if;
    // A physics-informed feature map separates energy pumping from balancing.
    // Only the seven gains are learned; no stabilizing gains are preset.
    energy := pre(observation[4])+16*pre(observation[5])^2/29.4;
    gate := ((1+pre(observation[4]))/2)^8;
    features := NeuralNetwork.clip({
      (1-gate)*pre(observation[1]),2*(1-gate)*pre(observation[2]),
      (1-gate)*(energy-1)*4*pre(observation[5])*pre(observation[4])/4,
      gate*pre(observation[1]),2*gate*pre(observation[2]),
      gate*pre(observation[3])/0.20943951023931956,2*gate*pre(observation[5])},5);
    outputValue := NeuralNetwork.activate(NeuralNetwork.dense({pre(learner.weights)},{0},features));
    // Zero force on a reset tick; the new candidate is read at the next tick.
    agentForce := if ended then 0 else 10*outputValue[1];
  end when;
  annotation(experiment(StopTime=20000,Interval=0.02,Solver="rk-like"));
end CartPoleSwingUp;
