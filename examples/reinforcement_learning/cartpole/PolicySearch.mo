within;
package PolicySearch "Diagonal cross-entropy search over neural policy weights"
  function candidate "Draw one policy; the caller owns the random state"
    input Real mean[:];
    input Real deviation[size(mean,1)];
    input Integer seeds[size(mean,1)];
    output Real weights[size(mean,1)];
    output Integer nextSeeds[size(mean,1)];
  protected
    Real noise;
  algorithm
    for i in 1:size(mean,1) loop
      (noise,nextSeeds[i]) := NeuralNetwork.normal(seeds[i]);
      weights[i] := mean[i]+deviation[i]*noise;
    end for;
  end candidate;

  function retain "Replace only the worst elite when this episode is better"
    input Real elite[:, :];
    input Real scores[size(elite,1)];
    input Real weights[size(elite,2)];
    input Real score;
    output Real updated[size(elite,1),size(elite,2)];
    output Real updatedScores[size(elite,1)];
  protected
    Integer worst;
  algorithm
    updated := elite;
    updatedScores := scores;
    worst := 1;
    for i in 2:size(scores,1) loop
      if scores[i]<scores[worst] then
        worst := i;
      end if;
    end for;
    if score>scores[worst] then
      for j in 1:size(weights,1) loop
        updated[worst,j] := weights[j];
      end for;
      updatedScores[worst] := score;
    end if;
  end retain;

  function fit "Refit each weight's Gaussian from the retained elite policies"
    input Real elite[:, :];
    output Real mean[size(elite,2)];
    output Real deviation[size(elite,2)];
  protected
    Real variance;
  algorithm
    for j in 1:size(elite,2) loop
      mean[j] := 0;
      for i in 1:size(elite,1) loop
        mean[j] := mean[j]+elite[i,j]/size(elite,1);
      end for;
      variance := 0;
      for i in 1:size(elite,1) loop
        variance := variance+(elite[i,j]-mean[j])^2/size(elite,1);
      end for;
      deviation[j] := max(1.0,sqrt(variance));
    end for;
  end fit;

  model Learner "Episode-level search, separate from the fast plant/controller clock"
    parameter Integer randomSeeds[7];
    input Boolean ended;
    input Boolean reached;
    input Real score;
    output Boolean trained(start=false,fixed=true);
    output Integer evaluations(start=0,fixed=true);
    output Integer generation(start=1,fixed=true);
    output Real weights[7](each start=0,each fixed=true);
  protected
    Integer rng[7](start=randomSeeds,each fixed=true);
    Real mean[7](each start=0,each fixed=true);
    Real deviation[7](each start=1,each fixed=true);
    Real elite[2,7](each start=0,each fixed=true);
    Real eliteScores[2](each start=-1e9,each fixed=true);
    Real retained[2,7](each start=0,each fixed=true);
    Real scores[2](each start=-1e9,each fixed=true);
  algorithm
    when ended then
      trained := pre(trained) or reached;
      if not pre(trained) then
        evaluations := pre(evaluations)+1;
        (retained,scores) := retain(pre(elite),pre(eliteScores),pre(weights),score);
        if mod(evaluations,32)==0 then
          (mean,deviation) := fit(retained);
          generation := pre(generation)+1;
        else
          mean := pre(mean);
          deviation := pre(deviation);
          generation := pre(generation);
        end if;
        elite := retained;
        eliteScores := scores;
        if trained then
          weights := pre(weights);
          rng := pre(rng);
        else
          // The first candidate after a refit uses the prior distribution;
          // this explicit event delay keeps search updates separate from sampling.
          (weights,rng) := candidate(pre(mean),pre(deviation),pre(rng));
        end if;
      else
        evaluations := pre(evaluations);
        generation := pre(generation);
        retained := pre(retained);
        scores := pre(scores);
        mean := pre(mean);
        deviation := pre(deviation);
        elite := pre(elite);
        eliteScores := pre(eliteScores);
        weights := pre(weights);
        rng := pre(rng);
      end if;
    end when;
  end Learner;
end PolicySearch;
