
package SampledCounters
  model Counter
    discrete Integer state(start=0,fixed=true);
    discrete Real timer(start=0,fixed=true);
  algorithm
    when sample(5,5) then
      state:=pre(state); timer:=pre(timer);
      if pre(state)==0 then state:=1;
      elseif pre(state)==1 then state:=2;
      elseif pre(state)==2 then state:=3;
      end if;
      timer:=pre(timer)+5;
    end when;
  end Counter;
  model Network
    Counter first; Counter second;
    Integer aliasState=first.state;
  end Network;
end SampledCounters;
