model TimedCrossing "Cross scheduled ticks and a state event in the same co-simulation step"
  input Real rate=1;
  Real position(start=0, fixed=true);
  discrete Real tickCount(start=0, fixed=true);
  discrete Real crossingTime(start=-1, fixed=true);
equation
  der(position)=rate;
  when sample(.005,.01) then
    tickCount=pre(tickCount)+1;
  end when;
  when position>=.011 then
    crossingTime=time;
  end when;
end TimedCrossing;
