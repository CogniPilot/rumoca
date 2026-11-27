model TimedCrossing "Cross scheduled ticks and a state event while solving a resistive chain"
  input Real rate=1;
  Real position(start=0, fixed=true);
  discrete Real tickCount(start=0, fixed=true);
  discrete Real crossingTime(start=-1, fixed=true);
protected
  Real node[12];
equation
  // All nodes settle to the input voltage. The solver retains the factor of
  // the coupled conductance matrix as each lane advances its own position.
  (rate - node[1]) / 0.1 + node[2] - node[1] = (node[1] - rate) / 10;
  for i in 2:11 loop
    node[i-1] + node[i+1] - 2 * node[i] = (node[i] - rate) / 10;
  end for;
  node[11] - node[12] = (node[12] - rate) / 10;
  der(position)=node[12];
  when sample(.005,.01) then
    tickCount=pre(tickCount)+1;
  end when;
  when position>=.011 then
    crossingTime=time;
  end when;
end TimedCrossing;
