model CircleChart
  Real q[2](start={1,0});
  Real v[2](start={0,1});
  Real lambda;
initial equation
  q[2]=0;
  v[2]=1;
equation
  der(q)=v;
  der(v)=lambda*q;
  q*q=1;
  annotation(experiment(StopTime=6.283185307179586,Tolerance=1e-8,Interval=0.01));
end CircleChart;
