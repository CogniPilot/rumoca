model Atan2Constraint
  parameter Real radialSpeed = 0;
  Real q[2](start={1,0}, each fixed=false);
  Real v[2](start={radialSpeed,1}, each fixed=false);
  Real force;
initial equation
  q[1] = 1;
  v[1] = radialSpeed;
equation
  der(q) = v;
  der(v) = {0,force};
  0 = atan2(q[2],q[1])-time;
  annotation(experiment(StopTime=0.1, Interval=0.01));
end Atan2Constraint;
