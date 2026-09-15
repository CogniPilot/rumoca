model CoupledRateConstraint
  Real q[2](start={0.2,0.4});
  Real omega[2](start={0.6,1.4});
  Real theta(start=0.6, fixed=true, stateSelect=StateSelect.always);
  Real force[2];
initial equation
  q[1] = 0.2;
  omega[1] = 0.6;
equation
  {{4,-1},{-2,3}}*der(q) = omega;
  der(omega) = force;
  der(theta) = 1;
  0 = q[1] + q[2] - theta;
  force[1] - force[2] = 0;
annotation(experiment(StopTime=0.1, Interval=0.01, Tolerance=1e-8));
end CoupledRateConstraint;
