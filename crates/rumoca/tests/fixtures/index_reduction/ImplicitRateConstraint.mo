model ImplicitRateConstraint
  Real q[2](start={0.2,0.4}, each fixed=true, each stateSelect=StateSelect.always);
  Real omega[2](start={0.6,1.2}, each fixed=true, each stateSelect=StateSelect.always);
  Real z(start=0.2);
  Real v(start=0.2);
  Real force;
equation
  2*der(q) + q = omega;
  der(omega) = {0,0};
  der(z) = v;
  der(v) = force;
  0 = z - q[1];
annotation(experiment(StopTime=0.1, Interval=0.01, Tolerance=1e-8));
end ImplicitRateConstraint;
