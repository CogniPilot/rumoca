model AngularRateContact
  function rateMap
    input Real theta;
    input Real rate[2];
    output Real omega[2];
  algorithm
    omega := {rate[1] + theta*rate[2], rate[2]};
    annotation(Inline=true);
  end rateMap;
  parameter Real theta0 = 0.2;
  Real theta[2](start={theta0,0.3}, each fixed=true, each stateSelect=StateSelect.always);
  Real omega[2](start={1,1}, each fixed=true, each stateSelect=StateSelect.always);
  Real rate[2];
  Real z(start=cos(theta0));
  Real vz(start=-(1-theta0)*sin(theta0));
  Real force;
equation
  der(theta) = rate;
  der(omega) = zeros(2);
  omega = rateMap(theta[1], rate);
  der(z) = vz;
  der(vz) = force;
  z = cos(theta[1]);
end AngularRateContact;
