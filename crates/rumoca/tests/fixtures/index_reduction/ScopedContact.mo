model ScopedContact
  function wave
    input Real q;
    output Real value;
  algorithm
    value := cos(q);
  end wave;
  function pair
    input Real q;
    output Real value;
  algorithm
    value := wave(q) + wave(2*q);
  end pair;
  Real theta(start=0.2, fixed=true, stateSelect=StateSelect.always);
  Real z;
  Real vz;
  Real force;
equation
  der(theta) = 1;
  der(z) = vz;
  der(vz) = force;
  z = pair(theta) + pair(theta + 0.3);
end ScopedContact;
