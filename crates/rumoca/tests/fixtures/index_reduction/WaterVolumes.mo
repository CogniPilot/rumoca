model WaterVolumes
  model Med
    Real T(stateSelect = StateSelect.prefer);
    Real u;
    Real d;
  equation
    u = 4184*(T - 273.15);
    d = 995;
  end Med;
  parameter Integer n = 2;
  Med mediums[n];
  Real ms[n];
  Real Us[n];
  Real mb[n];
  Real Hb[n];
  Real m[n + 1];
  Real H[n + 1];
  parameter Real V[n] = {5, 10};
initial equation
  mediums[1].T = 300;
  mediums[2].T = 300;
equation
  for i in 1:n loop
    ms[i] = V[i]*mediums[i].d;
  end for;
  for i in 1:n loop
    Us[i] = ms[i]*mediums[i].u;
  end for;
  der(Us[1]) = Hb[1];
  der(Us[2]) = Hb[2];
  der(ms[1]) = mb[1];
  der(ms[2]) = mb[2];
  mb[1] = m[1] - m[2];
  mb[2] = m[2] - m[3];
  Hb[1] = H[1] - H[2];
  Hb[2] = H[2] - H[3];
  m[1] = sin(time);
  H[1] = m[1]*4184*20;
  H[2] = m[2]*mediums[1].u;
  H[3] = m[3]*mediums[2].u;
  annotation(experiment(StopTime = 1));
end WaterVolumes;
