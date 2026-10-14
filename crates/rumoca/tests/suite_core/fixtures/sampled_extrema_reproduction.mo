model SampledExtremaReproduction
  Real u = time;
  discrete Real y_max;
  discrete Real t_max;
initial equation
  y_max = u;
  t_max = time;
equation
  when sample(0.1, 0.1) then
    y_max = max(u, pre(y_max));
    if y_max > pre(y_max) then
      t_max = time;
    else
      t_max = pre(t_max);
    end if;
  end when;
  annotation(experiment(StopTime=0.3, Interval=0.05));
end SampledExtremaReproduction;
