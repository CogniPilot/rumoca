package 'First'
  type 'StateSelect' = enumeration(never, avoid, default, prefer, always);

  model 'First' "Subset of the BaseModelica First example"
    parameter Real 'amplitude'(unit = "N.m", quantity = "Torque") = 10.0;
    parameter Real 'f'(unit = "Hz", quantity = "Frequency") = 5.0;
    parameter Real 'sine.startTime'(unit = "s", quantity = "Time") = 0.0;
    parameter 'StateSelect' 'inertia1.stateSelect' = 'StateSelect'.default;
    Real 'inertia1.phi'(stateSelect = 'StateSelect'.default, unit = "rad");
    Real 'inertia1.w'(stateSelect = 'StateSelect'.default, unit = "rad/s");
    Real 'inertia1.a'(unit = "rad/s2");
    Real 'sine.y' "Connector of Real output signal";
    parameter Boolean 'torque.useSupport' = true annotation(Evaluate = true);
  equation
    'inertia1.w' = der('inertia1.phi');
    'inertia1.a' = der('inertia1.w');
    'sine.y' = if time < 'sine.startTime' then 0.0 else 'amplitude' * sin(6.283185307179586 * 'f' * (time - 'sine.startTime'));
  end 'First';
end 'First';
