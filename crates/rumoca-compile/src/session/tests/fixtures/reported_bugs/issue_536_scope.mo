package ScopeWitness
  partial model Partial
    parameter Real k;
  end Partial;
  model Full
    extends Partial;
  end Full;
  model Unused
    replaceable model H = Full(k = 5) constrainedby Partial(k(start = 0));
    H h;
  end Unused;
  model Healthy
    Real x;
  equation
    x = 1;
  end Healthy;
end ScopeWitness;
