model Redecl
  model Base
    parameter Real a = 1;
    Real y = a;
  end Base;
  model Inner
    replaceable model M = Outer.M(a = 1) constrainedby Base;
    M m;
  end Inner;
  model Outer
    model M = Base(a = 6);
    Inner q(redeclare model M = M);
    Real y = q.m.y;
  end Outer;
  model Wrapper
    Outer m;
  end Wrapper;
  Base plain;
  Inner viaShort;
  Wrapper viaMod;
end Redecl;
