model Redecl
  model Base
    parameter Real a = 1;
    Real y = a;
  end Base;
  model Selected = Base(a = 6);
  model Inner
    replaceable model M = Base;
    M c;
    Real y = c.y;
  end Inner;
  model Outer
    replaceable model M = Base;
    replaceable Inner m constrainedby Inner(final M);
  end Outer;
  Base plain;
  Outer viaShort;
  Outer viaMod(redeclare model M = Selected);
end Redecl;
