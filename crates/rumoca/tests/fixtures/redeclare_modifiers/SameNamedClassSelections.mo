model Redecl
  model Base
    parameter Real a = 1;
    Real y = a;
  end Base;
  model Inner
    replaceable model M = Base;
    M m;
  end Inner;
  model ScopedInner
    replaceable model M = Base;
    M selected;
    Real y = selected.y;
  end ScopedInner;
  model Forwarder
    replaceable model M = Base;
    ScopedInner m(redeclare model M = Base(a = 6));
  end Forwarder;
  model B2 = Base(a = 2);
  B2 plain;
  Inner viaShort(redeclare model M = B2);
  Forwarder viaMod(redeclare model M = Base(a = 4));
end Redecl;
