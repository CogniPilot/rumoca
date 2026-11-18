model Redecl
  model Inner
    parameter Real a = 1;
    Real y = a;
  end Inner;
  model Outer
    parameter Real a = 2;
    Inner m;
  end Outer;
  model Derived
    extends Outer(m(final a));
  end Derived;
  Inner plain;
  Outer viaShort;
  Derived viaMod(a = 6);
end Redecl;
