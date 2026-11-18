model Redecl
  model Inner
    parameter Real a = 1;
    Real y = a;
  end Inner;
  model Outer
    parameter Real a = 2;
    Inner m(final a);
  end Outer;
  Inner plain;
  Outer viaShort;
  Outer viaMod(a = 6);
end Redecl;
