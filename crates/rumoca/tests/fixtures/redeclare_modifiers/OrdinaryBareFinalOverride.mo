model Redecl
  model Inner
    parameter Real a = 1;
    Real y = a;
  end Inner;
  model Outer
    Inner m(final a);
  end Outer;
  Inner plain;
  Outer viaShort;
  Outer viaMod(m(a = 6));
end Redecl;
