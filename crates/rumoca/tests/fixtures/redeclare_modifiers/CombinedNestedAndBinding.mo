model Redecl
  model Base
    parameter Real a = 1;
    Real y = a;
  end Base;
  model Inner
    replaceable model M = Base;
    M m;
  end Inner;
  model B2 = Base(a = 2);
  B2 plain;
  Inner viaShort(redeclare model M = B2);
  Inner viaMod(redeclare model M = Base(a(start = 3) = 4));
end Redecl;
