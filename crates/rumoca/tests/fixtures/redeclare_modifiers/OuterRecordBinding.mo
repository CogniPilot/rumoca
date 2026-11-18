model Redecl
  record Datum
    parameter Real x = 1;
  end Datum;
  model Base
    parameter Datum a;
    Real y = a.x;
  end Base;
  model Inner
    replaceable model M = Base;
    M m;
  end Inner;
  model B2 = Base(a(x = 2));
  B2 plain;
  Inner viaShort(redeclare model M = B2);
  Inner viaMod(redeclare model M = Base(a(x = 4)), m(a = Datum(6)));
end Redecl;
