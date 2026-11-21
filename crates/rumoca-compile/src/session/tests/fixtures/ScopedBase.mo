package Library
  import BaseAlias = Library.Base;

  model Base
    Real localOnly = 1;
  end Base;

  model Derived
    extends Base;
  end Derived;

  model Imported
    extends BaseAlias;
  end Imported;

  encapsulated model Global
    import GlobalBase = Base;
    extends GlobalBase;
  end Global;
end Library;

model Base
  Real unrelated = 2;
end Base;
