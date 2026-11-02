package Long
  partial model P
    parameter Real k;
  end P;
  model A
    parameter Real a = 2;
  end A;
  model C
    replaceable model M
      extends A;
      parameter Real k;
    end M constrainedby P(k = 1);
    M h;
  end C;
  model Inherited
    replaceable model N
      extends A;
      parameter Real k = 3;
    end N constrainedby A(a = 5);
    N h;
  end Inherited;
  model Healthy
    Real x;
  equation
    x = 1;
  end Healthy;
end Long;
