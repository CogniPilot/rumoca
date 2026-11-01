package ConstrainedByFinal
  partial model PartialSpring
    parameter Real k;
  end PartialSpring;
  model LinearSpring
    extends PartialSpring;
  end LinearSpring;
  model Pair
    parameter Real a;
    parameter Real b;
  end Pair;
  partial model PartialPairHolder
    Pair p;
  end PartialPairHolder;
  model PairHolder
    extends PartialPairHolder;
  end PairHolder;
  model TopFinal "The declaration modifies k, which the constraining clause makes final"
    replaceable LinearSpring spring(k = 5) constrainedby PartialSpring(final k = 3);
  end TopFinal;
  model NestedFinal "The declaration modifies p.a, which the constraining clause makes final"
    replaceable PairHolder h(p(a = 5)) constrainedby PartialPairHolder(p(final a = 1, b = 3));
  end NestedFinal;
end ConstrainedByFinal;
