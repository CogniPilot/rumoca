package ConstrainedByMod
  partial model PartialSpring "Constraining type: declares k but gives it no value"
    parameter Real k "Stiffness";
    Real x "Deflection";
    Real f "Force";
  equation
    f = k*x;
  end PartialSpring;

  model LinearSpring "Default type: adds nothing to the parameters"
    extends PartialSpring;
  end LinearSpring;

  model Holder "The constrainedby modifier is the only value of k"
    parameter Real kHolder = 3;
    replaceable LinearSpring spring constrainedby PartialSpring(k = kHolder);
  equation
    spring.x = time;
  end Holder;

  model HolderRedeclared "Redeclaration: the constrainedby modifier must apply too"
    extends Holder(redeclare LinearSpring spring);
  end HolderRedeclared;

  model ClassForm "Replaceable short class definition"
    replaceable model Spring = LinearSpring constrainedby PartialSpring(k = 3);
    Spring spring;
  equation
    spring.x = time;
  end ClassForm;
  model DeclarationFirst "A declaration modifier takes precedence over the constraining clause"
    replaceable LinearSpring spring(k = 5) constrainedby PartialSpring(k = 3);
  equation
    spring.x = time;
  end DeclarationFirst;
  model Pair "Two parameters of one nested element"
    parameter Real a;
    parameter Real b;
  end Pair;
  partial model PartialPairHolder
    Pair p;
  end PartialPairHolder;
  model PairHolder
    extends PartialPairHolder;
  end PairHolder;
  model NestedMerge "The declared p.a wins over the constrained one; the constrained p.b survives"
    replaceable PairHolder h(p(a = 5)) constrainedby PartialPairHolder(p(a = 1, b = 3));
  end NestedMerge;
  model NestedClassForm "The same merge for a replaceable short class"
    replaceable model H = PairHolder(p(a = 5)) constrainedby PartialPairHolder(p(a = 1, b = 3));
    H h;
  end NestedClassForm;
  model NestedFinalKept "A final constrained p.a stays; the declaration modifies only p.b"
    replaceable PairHolder h(p(b = 4)) constrainedby PartialPairHolder(p(final a = 1, b = 3));
  end NestedFinalKept;
  model NestedFinalClassKept "The same for a replaceable short class"
    replaceable model H = PairHolder(p(b = 4)) constrainedby PartialPairHolder(p(final a = 1, b = 3));
    H h;
  end NestedFinalClassKept;
  record R
    Real a;
    Real b;
  end R;
  partial model PartialRHolder
    parameter R p;
  end PartialRHolder;
  model RHolder
    extends PartialRHolder;
  end RHolder;
  model RecordKept "The declared whole record wins over a non-final constrained member, as in OpenModelica"
    replaceable RHolder h(p = R(5, 4)) constrainedby PartialRHolder(p(b = 3));
  end RecordKept;
  model RecordDirect "Not #536: a whole record binding and a member modifier written together"
    RHolder h(p(b = 3) = R(5, 4));
  end RecordDirect;
  partial model PartialGain "One parameter for attribute modifiers"
    parameter Real k;
  end PartialGain;
  model Gain
    extends PartialGain;
  end Gain;
  model ExtendsAttributes "An extends modifier that states attributes and a binding"
    extends Gain(k(min = 0, unit = "N/m") = 5);
  end ExtendsAttributes;
  model ClassAttributes "The same modifier on a short class"
    model G = Gain(k(min = 0, unit = "N/m") = 5);
    G g;
  end ClassAttributes;
  model AttributeKept "A declared binding keeps the constrained attributes"
    replaceable Gain h(k = 5) constrainedby PartialGain(k(min = 0, unit = "N/m"));
  end AttributeKept;
  model AttributeFinalKept "A declared binding does not modify a final constrained attribute"
    replaceable Gain h(k = 5) constrainedby PartialGain(k(final unit = "N/m"));
  end AttributeFinalKept;
end ConstrainedByMod;
