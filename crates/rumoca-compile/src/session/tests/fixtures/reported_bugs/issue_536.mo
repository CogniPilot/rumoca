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
end ConstrainedByMod;
