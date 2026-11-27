//! MLS §5.3, §7.3: in `package Medium = Moist; Medium.Probe probe;` the type
//! name is looked up through the local short class `Medium`. Strict
//! compilation prunes the class tree to the classes the target reaches before
//! it resolves again, so the alias it is spelled through must be kept with
//! the class it denotes, whether `Moist` declares, redeclares or only
//! inherits `Probe`. A `replaceable` alias already took another path.

use rumoca::Compiler;

const LIBRARY: &str = r#"
package P
  partial package PartialMedium
    constant Real reference_X[2] = {0.5, 0.5};
    replaceable model Probe
      Real X[2](start = reference_X);
    equation
      der(X) = -X;
    end Probe;
  end PartialMedium;

  package Moist
    extends PartialMedium(reference_X = {0.01, 0.99});
  end Moist;

  package Redeclaring
    extends PartialMedium(reference_X = {0.2, 0.8});
    redeclare model extends Probe
    end Probe;
  end Redeclaring;

  package Moist2 = Moist;

  model Inherited
    package Medium = Moist;
    Medium.Probe probe;
  end Inherited;

  model Redeclared
    package Medium = Redeclaring;
    Medium.Probe probe;
  end Redeclared;

  model TwoAliases
    package Medium = Moist2;
    Medium.Probe probe;
  end TwoAliases;
end P;
"#;

#[test]
fn a_type_spelled_through_a_local_package_alias_compiles() {
    for model in ["P.Inherited", "P.Redeclared", "P.TwoAliases"] {
        let flat = Compiler::new()
            .model(model)
            .compile_str_flat(LIBRARY, "P.mo")
            .unwrap_or_else(|error| panic!("{model} flattens: {error}"));
        assert!(
            flat.variables
                .contains_key(&rumoca_core::VarName::new("probe.X")),
            "{model} has probe.X"
        );
    }
}
