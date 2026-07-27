//! P4 parser allocation work: grammar payloads are moved out of the parse tree
//! instead of deep-cloned at every nesting level. These tests fail loudly if a
//! payload is taken twice (and silently emptied) or dropped on the floor.

use rumoca_ir_ast::{ClassDef, StoredDefinition};
use rumoca_phase_parse::parse_to_ast;

const DEEPLY_NESTED: &str = r#"
model L0
  import Modelica.SIunits;
  extends L0Base;
  Real l0x;

  model L1
    import Modelica.Constants;
    extends L1Base;
    Real l1x;

    model L2
      import Modelica.Math;
      extends L2Base;
      Real l2x;

      model L3
        import Modelica.Utilities;
        extends L3Base;
        Real l3x;
      equation
        l3x = 1.0;
      initial equation
        l3x = 0.0;
      algorithm
        l3x := 1.0;
      initial algorithm
        l3x := 2.0;
        annotation(Documentation(info = "l3"));
      end L3;
    equation
      l2x = 1.0;
    initial equation
      l2x = 0.0;
    algorithm
      l2x := 1.0;
    initial algorithm
      l2x := 2.0;
      annotation(Documentation(info = "l2"));
    end L2;
  equation
    l1x = 1.0;
  initial equation
    l1x = 0.0;
  algorithm
    l1x := 1.0;
  initial algorithm
    l1x := 2.0;
    annotation(Documentation(info = "l1"));
  end L1;
equation
  l0x = 1.0;
initial equation
  l0x = 0.0;
algorithm
  l0x := 1.0;
initial algorithm
  l0x := 2.0;
  annotation(Documentation(info = "l0"));
end L0;
"#;

fn nested<'a>(def: &'a StoredDefinition, path: &[&str]) -> &'a ClassDef {
    let mut current = def
        .classes
        .get(path[0])
        .unwrap_or_else(|| unreachable!("missing top-level class {}", path[0]));
    for segment in &path[1..] {
        current = current
            .classes
            .get(*segment)
            .unwrap_or_else(|| unreachable!("missing nested class {segment}"));
    }
    current
}

fn assert_level_is_complete(class: &ClassDef, level: &str) {
    let var = format!("{level}x");
    assert!(
        class.components.contains_key(var.as_str()),
        "{level}: component `{var}` was lost"
    );
    assert_eq!(class.equations.len(), 1, "{level}: equation was lost");
    assert_eq!(
        class.initial_equations.len(),
        1,
        "{level}: initial equation was lost"
    );
    assert_eq!(class.algorithms.len(), 1, "{level}: algorithm was lost");
    assert_eq!(
        class.initial_algorithms.len(),
        1,
        "{level}: initial algorithm was lost"
    );
    assert_eq!(class.imports.len(), 1, "{level}: import was lost");
    assert_eq!(class.extends.len(), 1, "{level}: extends was lost");
    assert!(!class.annotation.is_empty(), "{level}: annotation was lost");
    assert!(
        class.equation_keyword.is_some(),
        "{level}: equation keyword token was lost"
    );
    assert!(
        class.initial_equation_keyword.is_some(),
        "{level}: initial equation keyword token was lost"
    );
    assert!(
        class.algorithm_keyword.is_some(),
        "{level}: algorithm keyword token was lost"
    );
    assert!(
        class.initial_algorithm_keyword.is_some(),
        "{level}: initial algorithm keyword token was lost"
    );
    let initial_statement = class
        .initial_algorithms
        .first()
        .and_then(|section| section.first())
        .unwrap_or_else(|| unreachable!("{level}: initial algorithm section is empty"));
    assert!(
        matches!(
            initial_statement,
            rumoca_ir_ast::Statement::Assignment { .. }
        ),
        "{level}: initial algorithm statement was replaced, got {initial_statement:?}"
    );
}

#[test]
fn deeply_nested_package_preserves_every_member() {
    let def = parse_to_ast(DEEPLY_NESTED, "nested.mo").expect("source should parse");
    assert_level_is_complete(nested(&def, &["L0"]), "l0");
    assert_level_is_complete(nested(&def, &["L0", "L1"]), "l1");
    assert_level_is_complete(nested(&def, &["L0", "L1", "L2"]), "l2");
    assert_level_is_complete(nested(&def, &["L0", "L1", "L2", "L3"]), "l3");
}

const VISIBILITY_SECTIONS: &str = r#"
model Sections
  Real pubDefault;
public
  Real pubExplicit;
  model PubClass
  end PubClass;
  extends PubBase;
protected
  Real prot;
  model ProtClass
  end ProtClass;
  extends ProtBase;
equation
  pubDefault = 1.0;
algorithm
  pubExplicit := 2.0;
end Sections;
"#;

#[test]
fn protected_and_public_sections_both_survive_move_out() {
    let def = parse_to_ast(VISIBILITY_SECTIONS, "sections.mo").expect("source should parse");
    let class = nested(&def, &["Sections"]);

    for name in ["pubDefault", "pubExplicit", "prot"] {
        assert!(
            class.components.contains_key(name),
            "component `{name}` was lost"
        );
    }
    assert!(!class.components["pubDefault"].is_protected);
    assert!(!class.components["pubExplicit"].is_protected);
    assert!(
        class.components["prot"].is_protected,
        "protected component must keep its visibility flag"
    );

    for name in ["PubClass", "ProtClass"] {
        assert!(class.classes.contains_key(name), "class `{name}` was lost");
    }
    assert!(!class.classes["PubClass"].is_protected);
    assert!(
        class.classes["ProtClass"].is_protected,
        "protected nested class must keep its visibility flag"
    );

    assert_eq!(class.extends.len(), 2, "one extends clause was lost");
    assert!(!class.extends[0].is_protected);
    assert!(
        class.extends[1].is_protected,
        "protected extends must keep its visibility flag"
    );

    assert_eq!(class.equations.len(), 1);
    assert_eq!(class.algorithms.len(), 1);
}

#[test]
fn repeated_visibility_sections_all_merge() {
    let source = r#"
model Repeat
public
  Real a;
protected
  Real b;
public
  Real c;
protected
  Real d;
end Repeat;
"#;
    let def = parse_to_ast(source, "repeat.mo").expect("source should parse");
    let class = nested(&def, &["Repeat"]);
    assert_eq!(class.components.len(), 4);
    assert!(!class.components["a"].is_protected);
    assert!(class.components["b"].is_protected);
    assert!(!class.components["c"].is_protected);
    assert!(class.components["d"].is_protected);
}

const EXTERNAL_FUNCTIONS: &str = r#"
package Ext
  function Direct
    input Real u;
    output Real y;
  external "C" y = direct_call(u);
  end Direct;

  package Inner
    function Nested
      input Real u;
      input Real v;
      output Real y;
    external "FORTRAN 77" nested_call(u, v);
    end Nested;
  end Inner;
end Ext;
"#;

#[test]
fn external_declarations_survive_move_out_at_every_depth() {
    // MLS §12.9: the external clause is the last payload `convert_composition`
    // takes; a double take or a dropped take leaves the function body empty.
    let def = parse_to_ast(EXTERNAL_FUNCTIONS, "external.mo").expect("source should parse");

    let direct = nested(&def, &["Ext", "Direct"])
        .external
        .as_ref()
        .unwrap_or_else(|| unreachable!("top-level external clause was lost"));
    assert_eq!(direct.language.as_deref(), Some("C"));
    assert_eq!(
        direct
            .function_name
            .as_ref()
            .map(|token| token.text.as_ref()),
        Some("direct_call")
    );
    assert!(
        direct.output.is_some(),
        "external output binding was dropped"
    );
    assert_eq!(direct.args.len(), 1, "external argument was dropped");

    let deep = nested(&def, &["Ext", "Inner", "Nested"])
        .external
        .as_ref()
        .unwrap_or_else(|| unreachable!("nested external clause was lost"));
    assert_eq!(deep.language.as_deref(), Some("FORTRAN 77"));
    assert_eq!(
        deep.function_name.as_ref().map(|token| token.text.as_ref()),
        Some("nested_call")
    );
    assert!(
        deep.output.is_none(),
        "a call-form external clause has no output binding"
    );
    assert_eq!(deep.args.len(), 2, "external arguments were dropped");
}

#[test]
fn extends_class_specifier_keeps_its_composition() {
    // MLS §4.5 long-class-specifier, `extends` form: the composition payload is
    // consumed by `convert_extends_class_specifier`, a second take-site.
    let source = r#"
model extends Derived(gain = 2)
  Real extra;
equation
  extra = 3.0;
end Derived;
"#;
    let def = parse_to_ast(source, "derived.mo").expect("source should parse");
    let class = nested(&def, &["Derived"]);
    assert!(
        class.components.contains_key("extra"),
        "extends-form component was lost"
    );
    assert_eq!(class.equations.len(), 1, "extends-form equation was lost");
    assert_eq!(
        class.extends.len(),
        1,
        "the synthesized inherited extends clause is missing"
    );
}
