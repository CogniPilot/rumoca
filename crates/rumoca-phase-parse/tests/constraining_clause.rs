//! Constraining-clause modification ownership (MLS §7.3.2).
//!
//! The modifications of a constraining clause apply to the declaration
//! itself. For a replaceable component they merge into the ordinary
//! modification map; for a replaceable short class definition they merge
//! into the alias extends clause. No reserved-prefix side channel exists:
//! a modification key is always a plain member name.

use rumoca_ir_ast as ast;
use rumoca_phase_parse::parse_to_ast;

fn parse(source: &str) -> ast::StoredDefinition {
    parse_to_ast(source, "constraining_clause.mo").expect("source parses")
}

fn component<'a>(def: &'a ast::StoredDefinition, class: &str, name: &str) -> &'a ast::Component {
    def.classes
        .get(class)
        .unwrap_or_else(|| panic!("class `{class}` must exist"))
        .components
        .get(name)
        .unwrap_or_else(|| panic!("component `{name}` must exist in `{class}`"))
}

fn extend_mod_target(expr: &ast::Expression) -> Option<String> {
    match expr {
        ast::Expression::Modification { target, .. }
        | ast::Expression::ClassModification { target, .. } => Some(target.to_string()),
        _ => None,
    }
}

#[test]
fn component_constraining_defaults_merge_as_plain_modifications() {
    let def = parse(
        r#"
model H
  model A
    parameter Real n = 1;
  end A;
  replaceable A a constrainedby A(n = 2);
end H;
"#,
    );
    let a = component(&def, "H", "a");
    assert!(
        a.modifications.contains_key("n"),
        "constraining default must merge under the plain member name, got keys: {:?}",
        a.modifications.keys().collect::<Vec<_>>()
    );
    assert!(
        a.modifications.keys().all(|key| !key.contains("__")),
        "no reserved-prefix key may exist: {:?}",
        a.modifications.keys().collect::<Vec<_>>()
    );
}

#[test]
fn declaration_modification_takes_precedence_over_constraining_default() {
    let def = parse(
        r#"
model H
  model A
    parameter Real n = 1;
  end A;
  replaceable A a(n = 3) constrainedby A(n = 2);
end H;
"#,
    );
    let a = component(&def, "H", "a");
    let n = a
        .modifications
        .get("n")
        .expect("declaration modification must survive");
    let rendered = format!("{n:?}");
    assert!(
        rendered.contains('3') && !rendered.contains('2'),
        "declaration value must win over the constraining default, got: {rendered}"
    );
}

#[test]
fn constraining_each_and_final_flags_carry_on_the_plain_key() {
    let def = parse(
        r#"
model H
  model A
    parameter Real n = 1;
    parameter Real m = 1;
  end A;
  replaceable A a[2] constrainedby A(each n = 2, final m = 3);
end H;
"#,
    );
    let a = component(&def, "H", "a");
    assert!(
        a.each_modifications.contains("n"),
        "each flag must carry on the plain key: {:?}",
        a.each_modifications
    );
    assert!(
        a.final_attributes.contains("m"),
        "final flag must carry on the plain key: {:?}",
        a.final_attributes
    );
}

/// The class arm: `replaceable package Medium = X constrainedby P(nS = 2)`
/// must carry `nS = 2` on the alias extends clause. This is the exact shape
/// the pinned `Modelica.Fluid` Medium templates use.
#[test]
fn class_constraining_defaults_merge_into_the_alias_extends() {
    let def = parse(
        r#"
model H
  package P
    constant Integer nS = 1;
  end P;
  package X
    extends P;
  end X;
  replaceable package Medium = X constrainedby P(nS = 2);
end H;
"#,
    );
    let medium = def
        .classes
        .get("H")
        .expect("H must exist")
        .classes
        .get("Medium")
        .expect("Medium alias must exist");
    assert_eq!(medium.extends.len(), 1, "short class definition alias");
    let targets: Vec<String> = medium.extends[0]
        .modifications
        .iter()
        .filter_map(|em| extend_mod_target(&em.expr))
        .collect();
    assert!(
        targets.iter().any(|target| target == "nS"),
        "constraining modification must reach the alias extends clause, got: {targets:?}"
    );
}

#[test]
fn class_alias_modification_takes_precedence_over_constraining_default() {
    let def = parse(
        r#"
model H
  package P
    constant Integer nS = 1;
  end P;
  package X
    extends P;
  end X;
  replaceable package Medium = X(nS = 3) constrainedby P(nS = 2);
end H;
"#,
    );
    let medium = def
        .classes
        .get("H")
        .expect("H must exist")
        .classes
        .get("Medium")
        .expect("Medium alias must exist");
    let ns_entries: Vec<String> = medium.extends[0]
        .modifications
        .iter()
        .filter_map(|em| extend_mod_target(&em.expr))
        .filter(|target| target == "nS")
        .collect();
    assert_eq!(
        ns_entries.len(),
        1,
        "the alias declaration's own nS must be the only nS entry"
    );
    let rendered = format!("{:?}", medium.extends[0].modifications);
    assert!(
        rendered.contains('3'),
        "the alias declaration's value must survive: {rendered}"
    );
}

/// A class body that is not alias-shaped has no modification slot for the
/// constraining defaults; carrying them silently would change class-body
/// semantics, so the shape is refused.
#[test]
fn long_class_definition_with_constraining_modifications_is_refused() {
    let result = parse_to_ast(
        r#"
model H
  model B
    parameter Real x = 1;
  end B;
  replaceable model M
    parameter Real x = 1;
  end M constrainedby B(x = 2);
end H;
"#,
        "constraining_clause.mo",
    );
    let err = match result {
        Ok(_) => panic!("constraining modifications on a long class definition must be refused"),
        Err(err) => err,
    };
    assert!(
        err.to_string().contains("short class definitions"),
        "refusal must name the supported shape, got: {err}"
    );
}

/// No modification key anywhere in the parsed tree carries a reserved prefix:
/// the constraining-clause side channel does not exist in the representation.
#[test]
fn no_reserved_prefix_key_exists_anywhere() {
    let def = parse(
        r#"
model H
  model A
    parameter Real n = 1;
  end A;
  package P
    constant Integer nS = 1;
  end P;
  package X
    extends P;
  end X;
  replaceable A a constrainedby A(n = 2);
  replaceable package Medium = X constrainedby P(nS = 2);
end H;
"#,
    );
    fn assert_clean(class: &ast::ClassDef) {
        for comp in class.components.values() {
            for key in comp.modifications.keys() {
                assert!(
                    !key.contains("__constrainedby__"),
                    "reserved prefix key found on `{}`: {key}",
                    comp.name
                );
            }
        }
        for nested in class.classes.values() {
            assert_clean(nested);
        }
    }
    for class in def.classes.values() {
        assert_clean(class);
    }
}
