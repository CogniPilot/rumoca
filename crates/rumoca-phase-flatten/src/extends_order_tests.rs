//! MLS §5.6.1 extends-position ordering of flattened variables.
//!
//! An `extends`-clause is replaced by the flattened base class *at the position
//! of the extends-clause*, so satisfying an interface by `extends Base` must
//! produce the same flat variable order as declaring the base's members inline.
//! These tests flatten an `extends` form and an equivalent inline form from real
//! source and assert the flattened variable order is identical, which keeps the
//! emitted model and its simulation trace identical between the two spellings.

use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

fn flatten_model(source: &str, model_name: &str) -> flat::Model {
    let file_name = "extends_order_test.mo";
    let stored = rumoca_phase_parse::parse_to_ast(source, file_name).expect("fixture parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("fixture resolves");
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, model_name).expect("fixture instantiates");
    let ast::InstancedTree { tree, mut overlay } = instanced;
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, model_name)
        .expect("fixture typechecks");
    crate::flatten_ref_with_options(
        &tree,
        &overlay,
        model_name,
        crate::FlattenOptions::default(),
    )
    .expect("fixture flattens")
}

/// Flat variable order stripped of the model-name prefix so the extends and
/// inline models compare on their member paths alone.
fn variable_order(model: &flat::Model, model_name: &str) -> Vec<String> {
    let prefix = format!("{model_name}.");
    model
        .variables
        .keys()
        .map(|name| {
            name.as_str()
                .strip_prefix(&prefix)
                .unwrap_or_else(|| name.as_str())
                .to_string()
        })
        .collect()
}

/// A single-level `extends` at the top of the class: the base members already
/// precede the derived members in both forms. This is the case that flattened
/// correctly before the fix and must keep doing so.
const SINGLE_LEVEL: &str = r#"
package P
  partial model Base
    Real a = 1.0;
    Real b = 2.0;
  end Base;

  model ExtendsForm
    extends Base;
    Real c = 3.0;
    Real d = 4.0;
  equation
    der(a) = b;
    der(b) = c;
    der(c) = d;
    der(d) = a;
  end ExtendsForm;

  model InlineForm
    Real a = 1.0;
    Real b = 2.0;
    Real c = 3.0;
    Real d = 4.0;
  equation
    der(a) = b;
    der(b) = c;
    der(c) = d;
    der(d) = a;
  end InlineForm;
end P;
"#;

/// A multi-level chain whose `extends` clauses sit at the top of each class.
/// Before the fix the intermediate base's own members were hoisted ahead of the
/// grandparent's, so `ExtendsForm` flattened as `[m1, m2, g1, g2, l1]` instead
/// of the inline `[g1, g2, m1, m2, l1]`.
const MULTI_LEVEL: &str = r#"
package P
  partial model GrandBase
    Real g1 = 1.0;
    Real g2 = 2.0;
  end GrandBase;

  partial model Mid
    extends GrandBase;
    Real m1 = 3.0;
    Real m2 = 4.0;
  end Mid;

  model ExtendsForm
    extends Mid;
    Real l1 = 5.0;
  equation
    der(g1) = g2;
    der(g2) = m1;
    der(m1) = m2;
    der(m2) = l1;
    der(l1) = g1;
  end ExtendsForm;

  model InlineForm
    Real g1 = 1.0;
    Real g2 = 2.0;
    Real m1 = 3.0;
    Real m2 = 4.0;
    Real l1 = 5.0;
  equation
    der(g1) = g2;
    der(g2) = m1;
    der(m1) = m2;
    der(m2) = l1;
    der(l1) = g1;
  end InlineForm;
end P;
"#;

/// An `extends` clause sitting between own declarations. Before the fix the
/// inherited members were hoisted ahead of every own member, so `ExtendsForm`
/// flattened as `[b, c, a, d]` instead of the inline `[a, b, c, d]`.
const MID_POSITION: &str = r#"
package P
  partial model Base
    Real b = 2.0;
    Real c = 3.0;
  end Base;

  model ExtendsForm
    Real a = 1.0;
    extends Base;
    Real d = 4.0;
  equation
    der(a) = b;
    der(b) = c;
    der(c) = d;
    der(d) = a;
  end ExtendsForm;

  model InlineForm
    Real a = 1.0;
    Real b = 2.0;
    Real c = 3.0;
    Real d = 4.0;
  equation
    der(a) = b;
    der(b) = c;
    der(c) = d;
    der(d) = a;
  end InlineForm;
end P;
"#;

fn assert_extends_matches_inline(source: &str, expected: &[&str]) {
    let extends = variable_order(&flatten_model(source, "P.ExtendsForm"), "P.ExtendsForm");
    let inline = variable_order(&flatten_model(source, "P.InlineForm"), "P.InlineForm");
    assert_eq!(
        inline, expected,
        "inline form must flatten in declaration order"
    );
    assert_eq!(
        extends, inline,
        "extends form must flatten in the same variable order as the inline form (MLS 5.6.1)"
    );
}

#[test]
fn single_level_extends_matches_inline_order() {
    assert_extends_matches_inline(SINGLE_LEVEL, &["a", "b", "c", "d"]);
}

#[test]
fn multi_level_extends_places_grandparent_members_first() {
    assert_extends_matches_inline(MULTI_LEVEL, &["g1", "g2", "m1", "m2", "l1"]);
}

#[test]
fn mid_position_extends_splices_base_members_at_the_clause() {
    assert_extends_matches_inline(MID_POSITION, &["a", "b", "c", "d"]);
}
