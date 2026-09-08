//! Route-equivalent constraining-clause semantics (MLS §7.3.2).
//!
//! The same replaceable slot reached through the default route, the
//! extends-redeclare route, and the modifier-redeclare route must produce
//! identical Flat semantics: the constraining-clause default applies on
//! every route, and a declaration or redeclaration modification overrides it
//! on every route.

use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

fn typed_flat_model(source: &str, model: &str) -> flat::Model {
    let file_name = "<constrainedby_route_parity>";
    let stored = rumoca_phase_parse::parse_to_ast(source, file_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let overlay =
        match rumoca_phase_instantiate::instantiate_model_with_outcome(resolved.inner(), model) {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
    let typed = rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, model)
        .expect("instanced model typechecks");
    rumoca_phase_flatten::flatten_typed(typed, rumoca_phase_flatten::FlattenOptions::default())
        .expect("typed model flattens")
}

fn binding_value(model: &flat::Model, name: &str) -> f64 {
    let variable = model
        .variables
        .iter()
        .find(|(candidate, _)| candidate.as_str() == name)
        .map(|(_, variable)| variable)
        .unwrap_or_else(|| panic!("missing flat variable `{name}`"))
        .clone();
    let binding = variable
        .binding
        .as_ref()
        .unwrap_or_else(|| panic!("flat variable `{name}` must carry a binding"));
    literal_f64(binding)
        .unwrap_or_else(|| panic!("binding of `{name}` must be a literal: {binding:?}"))
}

fn literal_f64(expr: &rumoca_core::Expression) -> Option<f64> {
    match expr {
        rumoca_core::Expression::Literal { value, .. } => match value {
            rumoca_core::Literal::Real(real) => Some(*real),
            rumoca_core::Literal::Integer(integer) => {
                let converted = *integer as f64;
                (converted as i64 == *integer).then_some(converted)
            }
            _ => None,
        },
        _ => None,
    }
}

/// The measured probe fixture: `P.MD` (default), `P.ME` (extends-redeclare
/// of the same class), `P.ME2` (modifier-redeclare of the same class). All
/// three routes must yield `h.a.n = 2`, the constraining default.
const PROBE: &str = r#"
package P
  model A
    parameter Real n = 1;
  end A;
  model HD
    replaceable A a constrainedby A(n = 2);
  end HD;
  model HE
    extends HD(redeclare A a);
  end HE;
  model MD
    HD h;
  end MD;
  model ME
    HE h;
  end ME;
  model ME2
    HD h(redeclare A a);
  end ME2;
end P;
"#;

#[test]
fn default_route_applies_the_constraining_default() {
    let model = typed_flat_model(PROBE, "P.MD");
    assert_eq!(binding_value(&model, "h.a.n"), 2.0);
}

#[test]
fn extends_redeclare_route_applies_the_constraining_default() {
    let model = typed_flat_model(PROBE, "P.ME");
    assert_eq!(binding_value(&model, "h.a.n"), 2.0);
}

#[test]
fn modifier_redeclare_route_applies_the_constraining_default() {
    let model = typed_flat_model(PROBE, "P.ME2");
    assert_eq!(binding_value(&model, "h.a.n"), 2.0);
}

#[test]
fn all_three_routes_agree_on_the_flat_value() {
    let default_route = binding_value(&typed_flat_model(PROBE, "P.MD"), "h.a.n");
    let extends_route = binding_value(&typed_flat_model(PROBE, "P.ME"), "h.a.n");
    let modifier_route = binding_value(&typed_flat_model(PROBE, "P.ME2"), "h.a.n");
    assert_eq!(default_route, extends_route);
    assert_eq!(extends_route, modifier_route);
    assert_eq!(default_route, 2.0);
}

/// A redeclaration modification overrides the constraining default on both
/// explicit routes, and the declaration modification overrides it on the
/// default route.
#[test]
fn overriding_modifications_win_over_the_constraining_default_on_every_route() {
    let source = r#"
package P
  model A
    parameter Real n = 1;
  end A;
  model HD
    replaceable A a constrainedby A(n = 2);
  end HD;
  model HDECL
    replaceable A a(n = 4) constrainedby A(n = 2);
  end HDECL;
  model MDECL
    HDECL h;
  end MDECL;
  model MEXT
    extends HD;
  end MEXT;
  model ME2M
    HD h(redeclare A a(n = 5));
  end ME2M;
end P;
"#;
    assert_eq!(
        binding_value(&typed_flat_model(source, "P.MDECL"), "h.a.n"),
        4.0,
        "declaration modification wins on the default route"
    );
    assert_eq!(
        binding_value(&typed_flat_model(source, "P.ME2M"), "h.a.n"),
        5.0,
        "modifier-redeclare modification wins over the constraining default"
    );
}

/// Reduced pinned-Fluid shapes: the component-arm constraining clause with a
/// parenthesized modification (the `PartialLogicalClock` / pump
/// characteristics shape), exercised end to end through Flat.
#[test]
fn component_arm_fluid_shape_carries_the_constraining_modification() {
    let source = r#"
package P
  model PartialCombiner
    parameter Integer nu = 0;
    parameter Real y = nu;
  end PartialCombiner;
  model Clockwork
    parameter Integer nu = 3;
    replaceable PartialCombiner combiner constrainedby PartialCombiner(nu = nu);
  end Clockwork;
  model M
    Clockwork c;
  end M;
end P;
"#;
    let model = typed_flat_model(source, "P.M");
    let variable = model
        .variables
        .iter()
        .find(|(candidate, _)| candidate.as_str() == "c.combiner.nu")
        .map(|(_, variable)| variable)
        .expect("missing flat variable `c.combiner.nu`");
    assert!(
        variable.binding.is_some(),
        "the constraining modification `nu = nu` must reach the flat binding"
    );
}
