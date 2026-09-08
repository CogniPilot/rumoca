//! Parameter dependency ordering: sibling references carry occurrence identity
//! through comprehension expansion, and identity-free references stay refused.

use super::*;
use rumoca_core::{
    ComponentRefPart, ComponentReference, DefId, ExpressionVisitor, InstanceId, Reference,
    SourceId, Span,
};
use rumoca_eval_flat::constant::{ResolvedEnumCatalog, ResolvedOccurrenceKey};

const SOURCE: &str = "
package Lib
  model Phasor
    parameter Real inverse[m, 2] = {{cos(-phi[k]), -sin(-phi[k])} for k in 1:m};
    parameter Real scaled[m] = {gain*k for k in 1:m};
    parameter Integer m(min = 1) = 3 annotation(Evaluate = true);
    parameter Real phi[m] = {(k - 1)/m for k in 1:m};
    parameter Real gain = 2.0;
    Real x[m](each start = 0);
  equation
    der(x) = inverse[:, 1] + scaled - x;
  end Phasor;

  model Top
    Phasor p;
  end Top;
end Lib;
";

fn flatten_source(model: &str) -> Model {
    let file_name = "<parameter_dependency_tests>";
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, file_name).expect("fixture should parse");
    let mut tree = rumoca_ir_ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, SOURCE);
    let resolved = rumoca_phase_resolve::resolve(rumoca_ir_ast::ParsedTree::new(tree))
        .expect("fixture should resolve");
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
        .expect("fixture should typecheck");
    crate::flatten_typed(typed, crate::FlattenOptions::default()).expect("fixture should flatten")
}

fn dims_of(flat: &Model, name: &str) -> Vec<i64> {
    flat.variables
        .iter()
        .find(|(var_name, _)| var_name.as_str() == name)
        .map(|(_, variable)| variable.dims.clone())
        .unwrap_or_else(|| {
            let known: Vec<&str> = flat.variables.keys().map(|key| key.as_str()).collect();
            panic!("no flat variable `{name}`; got {known:?}")
        })
}

fn identity_of(variables: &[ResolvedFlatVariable<'_>], name: &str) -> ResolvedOccurrenceKey {
    variables
        .iter()
        .find(|entry| entry.name.as_str() == name)
        .map(|entry| entry.identity)
        .unwrap_or_else(|| panic!("no resolved flat variable `{name}`"))
}

fn exact_dependencies(
    binding: &ParamBinding<'_>,
    catalog: &ResolvedEnumCatalog,
) -> Vec<ResolvedOccurrenceKey> {
    let mut collector = ExactDependencyCollector::new(catalog);
    collector.visit_expression(binding.binding);
    assert_eq!(
        collector.malformed_reference, None,
        "binding of `{}` must carry identity on every reference",
        binding.name
    );
    collector.dependencies
}

fn position(ordered: &[ParamBinding<'_>], name: &str) -> usize {
    ordered
        .iter()
        .position(|binding| binding.name == name)
        .unwrap_or_else(|| panic!("`{name}` missing from ordered parameters"))
}

fn assert_dependency_order(ordered: &[ParamBinding<'_>]) {
    let m = position(ordered, "p.m");
    let phi = position(ordered, "p.phi");
    let inverse = position(ordered, "p.inverse");
    let gain = position(ordered, "p.gain");
    let scaled = position(ordered, "p.scaled");
    let names: Vec<&str> = ordered.iter().map(|binding| binding.name).collect();
    assert!(m < phi, "`p.m` must be evaluated before `p.phi`: {names:?}");
    assert!(
        phi < inverse,
        "`p.phi` must be evaluated before `p.inverse`: {names:?}"
    );
    assert!(
        gain < scaled,
        "`p.gain` must be evaluated before `p.scaled`: {names:?}"
    );
}

/// Sibling references inside expanded comprehension bodies keep their
/// occurrence identity, so the exact dependency graph orders `m` before
/// `phi` (value dependency in the body) and `phi` before `inverse` (element
/// selection in the body), and `gain` before `scaled`. The dependents are
/// declared first so a vacuous ordering would leave them first.
#[test]
fn expanded_comprehension_bindings_order_sibling_parameters_by_identity() {
    let flat = flatten_source("Lib.Top");
    assert_eq!(dims_of(&flat, "p.inverse"), vec![3, 2]);
    assert_eq!(dims_of(&flat, "p.scaled"), vec![3]);
    assert_eq!(dims_of(&flat, "p.phi"), vec![3]);

    let variables =
        Context::collect_flat_variables(&flat).expect("every flat variable is resolved");
    let mut ctx = Context::new();
    let params = ctx.collect_parameters(&flat, &variables);
    let catalog = ResolvedEnumCatalog::empty();

    let param = |name: &str| {
        params
            .iter()
            .find(|binding| binding.name == name)
            .copied()
            .unwrap_or_else(|| panic!("`{name}` is not a collected parameter"))
    };
    assert!(
        exact_dependencies(&param("p.phi"), &catalog).contains(&identity_of(&variables, "p.m")),
        "`p.phi` body references `m`"
    );
    assert!(
        exact_dependencies(&param("p.inverse"), &catalog)
            .contains(&identity_of(&variables, "p.phi")),
        "`p.inverse` body references `phi[k]`"
    );
    assert!(
        exact_dependencies(&param("p.scaled"), &catalog)
            .contains(&identity_of(&variables, "p.gain")),
        "`p.scaled` body references `gain`"
    );

    let ordered = dependency_ordered_parameters(&params, &catalog).expect("acyclic");
    assert_dependency_order(&ordered);
    let mut reversed = params.clone();
    reversed.reverse();
    let ordered = dependency_ordered_parameters(&reversed, &catalog).expect("acyclic");
    assert_dependency_order(&ordered);
}

fn span() -> Span {
    Span::from_offsets(
        SourceId::from_source_name("parameter_dependency_tests.mo"),
        0,
        1,
    )
}

fn declared_reference(name: &str, def_id: u32) -> Reference {
    let reference = ComponentReference::construct(
        false,
        span(),
        vec![ComponentRefPart {
            ident: name.to_string(),
            span: span(),
            subs: Vec::new(),
            def_id: DefId::new(def_id),
        }],
    )
    .expect("single-part reference");
    Reference::with_component_reference(name, reference)
}

fn var_ref(name: Reference) -> Expression {
    Expression::VarRef {
        name,
        subscripts: Vec::new(),
        span: span(),
    }
}

fn binding<'a>(
    name: &'a str,
    identity: ResolvedOccurrenceKey,
    expression: &'a Expression,
) -> ParamBinding<'a> {
    ParamBinding {
        name,
        identity,
        binding: expression,
        primitive: ParamPrimitive::Real,
        may_be_record_alias: false,
        binding_from_modification: false,
    }
}

fn literal(value: f64) -> Expression {
    Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: span(),
    }
}

/// A reference that carries its declaration but no occurrence is exactly the
/// shape a re-lowered binding has before scope attachment; ordering must keep
/// refusing it rather than treating the binding as dependency-free.
#[test]
fn declaration_only_reference_is_refused_as_identity_free() {
    let scope = InstanceId::new(7);
    let source = literal(1.0);
    let dependent = var_ref(declared_reference("a", 11));
    let params = [
        binding(
            "b",
            ResolvedOccurrenceKey {
                instance_id: scope,
                root_def_id: DefId::new(12),
            },
            &dependent,
        ),
        binding(
            "a",
            ResolvedOccurrenceKey {
                instance_id: scope,
                root_def_id: DefId::new(11),
            },
            &source,
        ),
    ];
    let Err(error) = dependency_ordered_parameters(&params, &ResolvedEnumCatalog::empty()) else {
        panic!("a declaration-only reference has no occurrence identity and must be refused");
    };
    let message = error.to_string();
    assert!(
        message.contains("post-Resolve parameter binding `b`")
            && message.contains("identity-free reference `a`"),
        "unexpected error: {message}"
    );
}

/// A reference with no resolved declaration at all is refused the same way.
#[test]
fn unresolved_reference_is_refused_as_identity_free() {
    let scope = InstanceId::new(7);
    let dependent = var_ref(Reference::new("a"));
    let params = [binding(
        "b",
        ResolvedOccurrenceKey {
            instance_id: scope,
            root_def_id: DefId::new(12),
        },
        &dependent,
    )];
    let Err(error) = dependency_ordered_parameters(&params, &ResolvedEnumCatalog::empty()) else {
        panic!("an unresolved reference has no identity and must be refused");
    };
    let message = error.to_string();
    assert!(
        message.contains("identity-free reference `a`"),
        "unexpected error: {message}"
    );
}

/// With both identity halves attached, the dependent is ordered after its
/// source even when declared first.
#[test]
fn scoped_reference_orders_dependent_after_source() {
    let scope = InstanceId::new(7);
    let source = literal(1.0);
    let dependent = var_ref(declared_reference("a", 11).with_instance_id(scope));
    let params = [
        binding(
            "b",
            ResolvedOccurrenceKey {
                instance_id: scope,
                root_def_id: DefId::new(12),
            },
            &dependent,
        ),
        binding(
            "a",
            ResolvedOccurrenceKey {
                instance_id: scope,
                root_def_id: DefId::new(11),
            },
            &source,
        ),
    ];
    let ordered = dependency_ordered_parameters(&params, &ResolvedEnumCatalog::empty())
        .expect("acyclic dependency");
    let names: Vec<&str> = ordered.iter().map(|binding| binding.name).collect();
    assert_eq!(names, vec!["a", "b"]);
}
