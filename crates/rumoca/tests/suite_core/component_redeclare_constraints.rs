//! MLS §7.3.2 checks alternatives against the declared constraining interface.

use rumoca::Compiler;
use rumoca_ir_ast as ast;
use rumoca_phase_instantiate::InstantiateError;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const RECORD_SOURCE: &str = r#"record Interface
  parameter Real gain=1;
end Interface;
record DefaultData
  extends Interface;
  parameter Real extra=5;
end DefaultData;
record AlternativeData
  extends Interface(gain=2);
end AlternativeData;
record InvalidData
  parameter Real other=2;
end InvalidData;
model Host
  replaceable parameter DefaultData data constrainedby Interface;
end Host;
model Root
  Host h(redeclare AlternativeData data);
  Real y=h.data.gain*time;
end Root;
"#;

const MODEL_SOURCE: &str = r#"partial model RateInterface
  parameter Real rate=1;
  Real x(start=0, fixed=true);
end RateInterface;
model DefaultRate
  extends RateInterface;
  parameter Real extra=3;
equation
  der(x)=rate;
end DefaultRate;
model AlternativeRate
  extends RateInterface(rate=2);
equation
  der(x)=rate;
end AlternativeRate;
model Host
  replaceable DefaultRate item constrainedby RateInterface;
end Host;
model Root
  Host h(redeclare AlternativeRate item);
  Real y=h.item.x;
end Root;
"#;

fn resolved(source: &str) -> ast::ClassTree {
    let file = "component_redeclare_constraints.mo";
    let parsed = rumoca_phase_parse::parse_to_ast(source, file).unwrap();
    let mut tree = ast::ClassTree::from_parsed(parsed);
    tree.source_map.add(file, source);
    rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .unwrap()
        .into_inner()
}

fn check_valid_replacement(source: &str, component: &str, constraint: &str) {
    let tree = resolved(source);
    let host = tree.get_class_by_qualified_name("Host").unwrap();
    assert_eq!(
        host.components[component]
            .constrainedby
            .as_ref()
            .unwrap()
            .def_id,
        tree.get_class_by_qualified_name(constraint).unwrap().def_id,
        "Resolve must preserve the source's explicit constraint identity"
    );
    rumoca_phase_instantiate::instantiate_model(&tree, "Root").expect(
        "replacement satisfies the explicit interface, despite lacking default-only fields",
    );
    let compiled = Compiler::new()
        .model("Root")
        .compile_str(source, "component_redeclare_constraints.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.2,
                dt: Some(0.05),
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap();
        let y = result.names.iter().position(|name| name == "y").unwrap();
        assert!(!result.times.is_empty());
        for (&time, &actual) in result.times.iter().zip(&result.data[y]) {
            assert!(
                (actual - 2.0 * time).abs() < 1e-7,
                "{solver_mode:?}: y({time})={actual}"
            );
        }
    }
}

#[test]
fn record_replacement_uses_explicit_constraint_not_default_implementation() {
    check_valid_replacement(RECORD_SOURCE, "data", "Interface");
}

#[test]
fn model_replacement_uses_explicit_constraint_not_default_implementation() {
    check_valid_replacement(MODEL_SOURCE, "item", "RateInterface");
}

fn check_constraint_rejection(source: &str) {
    let tree = resolved(source);
    let error = rumoca_phase_instantiate::instantiate_model(&tree, "Root").unwrap_err();
    assert!(
        matches!(
            *error,
            InstantiateError::RedeclareConstraintViolation { .. }
        ),
        "{error}"
    );
}

#[test]
fn replacement_must_still_provide_the_constraining_interface() {
    check_constraint_rejection(
        &RECORD_SOURCE
            .replace(
                "redeclare AlternativeData data",
                "redeclare InvalidData data",
            )
            .replace("Real y=h.data.gain*time;", "Real y=time;"),
    );
}

#[test]
fn omitted_constraint_requires_the_default_implementation_interface() {
    check_constraint_rejection(&RECORD_SOURCE.replace(" constrainedby Interface", ""));
}
