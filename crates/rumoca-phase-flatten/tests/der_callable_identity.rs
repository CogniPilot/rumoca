//! End-to-end identity witness for the UnitDerivative golden cone.

use rumoca_core::ExpressionVisitor;
use rumoca_ir_ast as ast;

const SOURCE_NAME: &str = "UnitDerivative.mo";
const SOURCE: &str = include_str!("../../rumoca/tests/fixtures/golden/UnitDerivative.mo");

fn resolved_and_instantiated(
    source: &str,
    source_name: &str,
    model_name: &str,
) -> (
    rumoca_phase_resolve::ResolvedTree,
    rumoca_ir_ast::InstanceOverlay,
) {
    let stored = rumoca_phase_parse::parse_to_ast(source, source_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(source_name, source);
    let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .expect("source resolves before the phase-boundary check");
    let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(
        resolved.inner(),
        model_name,
    ) {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        rumoca_phase_instantiate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
            panic!("fixture instantiation failed: {error}")
        }
    };
    (resolved, overlay)
}

#[test]
fn unit_derivative_links_typed_parse_identity_to_flat_and_dae_state_identity() {
    let stored =
        rumoca_phase_parse::parse_to_ast(SOURCE, SOURCE_NAME).expect("golden source parses");
    let model = stored
        .classes
        .get("UnitDerivative")
        .expect("golden model exists");
    let [ast::Equation::Simple { lhs, .. }] = model.equations.as_slice() else {
        panic!("UnitDerivative has one simple equation");
    };
    assert!(matches!(
        lhs,
        ast::Expression::DerivativeCall { args, .. }
            if matches!(args.as_slice(), [ast::Expression::ComponentReference(reference)] if reference.to_string() == "x")
    ));

    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, SOURCE);
    let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .expect("golden source resolves without reminting derivative identity");
    let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(
        resolved.inner(),
        "UnitDerivative",
    ) {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        rumoca_phase_instantiate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("golden fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
            panic!("golden fixture instantiation failed: {error}")
        }
    };
    let typed =
        rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, "UnitDerivative")
            .expect("golden fixture typechecks through typed derivative identity");
    let source_map = resolved.inner().source_map.clone();
    let flat =
        rumoca_phase_flatten::flatten_typed(typed, rumoca_phase_flatten::FlattenOptions::default())
            .expect("golden fixture lowers typed derivative identity");

    struct DerivativeOperands(Vec<rumoca_core::Reference>);
    impl ExpressionVisitor for DerivativeOperands {
        fn visit_builtin_call(
            &mut self,
            function: &rumoca_core::BuiltinFunction,
            args: &[rumoca_core::Expression],
        ) {
            if *function == rumoca_core::BuiltinFunction::Der
                && let [rumoca_core::Expression::VarRef { name, .. }] = args
            {
                self.0.push(name.clone());
            }
            self.walk_builtin_call(function, args);
        }
    }

    let mut operands = DerivativeOperands(Vec::new());
    for equation in &flat.equations {
        operands.visit_expression(&equation.residual);
    }
    let [operand] = operands.0.as_slice() else {
        panic!("UnitDerivative Flat model must carry one typed derivative operand");
    };
    let variable = flat
        .variables
        .get(&rumoca_core::VarName::new("x"))
        .expect("Flat model owns x");
    assert_eq!(operand.instance_id(), Some(variable.instance_id));
    assert_eq!(operand.component_ref(), variable.component_ref.as_ref());

    let product = rumoca_phase_dae::construct(&flat, source_map)
        .expect("typed Flat derivative links to a DAE state occurrence");
    let state_occurrences = product.dae().inspect(|view| {
        view.variables()
            .filter(|(_, variable)| {
                matches!(
                    variable.identity(),
                    rumoca_ir_dae::VariableIdentity::State(_)
                )
            })
            .map(|(_, variable)| variable.source_occurrence())
            .collect::<Vec<_>>()
    });
    assert_eq!(state_occurrences.len(), 1);
    assert_eq!(state_occurrences[0].index(), variable.instance_id.index());
}

#[test]
fn named_derivative_argument_is_refused_before_flatten_can_be_attempted() {
    let source = "model Named Real x; equation der(x = x) = 1.0; end Named;";
    let (resolved, overlay) = resolved_and_instantiated(source, "Named.mo", "Named");
    let checked = rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, "Named");

    let mut flatten_attempted = false;
    let flat_product = match checked {
        Err(diagnostics) => {
            assert!(diagnostics.iter().any(|diagnostic| {
                diagnostic.code.as_deref() == Some("ET009")
                    && diagnostic.message.contains("positional value expression")
                    && diagnostic.message.contains("MLS §3.7.4")
            }));
            None
        }
        Ok(typed) => {
            flatten_attempted = true;
            Some(rumoca_phase_flatten::flatten_typed(
                typed,
                rumoca_phase_flatten::FlattenOptions::default(),
            ))
        }
    };
    assert!(
        !flatten_attempted,
        "Typecheck must withhold the Flat mint input"
    );
    assert!(
        flat_product.is_none(),
        "no Flat attempt or product may exist"
    );
}

#[test]
fn real_array_derivative_and_post_call_index_remain_valid() {
    let source = r#"
model LegalDerivativeForms
  Real x[2];
  Real y;
equation
  der(x) = {1.0, 2.0};
  y = der(x)[1];
end LegalDerivativeForms;
"#;
    let (resolved, overlay) =
        resolved_and_instantiated(source, "LegalDerivativeForms.mo", "LegalDerivativeForms");
    let typed = rumoca_phase_typecheck::typecheck_instanced_tree(
        &resolved,
        overlay,
        "LegalDerivativeForms",
    )
    .expect("Real array derivatives and post-call indexing are valid");
    let flat =
        rumoca_phase_flatten::flatten_typed(typed, rumoca_phase_flatten::FlattenOptions::default())
            .expect("valid derivative forms lower to Flat");
    assert!(flat.equations.iter().any(|equation| {
        equation.residual.contains_subexpression(|expression| {
            matches!(
                expression,
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Der,
                    ..
                }
            )
        })
    }));
}
