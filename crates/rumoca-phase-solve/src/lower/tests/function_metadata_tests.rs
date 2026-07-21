use super::*;

#[test]
fn lower_function_call_does_not_fold_self_referential_start_metadata() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .metadata
        .variable_starts
        .insert("x".to_string(), var("x"));

    let mut identity = test_function("My.identity", lower_test_span());
    identity.inputs.push(function_param("u"));
    identity.outputs.push(function_param("y"));
    identity.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value: var("u"),
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(identity.name.clone(), identity);

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_expression_rows_from_expressions_with_runtime_metadata(
        &[rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.identity",
            )),
            args: vec![var("x")],
            is_constructor: false,
            span: lower_test_span(),
        }],
        &layout,
        &dae_model.symbols.functions,
        &dae_model.clocks.intervals,
        &dae_model.clocks.timings,
        &dae_model.metadata.variable_starts,
    )
    .expect("self-referential start metadata should not recurse during constant folding");

    assert_eq!(rows.len(), 1);
    assert!(
        rows[0]
            .iter()
            .any(|op| matches!(op, LinearOp::LoadY { .. }))
    );
}
