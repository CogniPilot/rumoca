use super::*;

#[test]
fn constructor_partial_application_survives_canonicalization_and_materialization() {
    let constructor_def_id = rumoca_core::DefId::new(95_001);
    let mut model = flat::Model::new();
    let mut constructor =
        rumoca_core::Function::new("Pkg.R", rumoca_core::DefId::new(61_001), test_span());
    constructor.def_id = Some(constructor_def_id);
    constructor.is_constructor = true;
    constructor.add_input(crate::test_support::real_param(
        "captured",
        Vec::new(),
        test_span(),
    ));
    constructor.add_input(crate::test_support::real_param(
        "unbound",
        Vec::new(),
        test_span(),
    ));
    model.add_function(constructor);
    let instance_id = model.functions[&rumoca_core::VarName::new("Pkg.R")]
        .instance_id
        .expect("Flat assigns the constructor instance");
    let reference = rumoca_core::Reference::new("Pkg.R").with_resolved_function(
        rumoca_core::ResolvedFunctionReference {
            instance_id,
            base_part_count: 0,
            transitively_non_replaceable: true,
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: reference,
            args: vec![rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(7),
                span: test_span(),
            }],
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::PartialApplication,
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "function_value".to_string(),
        },
    ));

    canonicalize_collected_function_calls_without_scopes(&mut model)
        .expect("canonicalize exact constructor identity");
    materialize_flat_function_call_args(&mut model)
        .expect("a partial application must not require its unbound slot");

    let rumoca_core::Expression::FunctionCall {
        args,
        is_constructor,
        call_kind,
        ..
    } = &model.equations[0].residual
    else {
        panic!("expected the function value");
    };
    assert!(
        *is_constructor,
        "canonical constructor identity is retained"
    );
    assert_eq!(
        *call_kind,
        rumoca_core::FunctionCallKind::PartialApplication,
        "argument materialization must not launder a function value into an invocation"
    );
    assert_eq!(args.len(), 1, "the unbound input remains unmaterialized");
}
