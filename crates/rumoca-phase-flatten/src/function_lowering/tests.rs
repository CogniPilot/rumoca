use super::*;
use rumoca_core::{ClassType, Literal, Span, VarName};

const RECORD_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7001);
const INNER_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7002);
const OUTER_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7003);
const ROTATION_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7004);
const ELEMENT_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7005);
const FIELD_A_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7010);
const FIELD_B_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7011);
const FIELD_COEFFS_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7012);
const OUTPUT_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7020);
const RECORD_PARAM_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7030);
const RECORD_VALUE_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7031);
const SOURCE_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7032);
const STATE_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7033);
const STATE_FIELD_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7034);
const REFERENCE_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7035);
const LOCAL_N_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7036);
const FUNCTION_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7037);
const ALIAS_SCOPE_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7038);

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("function_lowering_test.mo"),
        1,
        2,
    )
}

fn checked_reference(name: &str, def_id: rumoca_core::DefId) -> rumoca_core::Reference {
    let component_ref = rumoca_core::ComponentReference::construct(
        false,
        test_span(),
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: test_span(),
            subs: Vec::new(),
            def_id,
        }],
    )
    .expect("test reference is nonempty and resolved");
    rumoca_core::Reference::from_component_reference(component_ref)
}

fn var_ref(name: &str, def_id: rumoca_core::DefId) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: checked_reference(name, def_id),
        subscripts: vec![],
        span: test_span(),
    }
}

fn assignment_to(
    name: &str,
    def_id: rumoca_core::DefId,
    value: rumoca_core::Expression,
) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: rumoca_core::ComponentReference::construct(
            false,
            test_span(),
            vec![rumoca_core::ComponentRefPart {
                ident: name.to_string(),
                span: test_span(),
                subs: vec![],
                def_id,
            }],
        )
        .expect("assignment target is nonempty and resolved"),
        value,
        span: test_span(),
    }
}

fn component_ref_expr(parts: &[(&str, rumoca_core::DefId)]) -> rumoca_core::Expression {
    let display = parts
        .iter()
        .map(|(ident, _)| *ident)
        .collect::<Vec<_>>()
        .join(".");
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(
            &display,
            rumoca_core::ComponentReference::construct(
                false,
                test_span(),
                parts
                    .iter()
                    .map(|(ident, def_id)| rumoca_core::ComponentRefPart {
                        ident: (*ident).to_string(),
                        span: test_span(),
                        subs: Vec::new(),
                        def_id: *def_id,
                    })
                    .collect(),
            )
            .expect("test reference is nonempty and resolved"),
        ),
        subscripts: vec![],
        span: test_span(),
    }
}

fn record_constructor() -> rumoca_core::Function {
    let mut constructor = rumoca_core::Function::new("Pkg.Record", test_span());
    constructor.def_id = Some(RECORD_DEF_ID);
    constructor.is_constructor = true;
    constructor.add_input(
        crate::test_support::real_param("a", Vec::new(), test_span()).with_def_id(FIELD_A_DEF_ID),
    );
    constructor.add_input(
        crate::test_support::real_param("b", vec![3], test_span()).with_def_id(FIELD_B_DEF_ID),
    );
    constructor
}

fn function_with_record_input() -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new("Pkg.f", test_span());
    function.add_input(
        crate::test_support::aggregate_param("r", "Pkg.Record", Vec::new(), test_span())
            .with_def_id(RECORD_PARAM_DEF_ID)
            .with_type_class(ClassType::Record)
            .with_type_def_id(RECORD_DEF_ID),
    );
    function.add_output(
        crate::test_support::real_param("y", Vec::new(), test_span()).with_def_id(OUTPUT_DEF_ID),
    );
    function.body.push(assignment_to(
        "y",
        OUTPUT_DEF_ID,
        rumoca_core::Expression::FieldAccess {
            base: Box::new(var_ref("r", RECORD_PARAM_DEF_ID)),
            field: "a".to_string(),
            field_def_id: FIELD_A_DEF_ID,
            span: test_span(),
        },
    ));
    function
}

#[test]
fn record_param_lowering_uses_constructor_signature_metadata() {
    let mut flat = flat::Model::new();
    flat.add_function(record_constructor());
    flat.add_function(function_with_record_input());
    flat.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.f"),
            args: vec![var_ref("rec", RECORD_VALUE_DEF_ID)],
            is_constructor: false,
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
    ));

    lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

    let function = flat
        .functions
        .get(&VarName::new("Pkg.f"))
        .expect("function remains");
    let input_names = function
        .inputs
        .iter()
        .map(|input| input.name.as_str())
        .collect::<Vec<_>>();
    assert_eq!(input_names, vec!["r_a", "r_b"]);
    assert_eq!(function.inputs[0].dimensions(), Vec::<i64>::new());
    assert_eq!(function.inputs[1].dimensions(), vec![3]);
    let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
        panic!("expected assignment");
    };
    assert!(matches!(
        value,
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "r_a"
    ));
    let rumoca_core::Expression::VarRef { name, .. } = value else {
        panic!("expected rewritten record-field reference");
    };
    assert!(
        name.is_generated(),
        "decomposed record fields are compiler-generated function locals"
    );
    let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
        panic!("expected function call");
    };
    assert_eq!(args.len(), 2);
    assert!(matches!(
        &args[0],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.a"
    ));
    assert!(matches!(
        &args[1],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.b"
    ));
}

#[test]
fn record_param_lowering_follows_function_identity_before_name_canonicalization() {
    let mut flat = flat::Model::new();
    flat.add_function(record_constructor());
    let mut function = function_with_record_input();
    function.def_id = Some(FUNCTION_DEF_ID);
    flat.add_function(function);

    let alias_reference = rumoca_core::Reference::with_component_reference(
        "Alias.f",
        rumoca_core::ComponentReference::construct(
            false,
            test_span(),
            vec![
                rumoca_core::ComponentRefPart {
                    ident: "Alias".to_string(),
                    span: test_span(),
                    subs: Vec::new(),
                    def_id: ALIAS_SCOPE_DEF_ID,
                },
                rumoca_core::ComponentRefPart {
                    ident: "f".to_string(),
                    span: test_span(),
                    subs: Vec::new(),
                    def_id: FUNCTION_DEF_ID,
                },
            ],
        )
        .expect("alias call has resolved declaration identity"),
    );
    flat.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: alias_reference,
            args: vec![var_ref("rec", RECORD_VALUE_DEF_ID)],
            is_constructor: false,
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "alias probe".to_string(),
        },
    ));

    lower_record_function_params(&mut flat)
        .expect("resolved declaration identity selects the decomposed signature");

    let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
        panic!("expected function call");
    };
    assert_eq!(args.len(), 2);
    assert!(matches!(
        &args[0],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.a"
    ));
    assert!(matches!(
        &args[1],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.b"
    ));
}

#[test]
fn record_param_lowering_rewrites_compact_structured_templates() {
    let mut flat = flat::Model::new();
    flat.add_function(record_constructor());
    flat.add_function(function_with_record_input());
    flat.structured_equations
        .push(flat::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: Vec::new(),
            },
            first_equation_index: 0,
            equations_per_point: 1,
            span: test_span(),
            origin: flat::EquationOrigin::ComponentEquation {
                component: "compact probe".to_string(),
            },
            regular: None,
            template: Some(rumoca_core::ComprehensionTemplate {
                body: vec![rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::new("Pkg.f"),
                    args: vec![var_ref("rec", RECORD_VALUE_DEF_ID)],
                    is_constructor: false,
                    span: test_span(),
                }],
                scalar_view: rumoca_core::ComprehensionScalarView::BinderSubstitution,
            }),
            interiors_materialized: false,
        });

    lower_record_function_params(&mut flat)
        .expect("structured source and decomposed signature stay synchronized");

    let body = &flat.structured_equations[0]
        .template
        .as_ref()
        .expect("template remains compact")
        .body[0];
    let rumoca_core::Expression::FunctionCall { args, .. } = body else {
        panic!("expected function call");
    };
    assert_eq!(args.len(), 2);
    assert!(matches!(
        &args[0],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.a"
    ));
    assert!(matches!(
        &args[1],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.b"
    ));
}

#[test]
fn record_param_lowering_disambiguates_shared_definition_by_exposure() {
    let mut flat = flat::Model::new();
    let mut first = rumoca_core::Function::new("First.Record", test_span());
    first.def_id = Some(RECORD_DEF_ID);
    first.is_constructor = true;
    first.add_input(crate::test_support::real_param(
        "wrong",
        Vec::new(),
        test_span(),
    ));
    flat.add_function(first);
    let mut second = rumoca_core::Function::new("Second.Record", test_span());
    second.def_id = Some(RECORD_DEF_ID);
    second.is_constructor = true;
    second.add_input(crate::test_support::real_param(
        "right",
        Vec::new(),
        test_span(),
    ));
    flat.add_function(second);

    let mut function = rumoca_core::Function::new("Pkg.useSecond", test_span());
    function.add_input(
        crate::test_support::aggregate_param("r", "Second.Record", Vec::new(), test_span())
            .with_type_class(ClassType::Record)
            .with_type_def_id(RECORD_DEF_ID),
    );
    function.add_output(crate::test_support::real_param(
        "y",
        Vec::new(),
        test_span(),
    ));
    flat.add_function(function);

    lower_record_function_params(&mut flat).expect("exposure-qualified lookup should pass");

    let function = flat
        .functions
        .get(&VarName::new("Pkg.useSecond"))
        .expect("function remains");
    assert_eq!(function.inputs.len(), 1);
    assert_eq!(function.inputs[0].name, "r_right");
}

#[test]
fn record_param_lowering_preserves_named_argument_slots() {
    let mut flat = flat::Model::new();
    flat.add_function(record_constructor());
    flat.add_function(function_with_record_input());
    flat.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.f"),
            args: vec![named_function_arg_marker(
                "r".to_string(),
                var_ref("rec", RECORD_VALUE_DEF_ID),
                test_span(),
            )],
            is_constructor: false,
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
    ));

    lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

    let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
        panic!("expected function call");
    };
    let names_and_values = args
        .iter()
        .map(|arg| {
            let (name, value, _) = named_function_arg(arg).expect("named decomposed argument");
            let rumoca_core::Expression::VarRef { name: value, .. } = value else {
                panic!("expected record field reference");
            };
            (name.to_string(), value.as_str().to_string())
        })
        .collect::<Vec<_>>();
    assert_eq!(
        names_and_values,
        vec![
            ("r_a".to_string(), "rec.a".to_string()),
            ("r_b".to_string(), "rec.b".to_string()),
        ]
    );
}

#[test]
fn record_param_lowering_does_not_treat_flexible_field_as_empty() {
    let mut flat = flat::Model::new();
    let mut constructor = rumoca_core::Function::new("Pkg.FlexibleRecord", test_span());
    constructor.def_id = Some(RECORD_DEF_ID);
    constructor.is_constructor = true;
    constructor.add_input(
        crate::test_support::real_param("coeffs", vec![0], test_span())
            .with_def_id(FIELD_COEFFS_DEF_ID)
            .with_shape_expr(vec![rumoca_core::Subscript::colon(test_span())]),
    );
    flat.add_function(constructor);

    let mut function = rumoca_core::Function::new("Pkg.sumCoeffs", test_span());
    function.add_input(
        crate::test_support::aggregate_param("r", "Pkg.FlexibleRecord", Vec::new(), test_span())
            .with_def_id(RECORD_PARAM_DEF_ID)
            .with_type_class(ClassType::Record)
            .with_type_def_id(RECORD_DEF_ID),
    );
    function.add_output(
        crate::test_support::real_param("y", Vec::new(), test_span()).with_def_id(OUTPUT_DEF_ID),
    );
    function.body.push(assignment_to(
        "y",
        OUTPUT_DEF_ID,
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sum,
            args: vec![rumoca_core::Expression::FieldAccess {
                base: Box::new(var_ref("r", RECORD_PARAM_DEF_ID)),
                field: "coeffs".to_string(),
                field_def_id: FIELD_COEFFS_DEF_ID,
                span: test_span(),
            }],
            span: test_span(),
        },
    ));
    flat.add_function(function);
    flat.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.sumCoeffs"),
            args: vec![var_ref("rec", RECORD_VALUE_DEF_ID)],
            is_constructor: false,
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
    ));

    lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

    let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
        panic!("expected function call");
    };
    assert!(matches!(
        args.as_slice(),
        [rumoca_core::Expression::VarRef { name, .. }] if name.as_str() == "rec.coeffs"
    ));
}

#[test]
fn record_param_lowering_rewrites_runtime_assertion_calls() {
    let mut flat = flat::Model::new();
    flat.add_function(record_constructor());
    flat.add_function(function_with_record_input());
    flat.assert_equations.push(flat::AssertEquation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.f"),
            args: vec![var_ref("rec", RECORD_VALUE_DEF_ID)],
            is_constructor: false,
            span: test_span(),
        },
        rumoca_core::Expression::Literal {
            value: Literal::String("record assertion".to_string()),
            span: test_span(),
        },
        None,
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
    ));

    lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

    let rumoca_core::Expression::FunctionCall { args, .. } = &flat.assert_equations[0].condition
    else {
        panic!("expected assertion function call");
    };
    assert_eq!(args.len(), 2);
    assert!(matches!(
        &args[0],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.a"
    ));
    assert!(matches!(
        &args[1],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.b"
    ));
}

#[test]
fn record_array_param_lowering_rewrites_indexed_field_access() {
    let mut flat = flat::Model::new();
    flat.add_function(record_constructor());

    let mut function = rumoca_core::Function::new("Pkg.sumA", test_span());
    function.add_input(
        crate::test_support::aggregate_param("r", "Pkg.Record", vec![0], test_span())
            .with_def_id(RECORD_PARAM_DEF_ID)
            .with_type_class(ClassType::Record)
            .with_type_def_id(RECORD_DEF_ID)
            .with_shape_expr(vec![rumoca_core::Subscript::colon(test_span())]),
    );
    function.add_output(
        crate::test_support::real_param("y", Vec::new(), test_span()).with_def_id(OUTPUT_DEF_ID),
    );
    function.body.push(assignment_to(
        "y",
        OUTPUT_DEF_ID,
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sum,
            args: vec![rumoca_core::Expression::FieldAccess {
                base: Box::new(rumoca_core::Expression::Index {
                    base: Box::new(var_ref("r", RECORD_PARAM_DEF_ID)),
                    subscripts: vec![rumoca_core::Subscript::colon(test_span())],
                    span: test_span(),
                }),
                field: "a".to_string(),
                field_def_id: FIELD_A_DEF_ID,
                span: test_span(),
            }],
            span: test_span(),
        },
    ));
    flat.add_function(function);

    lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

    let function = flat
        .functions
        .get(&VarName::new("Pkg.sumA"))
        .expect("function remains");
    let input_names = function
        .inputs
        .iter()
        .map(|input| (input.name.as_str(), input.dimensions()))
        .collect::<Vec<_>>();
    assert_eq!(input_names, vec![("r_a", &[0][..]), ("r_b", &[0, 3][..])]);
    let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
        panic!("expected assignment");
    };
    let rumoca_core::Expression::BuiltinCall { args, .. } = value else {
        panic!("expected builtin call");
    };
    assert!(matches!(
        &args[0],
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "r_a" && matches!(subscripts.as_slice(), [rumoca_core::Subscript::Colon { .. }])
    ));
}

#[test]
fn record_param_lowering_preserves_index_on_array_field() {
    let mut flat = flat::Model::new();
    flat.add_function(record_constructor());

    let mut function = function_with_record_input();
    function.name = VarName::new("Pkg.firstB");
    function.body[0] = assignment_to(
        "y",
        OUTPUT_DEF_ID,
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                "r.b",
                rumoca_core::ComponentReference::construct(
                    false,
                    test_span(),
                    vec![
                        rumoca_core::ComponentRefPart {
                            ident: "r".to_string(),
                            span: test_span(),
                            subs: Vec::new(),
                            def_id: RECORD_PARAM_DEF_ID,
                        },
                        rumoca_core::ComponentRefPart {
                            ident: "b".to_string(),
                            span: test_span(),
                            subs: Vec::new(),
                            def_id: FIELD_B_DEF_ID,
                        },
                    ],
                )
                .expect("record field reference is resolved"),
            ),
            subscripts: vec![rumoca_core::Subscript::index(1, test_span())],
            span: test_span(),
        },
    );
    flat.add_function(function);

    lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

    let function = flat
        .functions
        .get(&VarName::new("Pkg.firstB"))
        .expect("function remains");
    let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
        panic!("expected assignment");
    };
    assert!(matches!(
        value,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "r_b"
                && matches!(subscripts.as_slice(), [rumoca_core::Subscript::Index { value: 1, .. }])
    ));
}

#[test]
fn record_array_param_lowering_rewrites_size_of_original_record_param() {
    let mut flat = flat::Model::new();
    flat.add_function(record_constructor());

    let mut function = rumoca_core::Function::new("Pkg.rms", test_span());
    function.add_input(
        crate::test_support::aggregate_param("r", "Pkg.Record", vec![0], test_span())
            .with_def_id(RECORD_PARAM_DEF_ID)
            .with_type_class(ClassType::Record)
            .with_type_def_id(RECORD_DEF_ID)
            .with_shape_expr(vec![rumoca_core::Subscript::colon(test_span())]),
    );
    function.add_output(
        crate::test_support::real_param("y", Vec::new(), test_span()).with_def_id(OUTPUT_DEF_ID),
    );
    function.locals.push(rumoca_core::FunctionParam {
        def_id: Some(LOCAL_N_DEF_ID),
        name: "n".to_string(),
        type_name: "Integer".to_string(),
        default: Some(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args: vec![
                var_ref("r", RECORD_PARAM_DEF_ID),
                rumoca_core::Expression::Literal {
                    value: Literal::Integer(1),
                    span: test_span(),
                },
            ],
            span: test_span(),
        }),
        ..crate::test_support::integer_param("n", Vec::new(), test_span())
    });
    flat.add_function(function);

    lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

    let function = flat
        .functions
        .get(&VarName::new("Pkg.rms"))
        .expect("function remains");
    let Some(default) = function.locals[0].default.as_ref() else {
        panic!("expected local default");
    };
    let rumoca_core::Expression::BuiltinCall { args, .. } = default else {
        panic!("expected size builtin");
    };
    assert!(matches!(
        &args[0],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "r_a"
    ));
}

#[test]
fn record_param_lowering_rejects_unknown_record_metadata() {
    let mut flat = flat::Model::new();
    flat.add_function(function_with_record_input());
    flat.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.f"),
            args: vec![rumoca_core::Expression::Literal {
                value: Literal::Real(1.0),
                span: test_span(),
            }],
            is_constructor: false,
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
    ));

    let err = lower_record_function_params(&mut flat)
        .expect_err("missing constructor metadata must be rejected");
    assert!(matches!(
        err,
        FlattenError::MissingResolvedClassMetadata { .. }
    ));
}

#[test]
fn record_param_lowering_reconstructs_whole_record_uses() {
    let mut flat = flat::Model::new();
    flat.add_function(record_constructor());

    let mut function = rumoca_core::Function::new("Pkg.copyRecord", test_span());
    function.add_input(
        crate::test_support::aggregate_param("source", "Pkg.Record", Vec::new(), test_span())
            .with_def_id(SOURCE_DEF_ID)
            .with_type_class(ClassType::Record)
            .with_type_def_id(RECORD_DEF_ID),
    );
    function.add_output(
        crate::test_support::aggregate_param("result", "Pkg.Record", Vec::new(), test_span())
            .with_def_id(OUTPUT_DEF_ID)
            .with_type_class(ClassType::Record)
            .with_type_def_id(RECORD_DEF_ID),
    );
    function.body.push(assignment_to(
        "result",
        OUTPUT_DEF_ID,
        var_ref("source", SOURCE_DEF_ID),
    ));
    flat.add_function(function);

    lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

    let function = flat
        .functions
        .get(&VarName::new("Pkg.copyRecord"))
        .expect("function remains");
    assert_eq!(
        function
            .inputs
            .iter()
            .map(|input| input.name.as_str())
            .collect::<Vec<_>>(),
        vec!["source_a", "source_b"]
    );
    let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
        panic!("expected assignment");
    };
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor,
        ..
    } = value
    else {
        panic!("expected reconstructed record constructor, got {value:?}");
    };
    assert!(*is_constructor);
    assert_eq!(name.as_str(), "Pkg.Record");
    assert_eq!(name.target_def_id(), Some(RECORD_DEF_ID));
    assert!(matches!(
        args.as_slice(),
        [
            rumoca_core::Expression::VarRef { name: first, .. },
            rumoca_core::Expression::VarRef { name: second, .. }
        ] if first.as_str() == "source_a" && second.as_str() == "source_b"
    ));
}

#[test]
fn record_field_normalization_uses_structured_component_ref_parts() {
    let mut function = rumoca_core::Function::new("Pkg.f", test_span());
    function.add_input(
        crate::test_support::aggregate_param("state", "Pkg.State", Vec::new(), test_span())
            .with_def_id(STATE_DEF_ID)
            .with_type_class(ClassType::Record),
    );
    function.add_output(
        crate::test_support::real_param("y", Vec::new(), test_span()).with_def_id(OUTPUT_DEF_ID),
    );
    function.body.push(assignment_to(
        "y",
        OUTPUT_DEF_ID,
        component_ref_expr(&[("state", STATE_DEF_ID), ("x", STATE_FIELD_DEF_ID)]),
    ));

    rewrite_record_field_access_in_body(&mut function);

    let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
        panic!("expected assignment");
    };
    let rumoca_core::Expression::FieldAccess { base, field, .. } = value else {
        panic!("expected normalized field access, got {value:?}");
    };
    assert_eq!(field, "x");
    assert!(matches!(
        base.as_ref(),
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "state"
    ));
}

#[test]
fn nested_record_param_call_uses_decomposed_caller_locals() {
    let mut flat = flat::Model::new();
    flat.add_function(record_constructor());

    let mut callee = rumoca_core::Function::new("Pkg.g", test_span());
    callee.add_input(
        crate::test_support::aggregate_param("r", "Pkg.Record", Vec::new(), test_span())
            .with_def_id(RECORD_PARAM_DEF_ID)
            .with_type_class(ClassType::Record)
            .with_type_def_id(RECORD_DEF_ID),
    );
    callee.add_output(
        crate::test_support::real_param("y", Vec::new(), test_span()).with_def_id(OUTPUT_DEF_ID),
    );
    callee.body.push(assignment_to(
        "y",
        OUTPUT_DEF_ID,
        rumoca_core::Expression::FieldAccess {
            base: Box::new(var_ref("r", RECORD_PARAM_DEF_ID)),
            field: "a".to_string(),
            field_def_id: FIELD_A_DEF_ID,
            span: test_span(),
        },
    ));
    flat.add_function(callee);

    let mut caller = rumoca_core::Function::new("Pkg.f", test_span());
    caller.add_input(
        crate::test_support::aggregate_param("state", "Pkg.Record", Vec::new(), test_span())
            .with_def_id(STATE_DEF_ID)
            .with_type_class(ClassType::Record)
            .with_type_def_id(RECORD_DEF_ID),
    );
    caller.add_output(
        crate::test_support::real_param("y", Vec::new(), test_span()).with_def_id(OUTPUT_DEF_ID),
    );
    caller.body.push(assignment_to(
        "y",
        OUTPUT_DEF_ID,
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.g"),
            args: vec![var_ref("state", STATE_DEF_ID)],
            is_constructor: false,
            span: test_span(),
        },
    ));
    flat.add_function(caller);

    lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

    let function = flat
        .functions
        .get(&VarName::new("Pkg.f"))
        .expect("caller remains");
    let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
        panic!("expected assignment");
    };
    let rumoca_core::Expression::FunctionCall { args, .. } = value else {
        panic!("expected function call");
    };
    assert_eq!(args.len(), 2);
    assert!(matches!(
        &args[0],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "state_a"
    ));
    assert!(matches!(
        &args[1],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "state_b"
    ));
}

#[test]
fn record_local_call_argument_remains_structural_field_access() {
    let mut flat = flat::Model::new();
    flat.add_function(record_constructor());
    flat.add_function(function_with_record_input());

    let mut caller = rumoca_core::Function::new("Pkg.caller", test_span());
    caller.add_local(
        crate::test_support::aggregate_param("localRecord", "Pkg.Record", Vec::new(), test_span())
            .with_def_id(RECORD_VALUE_DEF_ID)
            .with_type_class(ClassType::Record)
            .with_type_def_id(RECORD_DEF_ID),
    );
    caller.add_output(
        crate::test_support::real_param("y", Vec::new(), test_span()).with_def_id(OUTPUT_DEF_ID),
    );
    caller.body.push(assignment_to(
        "y",
        OUTPUT_DEF_ID,
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.f"),
            args: vec![var_ref("localRecord", RECORD_VALUE_DEF_ID)],
            is_constructor: false,
            span: test_span(),
        },
    ));
    flat.add_function(caller);

    lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

    let caller = &flat.functions[&VarName::new("Pkg.caller")];
    let rumoca_core::Statement::Assignment { value, .. } = &caller.body[0] else {
        panic!("expected assignment");
    };
    let rumoca_core::Expression::FunctionCall { args, .. } = value else {
        panic!("expected function call");
    };
    assert_eq!(args.len(), 2);
    for (argument, expected_field) in args.iter().zip(["a", "b"]) {
        assert!(matches!(
            argument,
            rumoca_core::Expression::FieldAccess { base, field, .. }
                if field == expected_field
                    && matches!(base.as_ref(), rumoca_core::Expression::VarRef { name, .. }
                        if name.as_str() == "localRecord")
        ));
    }
}

#[test]
fn nested_record_param_with_qualified_type_is_decomposed_to_fixpoint() {
    let mut flat = flat::Model::new();

    let mut inner_constructor = rumoca_core::Function::new("Pkg.Inner", test_span());
    inner_constructor.def_id = Some(INNER_DEF_ID);
    inner_constructor.is_constructor = true;
    inner_constructor.add_input(crate::test_support::real_param(
        "value",
        Vec::new(),
        test_span(),
    ));
    flat.add_function(inner_constructor);

    let mut outer_constructor = rumoca_core::Function::new("Pkg.Outer", test_span());
    outer_constructor.def_id = Some(OUTER_DEF_ID);
    outer_constructor.is_constructor = true;
    outer_constructor.add_input(
        crate::test_support::aggregate_param("inner", "Pkg.Inner", Vec::new(), test_span())
            .with_type_class(ClassType::Record)
            .with_type_def_id(INNER_DEF_ID),
    );
    flat.add_function(outer_constructor);

    let mut function = rumoca_core::Function::new("Pkg.f", test_span());
    function.add_input(
        crate::test_support::aggregate_param("outer", "Pkg.Outer", Vec::new(), test_span())
            .with_type_class(ClassType::Record)
            .with_type_def_id(OUTER_DEF_ID),
    );
    flat.add_function(function);

    lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

    let function = flat
        .functions
        .get(&VarName::new("Pkg.f"))
        .expect("function remains");
    assert_eq!(
        function
            .inputs
            .iter()
            .map(|input| input.name.as_str())
            .collect::<Vec<_>>(),
        vec!["outer_inner_value"]
    );
    assert!(
        function
            .inputs
            .iter()
            .all(|input| input.type_class.is_none())
    );
    let outer_constructor = &flat.functions[&VarName::new("Pkg.Outer")];
    assert_eq!(outer_constructor.inputs.len(), 1);
    assert_eq!(outer_constructor.inputs[0].name, "inner");
    assert_eq!(
        outer_constructor.inputs[0].type_class,
        Some(ClassType::Record),
        "constructors retain the compact nested aggregate layout"
    );
}

#[test]
fn nested_record_call_arg_projections_follow_decomposed_caller_inputs() {
    let mut flat = flat::Model::new();

    let mut rotation_constructor = rumoca_core::Function::new("Pkg.Rotation", test_span());
    rotation_constructor.def_id = Some(ROTATION_DEF_ID);
    rotation_constructor.is_constructor = true;
    rotation_constructor.add_input(
        crate::test_support::real_param("interfaceMarker", vec![0], test_span())
            .with_shape_expr(vec![rumoca_core::Subscript::index(0, test_span())]),
    );
    rotation_constructor.add_input(crate::test_support::real_param("q", vec![4], test_span()));
    flat.add_function(rotation_constructor);

    let mut element_constructor = rumoca_core::Function::new("Pkg.Element", test_span());
    element_constructor.def_id = Some(ELEMENT_DEF_ID);
    element_constructor.is_constructor = true;
    element_constructor.add_input(crate::test_support::real_param(
        "position",
        vec![3],
        test_span(),
    ));
    element_constructor.add_input(
        crate::test_support::aggregate_param("rotation", "Pkg.Rotation", Vec::new(), test_span())
            .with_type_class(ClassType::Record)
            .with_type_def_id(ROTATION_DEF_ID),
    );
    flat.add_function(element_constructor);

    let mut inverse = rumoca_core::Function::new("Pkg.inverse", test_span());
    inverse.add_input(
        crate::test_support::aggregate_param("element", "Pkg.Element", Vec::new(), test_span())
            .with_type_class(ClassType::Record)
            .with_type_def_id(ELEMENT_DEF_ID),
    );
    inverse.add_output(crate::test_support::real_param(
        "y",
        Vec::new(),
        test_span(),
    ));
    flat.add_function(inverse);

    let mut caller = rumoca_core::Function::new("Pkg.caller", test_span());
    caller.add_input(
        crate::test_support::aggregate_param("reference", "Pkg.Element", Vec::new(), test_span())
            .with_def_id(REFERENCE_DEF_ID)
            .with_type_class(ClassType::Record)
            .with_type_def_id(ELEMENT_DEF_ID),
    );
    caller.add_output(
        crate::test_support::real_param("y", Vec::new(), test_span()).with_def_id(OUTPUT_DEF_ID),
    );
    caller.body.push(assignment_to(
        "y",
        OUTPUT_DEF_ID,
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.inverse"),
            args: vec![var_ref("reference", REFERENCE_DEF_ID)],
            is_constructor: false,
            span: test_span(),
        },
    ));
    flat.add_function(caller);

    lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

    let caller = flat
        .functions
        .get(&VarName::new("Pkg.caller"))
        .expect("caller remains");
    let rumoca_core::Statement::Assignment { value, .. } = &caller.body[0] else {
        panic!("expected assignment");
    };
    let rumoca_core::Expression::FunctionCall { args, .. } = value else {
        panic!("expected function call");
    };
    assert_eq!(args.len(), 3);
    assert!(matches!(
        &args[0],
        rumoca_core::Expression::VarRef { name, .. }
            if name.as_str() == "reference_position"
    ));
    assert!(matches!(
        &args[1],
        rumoca_core::Expression::Array { elements, .. } if elements.is_empty()
    ));
    assert!(matches!(
        &args[2],
        rumoca_core::Expression::VarRef { name, .. }
            if name.as_str() == "reference_rotation_q"
    ));
}
