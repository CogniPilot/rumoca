use std::collections::{HashMap, HashSet};

use indexmap::IndexSet;
use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

use super::{
    EqFilterContext, classify_equations, collect_discrete_valued_lhs_target_counts,
    collect_explicit_discrete_assignments, expand_record_field_equation,
    explicit_lhs_reference_from_target, output_alias_skip_reason, output_has_component_equation,
};
use crate::ToDaeError;

fn fixture_span() -> Span {
    Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 0, 1)
}

fn var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![],
        span: fixture_span(),
    }
}

fn residual(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: fixture_span(),
    }
}

fn call(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args: Vec::new(),
        is_constructor: false,
        span: fixture_span(),
    }
}

fn component_ref_with_def_id(
    parts: Vec<(&str, Vec<i64>)>,
    def_id: Option<rumoca_core::DefId>,
) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: fixture_span(),
        parts: parts
            .into_iter()
            .map(|(ident, subs)| rumoca_core::ComponentRefPart {
                ident: ident.to_string(),
                span: fixture_span(),
                subs: subs
                    .into_iter()
                    .map(|value| rumoca_core::Subscript::generated_index(value, fixture_span()))
                    .collect(),
            })
            .collect(),
        def_id,
    }
}

fn component_ref(parts: Vec<(&str, Vec<i64>)>) -> rumoca_core::ComponentReference {
    component_ref_with_def_id(parts, None)
}

fn reference_with_parts(name: &str, parts: Vec<(&str, Vec<i64>)>) -> rumoca_core::Reference {
    rumoca_core::Reference::with_component_reference(name, component_ref(parts))
}

fn var_ref_with_parts(name: &str, parts: Vec<(&str, Vec<i64>)>) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: reference_with_parts(name, parts),
        subscripts: vec![],
        span: fixture_span(),
    }
}

fn primitive_variable_with_dims_and_parts(
    name: &str,
    dims: Vec<i64>,
    parts: Vec<(&str, Vec<i64>)>,
    def_id: rumoca_core::DefId,
) -> flat::Variable {
    let var_name = rumoca_core::VarName::new(name);
    flat::Variable {
        name: var_name,
        dims,
        component_ref: Some(component_ref_with_def_id(parts, Some(def_id))),
        is_primitive: true,
        ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    }
}

fn primitive_variable_with_parts(
    name: &str,
    parts: Vec<(&str, Vec<i64>)>,
    def_id: rumoca_core::DefId,
) -> flat::Variable {
    primitive_variable_with_dims_and_parts(name, Vec::new(), parts, def_id)
}

fn add_pair_constructor(flat_model: &mut flat::Model) {
    let mut constructor = rumoca_core::Function::new("PairRecord", fixture_span());
    constructor.is_constructor = true;
    constructor.add_input(
        rumoca_core::FunctionParam::new("alpha", "Real", fixture_span())
            .with_def_id(rumoca_core::DefId::new(101)),
    );
    constructor.add_input(
        rumoca_core::FunctionParam::new("beta", "Real", fixture_span())
            .with_def_id(rumoca_core::DefId::new(102)),
    );
    flat_model
        .functions
        .insert(constructor.name.clone(), constructor);
}

fn add_pair_record_function(flat_model: &mut flat::Model, name: &str) {
    let mut function = rumoca_core::Function::new(name, fixture_span());
    let mut output = rumoca_core::FunctionParam::new("y", "PairRecord", fixture_span());
    output.type_class = Some(rumoca_core::ClassType::Record);
    function.add_output(output);
    flat_model.functions.insert(function.name.clone(), function);
}

#[test]
fn test_record_function_equation_expands_to_declared_fields() {
    let mut flat_model = flat::Model::new();
    for (name, dims, parts, def_id) in [
        (
            "R.T",
            vec![3, 3],
            vec![("R", vec![]), ("T", vec![])],
            rumoca_core::DefId::new(11),
        ),
        (
            "R.w",
            vec![3],
            vec![("R", vec![]), ("w", vec![])],
            rumoca_core::DefId::new(12),
        ),
    ] {
        let var = primitive_variable_with_dims_and_parts(name, dims, parts, def_id);
        flat_model.variables.insert(var.name.clone(), var);
    }

    let mut constructor = rumoca_core::Function::new("Frames.Orientation", fixture_span());
    constructor.is_constructor = true;
    constructor.add_input(
        rumoca_core::FunctionParam::new("T", "Real", fixture_span())
            .with_dims(vec![3, 3])
            .with_def_id(rumoca_core::DefId::new(11)),
    );
    constructor.add_input(
        rumoca_core::FunctionParam::new("w", "Real", fixture_span())
            .with_dims(vec![3])
            .with_def_id(rumoca_core::DefId::new(12)),
    );
    flat_model
        .functions
        .insert(constructor.name.clone(), constructor);

    let mut null_rotation = rumoca_core::Function::new("Frames.nullRotation", fixture_span());
    let mut output = rumoca_core::FunctionParam::new("R", "Orientation", fixture_span());
    output.type_class = Some(rumoca_core::ClassType::Record);
    null_rotation.add_output(output);
    flat_model
        .functions
        .insert(null_rotation.name.clone(), null_rotation);

    let equation = flat::Equation::new(
        residual(
            var_ref_with_parts("R", vec![("R", vec![])]),
            call("Frames.nullRotation"),
        ),
        fixture_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "body".to_string(),
        },
    );

    let expanded = expand_record_field_equation(&equation, &flat_model)
        .unwrap()
        .expect("record-valued equation should expand");
    assert_eq!(expanded.len(), 2);
    assert_eq!(expanded[0].scalar_count, 9);
    assert_eq!(expanded[1].scalar_count, 3);
    assert!(format!("{:?}", expanded[0].residual).contains("R.T"));
    assert!(format!("{:?}", expanded[0].residual).contains("FieldAccess"));
    assert!(format!("{:?}", expanded[1].residual).contains("R.w"));
}

#[test]
fn test_record_function_equation_expands_array_fields_by_component_ref() {
    let mut flat_model = flat::Model::new();
    for (name, parts, def_id) in [
        (
            "controller.y[1].alpha",
            vec![("controller", vec![]), ("y", vec![1]), ("alpha", vec![])],
            rumoca_core::DefId::new(101),
        ),
        (
            "controller.y[2].alpha",
            vec![("controller", vec![]), ("y", vec![2]), ("alpha", vec![])],
            rumoca_core::DefId::new(101),
        ),
        (
            "controller.y[1].beta",
            vec![("controller", vec![]), ("y", vec![1]), ("beta", vec![])],
            rumoca_core::DefId::new(102),
        ),
        (
            "controller.y[2].beta",
            vec![("controller", vec![]), ("y", vec![2]), ("beta", vec![])],
            rumoca_core::DefId::new(102),
        ),
    ] {
        let var = primitive_variable_with_parts(name, parts, def_id);
        flat_model.variables.insert(var.name.clone(), var);
    }
    add_pair_constructor(&mut flat_model);
    add_pair_record_function(&mut flat_model, "Records.makePair");

    let equation = flat::Equation::new_array(
        residual(
            var_ref_with_parts("controller.y", vec![("controller", vec![]), ("y", vec![])]),
            call("Records.makePair"),
        ),
        fixture_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "controller".to_string(),
        },
        2,
    );

    let expanded = expand_record_field_equation(&equation, &flat_model)
        .unwrap()
        .expect("record array equation should expand");
    assert_eq!(expanded.len(), 2);
    assert_eq!(expanded[0].scalar_count, 2);
    assert_eq!(expanded[1].scalar_count, 2);

    let first = format!("{:?}", expanded[0].residual);
    assert!(first.contains("controller.y[1].alpha"));
    assert!(first.contains("controller.y[2].alpha"));
    assert!(first.contains("FieldAccess"));
    let second = format!("{:?}", expanded[1].residual);
    assert!(second.contains("controller.y[1].beta"));
    assert!(second.contains("controller.y[2].beta"));
    assert!(second.contains("FieldAccess"));
}

#[test]
fn test_record_function_equation_matches_subscripts_semantically() {
    let mut flat_model = flat::Model::new();
    let generated_elsewhere = rumoca_core::Span {
        source: rumoca_core::SourceId::from_source_name(file!()),
        start: rumoca_core::BytePos(11),
        end: rumoca_core::BytePos(12),
    };
    for (name, field, def_id) in [
        (
            "controller.y[1].alpha",
            "alpha",
            rumoca_core::DefId::new(101),
        ),
        ("controller.y[1].beta", "beta", rumoca_core::DefId::new(102)),
    ] {
        let mut var = primitive_variable_with_parts(
            name,
            vec![("controller", vec![]), ("y", vec![1]), (field, vec![])],
            def_id,
        );
        var.component_ref.as_mut().expect("component ref").parts[1].subs =
            vec![rumoca_core::Subscript::generated_index(
                1,
                generated_elsewhere,
            )];
        flat_model.variables.insert(var.name.clone(), var);
    }
    add_pair_constructor(&mut flat_model);
    add_pair_record_function(&mut flat_model, "Records.makePair");

    let equation = flat::Equation::new_array(
        residual(
            var_ref_with_parts(
                "controller.y[1]",
                vec![("controller", vec![]), ("y", vec![1])],
            ),
            call("Records.makePair"),
        ),
        fixture_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "controller".to_string(),
        },
        1,
    );

    let expanded = expand_record_field_equation(&equation, &flat_model)
        .unwrap()
        .expect("same subscript value from a different span should match");
    assert_eq!(expanded.len(), 2);
    assert!(format!("{:?}", expanded[0].residual).contains("controller.y[1].alpha"));
}

#[test]
fn test_record_function_equation_matches_record_array_index_on_field_leaf() {
    let mut flat_model = flat::Model::new();
    for (name, parts, def_id) in [
        (
            "controller.y.alpha[1]",
            vec![("controller", vec![]), ("y", vec![]), ("alpha", vec![1])],
            rumoca_core::DefId::new(101),
        ),
        (
            "controller.y.alpha[2]",
            vec![("controller", vec![]), ("y", vec![]), ("alpha", vec![2])],
            rumoca_core::DefId::new(101),
        ),
        (
            "controller.y.beta[1]",
            vec![("controller", vec![]), ("y", vec![]), ("beta", vec![1])],
            rumoca_core::DefId::new(102),
        ),
        (
            "controller.y.beta[2]",
            vec![("controller", vec![]), ("y", vec![]), ("beta", vec![2])],
            rumoca_core::DefId::new(102),
        ),
    ] {
        let var = primitive_variable_with_parts(name, parts, def_id);
        flat_model.variables.insert(var.name.clone(), var);
    }
    add_pair_constructor(&mut flat_model);
    add_pair_record_function(&mut flat_model, "Records.makePair");

    let equation = flat::Equation::new_array(
        residual(
            var_ref_with_parts(
                "controller.y[1]",
                vec![("controller", vec![]), ("y", vec![1])],
            ),
            call("Records.makePair"),
        ),
        fixture_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "controller".to_string(),
        },
        1,
    );

    let expanded = expand_record_field_equation(&equation, &flat_model)
        .unwrap()
        .expect("record array element equation should match field-leaf indices");
    assert_eq!(expanded.len(), 2);
    assert_eq!(expanded[0].scalar_count, 1);
    assert_eq!(expanded[1].scalar_count, 1);

    let first = format!("{:?}", expanded[0].residual);
    assert!(first.contains("controller.y.alpha[1]"));
    assert!(!first.contains("controller.y.alpha[2]"));
    let second = format!("{:?}", expanded[1].residual);
    assert!(second.contains("controller.y.beta[1]"));
    assert!(!second.contains("controller.y.beta[2]"));
}

#[test]
fn test_record_function_equation_matches_field_def_id_not_spelling() {
    let mut flat_model = flat::Model::new();
    let var = primitive_variable_with_parts(
        "controller.y[1].alpha",
        vec![("controller", vec![]), ("y", vec![1]), ("alpha", vec![])],
        rumoca_core::DefId::new(999),
    );
    flat_model.variables.insert(var.name.clone(), var);
    add_pair_constructor(&mut flat_model);
    add_pair_record_function(&mut flat_model, "Records.makePair");

    let equation = flat::Equation::new_array(
        residual(
            var_ref_with_parts("controller.y", vec![("controller", vec![]), ("y", vec![])]),
            call("Records.makePair"),
        ),
        fixture_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "controller".to_string(),
        },
        1,
    );

    let err = expand_record_field_equation(&equation, &flat_model)
        .expect_err("record expansion must not match a field by spelling when the DefId differs");
    assert!(matches!(err, ToDaeError::RuntimeContractViolation { .. }));
}

#[test]
fn test_classify_record_function_equation_routes_expanded_fields() {
    let mut flat_model = flat::Model::new();
    for (name, dims, parts, def_id) in [
        (
            "R.T",
            vec![3, 3],
            vec![("R", vec![]), ("T", vec![])],
            rumoca_core::DefId::new(11),
        ),
        (
            "R.w",
            vec![3],
            vec![("R", vec![]), ("w", vec![])],
            rumoca_core::DefId::new(12),
        ),
    ] {
        let var = primitive_variable_with_dims_and_parts(name, dims, parts, def_id);
        flat_model.variables.insert(var.name.clone(), var);
    }

    let mut constructor = rumoca_core::Function::new("Frames.Orientation", fixture_span());
    constructor.is_constructor = true;
    constructor.add_input(
        rumoca_core::FunctionParam::new("T", "Real", fixture_span())
            .with_dims(vec![3, 3])
            .with_def_id(rumoca_core::DefId::new(11)),
    );
    constructor.add_input(
        rumoca_core::FunctionParam::new("w", "Real", fixture_span())
            .with_dims(vec![3])
            .with_def_id(rumoca_core::DefId::new(12)),
    );
    flat_model
        .functions
        .insert(constructor.name.clone(), constructor);

    let mut null_rotation = rumoca_core::Function::new("Frames.nullRotation", fixture_span());
    let mut output = rumoca_core::FunctionParam::new("R", "Orientation", fixture_span());
    output.type_class = Some(rumoca_core::ClassType::Record);
    null_rotation.add_output(output);
    flat_model
        .functions
        .insert(null_rotation.name.clone(), null_rotation);

    flat_model.equations.push(flat::Equation::new(
        residual(
            var_ref_with_parts("R", vec![("R", vec![])]),
            call("Frames.nullRotation"),
        ),
        fixture_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "body".to_string(),
        },
    ));

    let mut dae_model = dae::Dae::new();
    for (name, dims) in [("R.T", vec![3, 3]), ("R.w", vec![3])] {
        let var_name = rumoca_core::VarName::new(name);
        let mut var = dae::Variable::new(
            var_name.clone(),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        );
        var.dims = dims;
        dae_model.variables.algebraics.insert(var_name, var);
    }
    let prefix_counts = crate::build_prefix_counts(&flat_model);

    classify_equations(&mut dae_model, &flat_model, &prefix_counts).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 2);
    assert_eq!(dae_model.continuous.equations[0].scalar_count, 9);
    assert_eq!(dae_model.continuous.equations[1].scalar_count, 3);
    assert!(format!("{:?}", dae_model.continuous.equations[0].rhs).contains("R.T"));
    assert!(format!("{:?}", dae_model.continuous.equations[1].rhs).contains("R.w"));
}

#[test]
fn test_discrete_alias_assignment_orients_to_unowned_rhs() {
    let mut dae_model = dae::Dae::new();
    for name in ["phase", "state.phase"] {
        dae_model.variables.discrete_valued.insert(
            rumoca_core::VarName::new(name),
            dae::Variable::new(
                rumoca_core::VarName::new(name),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
    }

    let mut flat_model = flat::Model::new();
    flat_model.equations.push(flat::Equation::new(
        residual(
            var_ref("phase"),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(0),
                span: fixture_span(),
            },
        ),
        fixture_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "medium".to_string(),
        },
    ));
    flat_model.equations.push(flat::Equation::new(
        residual(var_ref("phase"), var_ref("state.phase")),
        fixture_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "medium".to_string(),
        },
    ));

    let counts = collect_discrete_valued_lhs_target_counts(&dae_model, &flat_model);
    let assignments = collect_explicit_discrete_assignments(
        &flat_model.equations[1].residual,
        &dae_model,
        &counts,
        flat_model.equations[1].span,
    )
    .unwrap()
    .expect("alias assignment");

    assert!(assignments.contains_key(&rumoca_core::VarName::new("state.phase")));
    assert!(!assignments.contains_key(&rumoca_core::VarName::new("phase")));
}

#[test]
fn test_discrete_alias_assignment_preserves_indexed_lhs_target() {
    let mut dae_model = dae::Dae::new();
    for name in ["auxiliary", "x"] {
        let var_name = rumoca_core::VarName::new(name);
        let mut variable = dae::Variable::new(
            var_name.clone(),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        );
        variable.dims = vec![2];
        dae_model
            .variables
            .discrete_valued
            .insert(var_name, variable);
    }

    let mut counts = HashMap::new();
    counts.insert(rumoca_core::VarName::new("auxiliary"), 2);
    counts.insert(rumoca_core::VarName::new("x"), 0);

    let expr = residual(var_ref("auxiliary[1]"), var_ref("x[1]"));
    let assignments =
        collect_explicit_discrete_assignments(&expr, &dae_model, &counts, fixture_span())
            .unwrap()
            .expect("indexed alias assignment");

    assert!(assignments.contains_key(&rumoca_core::VarName::new("auxiliary[1]")));
    assert!(!assignments.contains_key(&rumoca_core::VarName::new("x[1]")));
}

#[test]
fn test_zero_discrete_assignment_requires_equation_span() {
    let mut dae_model = dae::Dae::new();
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable::new(
            rumoca_core::VarName::new("x"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    let mut counts = HashMap::new();
    counts.insert(rumoca_core::VarName::new("x"), 1);
    let expr = residual(
        var_ref("x"),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span: fixture_span(),
        },
    );

    let err = collect_explicit_discrete_assignments(&expr, &dae_model, &counts, Span::DUMMY)
        .expect_err("generated zero should require equation provenance");

    assert!(
        err.to_string()
            .contains("explicit discrete assignment zero")
    );
}

#[test]
fn test_explicit_lhs_reference_requires_scalar_target_subscript_provenance() {
    let mut flat_model = flat::Model::new();
    flat_model.variables.insert(
        rumoca_core::VarName::new("x"),
        primitive_variable_with_dims_and_parts(
            "x",
            vec![2],
            vec![("x", Vec::new())],
            rumoca_core::DefId::new(91),
        ),
    );

    let err = explicit_lhs_reference_from_target(
        &rumoca_core::VarName::new("x[1]"),
        &flat_model,
        Span::DUMMY,
    )
    .expect_err("generated scalar target subscript should require provenance");

    assert!(
        err.to_string()
            .contains("explicit discrete assignment target subscript"),
        "unexpected error: {err}"
    );
}

#[test]
fn test_output_has_component_equation_matches_unsubscripted_base() {
    let outputs_with_component_eqs: HashSet<rumoca_core::VarName> =
        [rumoca_core::VarName::new("y")].into_iter().collect();
    assert!(output_has_component_equation(
        &rumoca_core::VarName::new("y[2]"),
        &outputs_with_component_eqs
    ));
}

#[test]
fn test_output_has_component_equation_matches_multilayer_unsubscripted_base() {
    let outputs_with_component_eqs: HashSet<rumoca_core::VarName> =
        [rumoca_core::VarName::new("bus.signal")]
            .into_iter()
            .collect();
    assert!(output_has_component_equation(
        &rumoca_core::VarName::new("bus[1].signal[2]"),
        &outputs_with_component_eqs
    ));
}

#[test]
fn test_output_alias_skip_preserves_internal_input_alias_connection() {
    let flat_model = flat::Model::new();
    let outputs_with_component_eqs: HashSet<rumoca_core::VarName> =
        [rumoca_core::VarName::new("booleanPulse1.y")]
            .into_iter()
            .collect();
    let non_connection_rhs_var_refs: HashSet<rumoca_core::VarName> =
        [rumoca_core::VarName::new("multiSwitch1.u")]
            .into_iter()
            .collect();
    let top_level_oc_connectors: IndexSet<String> = IndexSet::new();
    let ctx = EqFilterContext {
        flat: &flat_model,
        outputs_with_component_eqs: &outputs_with_component_eqs,
        non_connection_rhs_var_refs: &non_connection_rhs_var_refs,
        top_level_oc_connectors: &top_level_oc_connectors,
        debug_eq_filter: false,
    };

    let mut dae_model = dae::Dae::new();
    dae_model.variables.inputs.insert(
        rumoca_core::VarName::new("multiSwitch1.u"),
        dae::Variable::new(
            rumoca_core::VarName::new("multiSwitch1.u"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("multiSwitch1.u"),
        dae::Variable::new(
            rumoca_core::VarName::new("multiSwitch1.u"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let eq = flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("booleanPulse1.y").into(),
                subscripts: vec![],
                span: fixture_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("multiSwitch1.u[1]").into(),
                subscripts: vec![],
                span: fixture_span(),
            }),
            span: fixture_span(),
        },
        fixture_span(),
        flat::EquationOrigin::Connection {
            lhs: "booleanPulse1.y".to_string(),
            rhs: "multiSwitch1.u[1]".to_string(),
        },
    );

    assert!(
        output_alias_skip_reason(&eq, &ctx, &dae_model).is_none(),
        "internal input alias connections must be preserved"
    );
}

#[test]
fn test_output_alias_skip_preserves_discrete_output_alias_connection() {
    let mut flat_model = flat::Model::new();
    flat_model.variables.insert(
        rumoca_core::VarName::new("table1.y"),
        flat::Variable {
            name: rumoca_core::VarName::new("table1.y"),
            is_discrete_type: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    flat_model.variables.insert(
        rumoca_core::VarName::new("table1.realToBoolean.y"),
        flat::Variable {
            name: rumoca_core::VarName::new("table1.realToBoolean.y"),
            is_discrete_type: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );

    let outputs_with_component_eqs: HashSet<rumoca_core::VarName> =
        [rumoca_core::VarName::new("table1.realToBoolean.y")]
            .into_iter()
            .collect();
    let non_connection_rhs_var_refs: HashSet<rumoca_core::VarName> = HashSet::default();
    let top_level_oc_connectors: IndexSet<String> = IndexSet::new();
    let ctx = EqFilterContext {
        flat: &flat_model,
        outputs_with_component_eqs: &outputs_with_component_eqs,
        non_connection_rhs_var_refs: &non_connection_rhs_var_refs,
        top_level_oc_connectors: &top_level_oc_connectors,
        debug_eq_filter: false,
    };

    let mut dae_model = dae::Dae::new();
    dae_model.variables.outputs.insert(
        rumoca_core::VarName::new("table1.y"),
        dae::Variable::new(
            rumoca_core::VarName::new("table1.y"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.outputs.insert(
        rumoca_core::VarName::new("table1.realToBoolean.y"),
        dae::Variable::new(
            rumoca_core::VarName::new("table1.realToBoolean.y"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("table1.y"),
        dae::Variable::new(
            rumoca_core::VarName::new("table1.y"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("table1.realToBoolean.y"),
        dae::Variable::new(
            rumoca_core::VarName::new("table1.realToBoolean.y"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let eq = flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("table1.realToBoolean.y").into(),
                subscripts: vec![],
                span: fixture_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("table1.y").into(),
                subscripts: vec![],
                span: fixture_span(),
            }),
            span: fixture_span(),
        },
        fixture_span(),
        flat::EquationOrigin::Connection {
            lhs: "table1.realToBoolean.y".to_string(),
            rhs: "table1.y".to_string(),
        },
    );

    assert!(
        output_alias_skip_reason(&eq, &ctx, &dae_model).is_none(),
        "discrete output alias connections must be preserved"
    );
}

#[test]
fn test_output_alias_skip_applies_when_both_sides_are_component_defined() {
    let flat_model = flat::Model::new();
    let outputs_with_component_eqs: HashSet<rumoca_core::VarName> = [
        rumoca_core::VarName::new("source.y"),
        rumoca_core::VarName::new("sink.u"),
    ]
    .into_iter()
    .collect();
    let non_connection_rhs_var_refs: HashSet<rumoca_core::VarName> = HashSet::default();
    let top_level_oc_connectors: IndexSet<String> = IndexSet::new();
    let ctx = EqFilterContext {
        flat: &flat_model,
        outputs_with_component_eqs: &outputs_with_component_eqs,
        non_connection_rhs_var_refs: &non_connection_rhs_var_refs,
        top_level_oc_connectors: &top_level_oc_connectors,
        debug_eq_filter: false,
    };

    let dae_model = dae::Dae::new();

    let eq = flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("source.y").into(),
                subscripts: vec![],
                span: fixture_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("sink.u").into(),
                subscripts: vec![],
                span: fixture_span(),
            }),
            span: fixture_span(),
        },
        fixture_span(),
        flat::EquationOrigin::Connection {
            lhs: "source.y".to_string(),
            rhs: "sink.u".to_string(),
        },
    );

    assert_eq!(
        output_alias_skip_reason(&eq, &ctx, &dae_model).as_ref(),
        Some(&rumoca_core::VarName::new("source.y")),
        "alias skip should apply for non-preserved output aliases"
    );
}

#[test]
fn test_classify_equations_preserves_repeated_residuals_for_validation() {
    let residual = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("x").into(),
            subscripts: vec![],
            span: fixture_span(),
        }),
        rhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: fixture_span(),
        }),
        span: fixture_span(),
    };
    let mut flat_model = flat::Model::new();
    flat_model.add_variable(
        rumoca_core::VarName::new("x"),
        flat::Variable {
            name: rumoca_core::VarName::new("x"),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    for component in ["A", "B"] {
        flat_model.add_equation(flat::Equation {
            residual: residual.clone(),
            span: fixture_span(),
            origin: flat::EquationOrigin::ComponentEquation {
                component: component.to_string(),
            },
            scalar_count: 1,
        });
    }

    let mut dae_model = dae::Dae::new();
    classify_equations(
        &mut dae_model,
        &flat_model,
        &rustc_hash::FxHashMap::default(),
    )
    .unwrap();

    assert_eq!(
        dae_model.continuous.equations.len(),
        2,
        "ToDae must preserve repeated source equations instead of hiding them by residual text"
    );
}
