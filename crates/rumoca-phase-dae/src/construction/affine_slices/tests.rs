mod constants;

use super::*;
use rumoca_core::{Reference, SourceMap};

fn span() -> Span {
    let mut sources = SourceMap::new();
    let source = sources.add("slice.mo", "i-3:i+3");
    Span::from_offsets(source, 0, 7)
}

fn integer(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: span(),
    }
}

fn reference(name: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span: span(),
    }
}

fn binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: span(),
    }
}

fn range(start: Expression, step: i64, end: Expression) -> Expression {
    Expression::Range {
        start: Box::new(start),
        step: Some(Box::new(integer(step))),
        end: Box::new(end),
        span: span(),
    }
}

fn scope(lower: i64, upper: i64) -> ShapeEnvironment {
    let mut scope = ShapeEnvironment::default();
    scope.bind_slice_binder(VarName::new("i"), lower, upper);
    scope
}

#[test]
fn checked_affine_slice_matches_independent_enumeration_and_shape() {
    for (lower, upper) in [(-20, 30), (1, 90), (7, 7)] {
        for (start, step, end) in [
            (-3, 1, 3),
            (3, -2, -3),
            (-3, 2, 4),
            (3, 1, -3),
            (0, i64::MIN, 0),
        ] {
            let expression = range(
                binary(OpBinary::Add, reference("i"), integer(start)),
                step,
                binary(OpBinary::Add, reference("i"), integer(end)),
            );
            let scope = scope(lower, upper);
            let checked = plan(&expression, &scope).expect("complete bounded affine plan");
            let shape = call_free_expression_shape(&expression, &scope).unwrap();
            assert_eq!(shape, vec![checked.extent]);
            for i in lower..=upper {
                check_independent_points(&checked, i, start, step, end);
            }
        }
    }
}

fn check_independent_points(checked: &SlicePlan<'_>, i: i64, start: i64, step: i64, end: i64) {
    let mut expected = Vec::new();
    let mut value = i128::from(i) + i128::from(start);
    let stop = i128::from(i) + i128::from(end);
    while if step > 0 {
        value <= stop
    } else {
        value >= stop
    } {
        expected.push(i64::try_from(value).unwrap());
        value += i128::from(step);
    }
    let mut values = EvalContext::new();
    values.add_parameter("i", EvalValue::Integer(i));
    for (ordinal, expected) in expected.iter().enumerate() {
        values.add_parameter("k", EvalValue::Integer(ordinal as i64 + 1));
        assert_eq!(
            eval_expr(&checked.coordinate("k"), &values).unwrap(),
            EvalValue::Integer(*expected)
        );
    }
    assert_eq!(expected.len(), checked.extent as usize);
}

#[test]
fn checked_affine_slice_preserves_original_intermediate_overflow_refusal() {
    let overflow = binary(
        OpBinary::Sub,
        binary(OpBinary::Add, reference("i"), integer(1)),
        integer(1),
    );
    assert!(
        plan(
            &range(overflow, 1, reference("i")),
            &scope(i64::MAX, i64::MAX)
        )
        .is_none()
    );
    assert!(
        plan(
            &range(
                reference("i"),
                1,
                binary(OpBinary::Add, reference("i"), integer(1))
            ),
            &scope(0, i64::MAX)
        )
        .is_none()
    );
    let scaled = binary(OpBinary::Mul, reference("i"), integer(2));
    assert!(plan(&range(scaled.clone(), 1, scaled), &scope(i64::MIN, 1)).is_none());
}

#[test]
fn checked_affine_slice_refuses_runtime_values_nonaffine_and_unshared_endpoints() {
    let expression = range(
        reference("i"),
        1,
        binary(OpBinary::Add, reference("i"), integer(3)),
    );
    let mut runtime = ShapeEnvironment::default();
    runtime.bind_integer_bounds(VarName::new("i"), 1, 90);
    assert!(plan(&expression, &runtime).is_none());
    let mut scope = scope(1, 90);
    scope.bind_slice_binder(VarName::new("j"), 1, 90);
    assert!(plan(&range(reference("i"), 1, reference("j")), &scope).is_none());
    let nonaffine = binary(OpBinary::Mul, reference("i"), reference("i"));
    assert!(plan(&range(nonaffine.clone(), 1, nonaffine), &scope).is_none());
    assert!(plan(&range(reference("i"), 0, reference("i")), &scope).is_none());
}

#[test]
fn checked_affine_slice_refuses_calls_even_if_both_endpoints_match() {
    let call = Expression::FunctionCall {
        name: Reference::new("Identity"),
        args: vec![reference("i")],
        is_constructor: false,
        span: span(),
    };
    assert!(plan(&range(call.clone(), 1, call), &scope(1, 90)).is_none());
    let call = Expression::BuiltinCall {
        function: BuiltinFunction::Integer,
        args: vec![reference("i")],
        span: span(),
    };
    assert!(plan(&range(call.clone(), 1, call), &scope(1, 90)).is_none());
}

#[test]
fn checked_affine_slice_shadowing_clears_lexical_authority() {
    let expression = range(
        reference("i"),
        1,
        binary(OpBinary::Add, reference("i"), integer(3)),
    );
    let mut scope = scope(1, 90);
    scope.insert(VarName::new("i"), Vec::new());
    assert!(plan(&expression, &scope).is_none());
    scope.bind_scalar_value(VarName::new("i"), EvalValue::Integer(5));
    assert!(plan(&expression, &scope).is_none());
}

#[test]
fn checked_affine_slice_requires_identical_reference_occurrences_except_spans() {
    let mut scope = scope(1, 90);
    scope.bind_slice_binder(VarName::new("j"), 1, 90);
    let changed = Expression::VarRef {
        name: Reference::new("i").with_instance_id(rumoca_core::InstanceId::new(123)),
        subscripts: Vec::new(),
        span: span(),
    };
    assert!(plan(&range(reference("i"), 1, changed), &scope).is_none());
    let extra = binary(
        OpBinary::Add,
        reference("i"),
        binary(OpBinary::Sub, reference("j"), reference("j")),
    );
    assert!(plan(&range(reference("i"), 1, extra), &scope).is_none());
}

#[test]
fn checked_affine_slice_rejects_both_wrong_coordinate_instances() {
    let forged = Expression::VarRef {
        name: Reference::new("i").with_instance_id(rumoca_core::InstanceId::new(123)),
        subscripts: Vec::new(),
        span: span(),
    };
    assert!(
        plan(
            &range(forged.clone(), 1, binary(OpBinary::Add, forged, integer(3))),
            &scope(1, 90)
        )
        .is_none()
    );
}

fn instance_reference(instance: u32) -> Expression {
    Expression::VarRef {
        name: Reference::new("i").with_instance_id(rumoca_core::InstanceId::new(instance)),
        subscripts: Vec::new(),
        span: span(),
    }
}

fn occurrence_scope() -> ShapeEnvironment {
    let mut flat = flat::Model::default();
    for (instance, kind) in [
        (1, flat::InstanceKind::Class),
        (2, flat::InstanceKind::Class),
        (3, flat::InstanceKind::Aggregate),
        (4, flat::InstanceKind::Materialized),
    ] {
        flat.instance_relations.insert(
            rumoca_core::InstanceId::new(instance),
            flat::InstanceRelation {
                owner: None,
                declaration: None,
                indices: Box::default(),
                kind,
            },
        );
    }
    let mut shapes = scope(1, 90);
    shapes.bind_slice_class_scopes(&flat);
    shapes
}

#[test]
fn checked_affine_slice_class_metadata_preserves_dense_name_binding() {
    // Flat attaches the enclosing class occurrence to lexical references.
    // BinderSubstitution still binds names; either checked class occurrence
    // preserves that meaning without granting a different lexical domain.
    let shapes = occurrence_scope();
    for instance in [1, 2] {
        let expression = range(
            instance_reference(instance),
            1,
            binary(OpBinary::Add, instance_reference(instance), integer(2)),
        );
        let checked = plan(&expression, &shapes).unwrap();
        for i in [1, 7, 90] {
            let mut values = EvalContext::new();
            values.add_parameter("i", EvalValue::Integer(i));
            let EvalValue::Array(dense) = eval_expr(&expression, &values).unwrap() else {
                panic!("dense range");
            };
            assert_eq!(dense.len(), checked.extent as usize);
            for (ordinal, expected) in dense.iter().enumerate() {
                values.add_parameter("k", EvalValue::Integer(ordinal as i64 + 1));
                assert_eq!(
                    eval_expr(&checked.coordinate("k"), &values).unwrap(),
                    *expected
                );
            }
        }
    }
}

#[test]
fn checked_affine_slice_occurrence_kind_never_creates_binder_authority() {
    let shapes = occurrence_scope();
    for instance in [3, 4, 123] {
        let expression = range(
            instance_reference(instance),
            1,
            binary(OpBinary::Add, instance_reference(instance), integer(2)),
        );
        assert!(plan(&expression, &shapes).is_none());
    }
    assert!(
        plan(
            &range(instance_reference(1), 1, instance_reference(2)),
            &shapes
        )
        .is_none()
    );
    let mut no_binder = ShapeEnvironment::default();
    let flat = flat::Model::default();
    no_binder.bind_slice_class_scopes(&flat);
    assert!(
        plan(
            &range(instance_reference(1), 1, instance_reference(1)),
            &no_binder
        )
        .is_none()
    );
}

#[test]
fn checked_affine_slice_uses_existing_dense_name_contract_for_declaration_metadata() {
    // Canonical BinderSubstitution binds display_names, not supplemental source
    // DefIds. Preserve that old dense meaning rather than inventing a header
    // declaration identity the canonical domain does not retain.
    for declaration in [17, 91] {
        let component = rumoca_core::ComponentReference::construct(
            false,
            span(),
            vec![rumoca_core::ComponentRefPart {
                ident: "i".into(),
                span: span(),
                subs: Vec::new(),
                def_id: rumoca_core::DefId::new(declaration),
            }],
        )
        .unwrap();
        let reference = Expression::VarRef {
            name: Reference::from_component_reference(component),
            subscripts: Vec::new(),
            span: span(),
        };
        let expression = range(
            binary(OpBinary::Sub, reference.clone(), integer(2)),
            2,
            binary(OpBinary::Add, reference, integer(3)),
        );
        let checked = plan(&expression, &scope(1, 90)).unwrap();
        for i in [1, 7, 90] {
            let mut values = EvalContext::new();
            values.add_parameter("i", EvalValue::Integer(i));
            let EvalValue::Array(dense) = eval_expr(&expression, &values).unwrap() else {
                panic!("dense range");
            };
            assert_eq!(dense.len(), checked.extent as usize);
            for (ordinal, expected) in dense.iter().enumerate() {
                values.add_parameter("k", EvalValue::Integer(ordinal as i64 + 1));
                assert_eq!(
                    eval_expr(&checked.coordinate("k"), &values).unwrap(),
                    *expected
                );
            }
        }
    }
}
