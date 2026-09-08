use super::*;

// Fixture-only function exposure identities in this file use the 30_000 range.
// Tests constructing several functions pass a distinct identity to each one.

/// A `Real y[n]` formal whose extent is only written in `shape_expr`.
///
/// This is the shape Flat gives
/// `Modelica.Electrical.Polyphase.Functions.symmetricOrientation`: the
/// declared dimension names an input, so `effective_type` carries `0` and
/// only `shape_expr` names `n`.
pub(super) fn real_vector_param(name: &str, extent: &str) -> rumoca_core::FunctionParam {
    let real = rumoca_core::TypeId::new(1);
    let effective_type = rumoca_core::EffectiveType::new(real, real, vec![0]).expect("vector type");
    rumoca_core::FunctionParam::new(name, "Real", effective_type, test_span())
        .with_def_id(fixture_def_id(name))
        .with_shape_expr(vec![Subscript::Expr {
            expr: Box::new(var_ref(extent)),
            span: test_span(),
        }])
}

pub(super) fn var_ref(name: &str) -> Expression {
    Expression::VarRef {
        name: fixture_reference(name),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    }
}

pub(super) fn real_literal(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: Span::DUMMY,
    }
}

pub(super) fn integer_literal(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: Span::DUMMY,
    }
}

/// `name[subscripts] := …` as an assignment target.
pub(super) fn element_target(name: &str, subscripts: Vec<Subscript>) -> ComponentReference {
    let def_id = name.bytes().fold(1_u32, |hash, byte| {
        hash.wrapping_mul(16_777_619) ^ u32::from(byte)
    });
    ComponentReference::construct(
        false,
        test_span(),
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: test_span(),
            subs: subscripts,
            def_id: rumoca_core::DefId::new(def_id.max(1)),
        }],
    )
    .expect("fixture assignment target is exact")
}

pub(super) fn assign(comp: ComponentReference, value: Expression) -> Statement {
    Statement::Assignment {
        comp,
        value,
        span: Span::DUMMY,
    }
}

/// An `Integer y[extent]` formal whose extent is a literal dimension.
pub(super) fn integer_vector_param(name: &str, extent: i64) -> rumoca_core::FunctionParam {
    let integer = rumoca_core::TypeId::new(2);
    let effective_type =
        rumoca_core::EffectiveType::new(integer, integer, vec![extent]).expect("vector type");
    rumoca_core::FunctionParam::new(name, "Integer", effective_type, test_span())
        .with_def_id(fixture_def_id(name))
}

pub(super) fn integer_matrix_param(
    name: &str,
    rows: i64,
    columns: i64,
) -> rumoca_core::FunctionParam {
    let integer = rumoca_core::TypeId::new(2);
    let effective_type = rumoca_core::EffectiveType::new(integer, integer, vec![rows, columns])
        .expect("matrix type");
    rumoca_core::FunctionParam::new(name, "Integer", effective_type, test_span())
        .with_def_id(fixture_def_id(name))
}

/// `start:step:end` (or `start:end`) as an assignment-target subscript.
pub(super) fn range_subscript(start: i64, step: Option<i64>, end: i64) -> Subscript {
    Subscript::Expr {
        expr: Box::new(Expression::Range {
            start: Box::new(integer_literal(start)),
            step: step.map(|step| Box::new(integer_literal(step))),
            end: Box::new(integer_literal(end)),
            span: Span::DUMMY,
        }),
        span: test_span(),
    }
}

pub(super) fn integer_vector(values: &[i64]) -> Expression {
    Expression::Array {
        elements: values.iter().copied().map(integer_literal).collect(),
        is_matrix: false,
        span: Span::DUMMY,
    }
}

pub(super) fn integer_matrix(rows: &[&[i64]]) -> Expression {
    Expression::Array {
        elements: rows.iter().map(|row| integer_vector(row)).collect(),
        is_matrix: true,
        span: Span::DUMMY,
    }
}

/// `function f() output Integer y[extent]; algorithm y[<target>] := <value>;`
pub(super) fn slice_writer(
    exposure_def_id: rumoca_core::DefId,
    extent: i64,
    target: Subscript,
    value: Expression,
) -> Function {
    let mut func = Function::new("test.sliceWriter", exposure_def_id, Span::DUMMY);
    func.add_output(integer_vector_param("y", extent));
    func.pure = true;
    func.body = vec![assign(element_target("y", vec![target]), value)];
    func
}

pub(super) fn fold(func: &Function) -> Result<Value, EvalError> {
    eval_function(
        func,
        Vec::new(),
        &EvalContext::structural_preidentity(),
        &EvalLimits::default(),
        0,
        Span::DUMMY,
    )
}

/// MLS 3.6 §10.4.1 gives `3:-1:1` the indices `{3, 2, 1}` (OMC 4.1.0 agrees),
/// and §10.5 writes the value into those slots in that order, so `Integer y[3]`
/// ends as `{3, 2, 1}` — the same answer OMC gives for the equivalent loop.
///
/// The unsigned slice arithmetic this replaced computed `(end - start)` on
/// `usize` and panicked with "attempt to subtract with overflow" in debug,
/// wrapping to a garbage length in release.
#[test]
fn descending_slice_target_writes_each_named_index() {
    let func = slice_writer(
        rumoca_core::DefId::new(30_001),
        3,
        range_subscript(3, Some(-1), 1),
        integer_vector(&[1, 2, 3]),
    );
    assert_eq!(
        fold(&func).expect("descending slice folds"),
        Value::Array(vec![
            Value::Integer(3),
            Value::Integer(2),
            Value::Integer(1)
        ])
    );
}

/// MLS 3.6 §10.4.1 leaves `a:0:b` undefined and OMC rejects it ("Illegal
/// subscript 1:0:3"). It is reported, never divided by: the previous form
/// evaluated `(end - start) / step` and crashed in every profile.
#[test]
fn zero_step_slice_target_is_reported_not_divided_by() {
    let func = slice_writer(
        rumoca_core::DefId::new(30_002),
        3,
        range_subscript(1, Some(0), 3),
        integer_vector(&[1, 2, 3]),
    );
    let error = fold(&func).expect_err("a zero step has no indices");
    assert!(
        matches!(error, EvalError::RangeError { .. }),
        "zero step is a range error: {error}"
    );
}

/// MLS 3.6 §10.6.1 requires both sides of an assignment to have the same sizes,
/// so the empty target `y[2:1]` takes an empty array and nothing else. OMC
/// rejects `y[{}] := {9}` as `Integer[0] := Integer[1]`; the previous form
/// passed its own size check, wrote nothing, and returned success.
#[test]
fn empty_slice_target_takes_only_an_empty_value() {
    let oversized = slice_writer(
        rumoca_core::DefId::new(30_003),
        3,
        range_subscript(2, None, 1),
        integer_vector(&[9]),
    );
    let error = fold(&oversized).expect_err("an empty target cannot take one element");
    assert!(
        matches!(error, EvalError::FunctionError { .. }),
        "size mismatch is reported: {error}"
    );

    let exact = slice_writer(
        rumoca_core::DefId::new(30_004),
        3,
        range_subscript(2, None, 1),
        integer_vector(&[]),
    );
    assert_eq!(
        fold(&exact).expect("an empty slice writes nothing"),
        Value::Array(vec![Value::Integer(0); 3])
    );
}

/// MLS 3.6 §10.5 gives `:` the whole dimension, so `y[:]` is a slice of exactly
/// `size(y, 1)` elements and is size-checked like any other. OMC rejects
/// `Integer[2] := Integer[4]`; treating `:` as a whole-value replacement let the
/// assignment resize the component, which is the very fallback the subscripted
/// dispatch exists to prevent.
#[test]
fn colon_slice_target_is_size_conformant_with_the_container() {
    let oversized = slice_writer(
        rumoca_core::DefId::new(30_005),
        2,
        Subscript::Colon { span: test_span() },
        integer_vector(&[1, 2, 3, 4]),
    );
    let error = fold(&oversized).expect_err("`y[:]` cannot resize `y`");
    assert!(
        matches!(error, EvalError::FunctionError { .. }),
        "size mismatch is reported: {error}"
    );

    let exact = slice_writer(
        rumoca_core::DefId::new(30_006),
        2,
        Subscript::Colon { span: test_span() },
        integer_vector(&[7, 8]),
    );
    assert_eq!(
        fold(&exact).expect("a conformant `y[:]` folds"),
        Value::Array(vec![Value::Integer(7), Value::Integer(8)])
    );

    let scalar = slice_writer(
        rumoca_core::DefId::new(30_007),
        2,
        Subscript::Colon { span: test_span() },
        integer_literal(5),
    );
    assert!(
        fold(&scalar).is_err(),
        "a scalar cannot be assigned to a whole dimension"
    );
}

#[test]
fn vector_only_assignment_uses_the_shared_selection_path() {
    let selector = Subscript::Expr {
        expr: Box::new(integer_vector(&[1, 3])),
        span: test_span(),
    };
    let func = slice_writer(
        rumoca_core::DefId::new(30_008),
        3,
        selector,
        integer_vector(&[7, 8]),
    );
    assert_eq!(
        fold(&func).expect("vector-only selector folds"),
        Value::Array(vec![
            Value::Integer(7),
            Value::Integer(0),
            Value::Integer(8)
        ])
    );
}

#[test]
fn multidimensional_slice_assignment_preserves_every_selected_axis() {
    let mut column = Function::new(
        "test.fillMatrixColumn",
        rumoca_core::DefId::new(30_009),
        Span::DUMMY,
    );
    column.add_output(integer_matrix_param("y", 2, 3));
    column.pure = true;
    column.body = vec![assign(
        element_target(
            "y",
            vec![
                Subscript::Colon { span: test_span() },
                Subscript::Index {
                    value: 2,
                    span: test_span(),
                },
            ],
        ),
        integer_vector(&[7, 8]),
    )];

    assert_eq!(
        fold(&column).expect("a slice followed by a scalar axis folds"),
        Value::Array(vec![
            Value::Array(vec![
                Value::Integer(0),
                Value::Integer(7),
                Value::Integer(0)
            ]),
            Value::Array(vec![
                Value::Integer(0),
                Value::Integer(8),
                Value::Integer(0)
            ]),
        ])
    );

    let mut matrix = Function::new(
        "test.fillMatrix",
        rumoca_core::DefId::new(30_010),
        Span::DUMMY,
    );
    matrix.add_output(integer_matrix_param("y", 2, 3));
    matrix.pure = true;
    matrix.body = vec![assign(
        element_target(
            "y",
            vec![
                range_subscript(1, None, 2),
                Subscript::Colon { span: test_span() },
            ],
        ),
        integer_matrix(&[&[1, 2, 3], &[4, 5, 6]]),
    )];
    assert_eq!(
        fold(&matrix).expect("two sliced dimensions fold"),
        Value::Array(vec![
            Value::Array(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3)
            ]),
            Value::Array(vec![
                Value::Integer(4),
                Value::Integer(5),
                Value::Integer(6)
            ]),
        ])
    );
}

#[test]
fn multidimensional_slice_read_preserves_every_selected_axis() {
    let mut func = Function::new(
        "test.readMatrixSlice",
        rumoca_core::DefId::new(30_011),
        Span::DUMMY,
    );
    func.add_input(integer_matrix_param("x", 3, 2));
    func.add_output(integer_matrix_param("y", 2, 2));
    func.pure = true;
    func.body = vec![assign(
        component_reference("y"),
        Expression::VarRef {
            name: fixture_reference("x"),
            subscripts: vec![
                range_subscript(2, None, 3),
                Subscript::Colon { span: test_span() },
            ],
            span: test_span(),
        },
    )];
    let input = Value::Array(vec![
        Value::Array(vec![Value::Integer(1), Value::Integer(2)]),
        Value::Array(vec![Value::Integer(3), Value::Integer(4)]),
        Value::Array(vec![Value::Integer(5), Value::Integer(6)]),
    ]);

    assert_eq!(
        eval_function(
            &func,
            vec![input],
            &EvalContext::structural_preidentity(),
            &EvalLimits::default(),
            0,
            test_span(),
        )
        .unwrap(),
        Value::Array(vec![
            Value::Array(vec![Value::Integer(3), Value::Integer(4)]),
            Value::Array(vec![Value::Integer(5), Value::Integer(6)]),
        ])
    );
}

#[test]
fn multidimensional_selection_cannot_multiply_past_the_node_budget() {
    let selector = Subscript::Expr {
        expr: Box::new(integer_vector(&[1; 400])),
        span: test_span(),
    };
    let mut func = Function::new(
        "test.boundedMatrixSelection",
        rumoca_core::DefId::new(30_012),
        Span::DUMMY,
    );
    func.add_input(integer_matrix_param("x", 1, 1));
    func.add_output(integer_param("y"));
    func.pure = true;
    func.body = vec![assign(
        component_reference("y"),
        Expression::VarRef {
            name: fixture_reference("x"),
            subscripts: vec![selector.clone(), selector],
            span: test_span(),
        },
    )];

    let error = eval_function(
        &func,
        vec![Value::Array(vec![Value::Array(vec![Value::Integer(7)])])],
        &EvalContext::structural_preidentity(),
        &EvalLimits::default(),
        0,
        test_span(),
    )
    .expect_err("bounded selectors must not create an unbounded Cartesian result");
    assert!(
        matches!(error, EvalError::UnsupportedExpression { .. }),
        "unexpected selection error: {error}"
    );
}

/// MLS 3.6 §12.2 lets a declared extent name an input, and an extent this
/// evaluator cannot settle refuses the call — exactly as an unsettleable
/// binding does. Falling back to the declared `0` built an empty container that
/// `size()` and later loops read as the component's real extent.
#[test]
fn unsettleable_declared_extent_refuses_the_call() {
    let mut func = Function::new(
        "test.unsettled",
        rumoca_core::DefId::new(30_013),
        Span::DUMMY,
    );
    func.add_output(real_vector_param("y", "notInScope"));
    func.pure = true;
    func.body = vec![assign(
        element_target(
            "y",
            vec![Subscript::Index {
                value: 1,
                span: test_span(),
            }],
        ),
        real_literal(1.0),
    )];

    let error = fold(&func).expect_err("an unsettleable extent has no container");
    assert!(
        error.runtime_dependent_reason().is_some(),
        "an unknown extent leaves the value for the runtime: {error}"
    );
}

/// A declared extent that settles to a negative number is reported by name
/// rather than silently clamped.
#[test]
fn negative_declared_extent_is_reported() {
    let mut func = Function::new(
        "test.negative",
        rumoca_core::DefId::new(30_014),
        Span::DUMMY,
    );
    func.add_input(integer_param("n"));
    func.add_output(real_vector_param("y", "n"));
    func.pure = true;
    func.body = vec![assign(component_reference("y"), real_literal(1.0))];

    let error = eval_function(
        &func,
        vec![Value::Integer(-2)],
        &EvalContext::structural_preidentity(),
        &EvalLimits::default(),
        0,
        Span::DUMMY,
    )
    .expect_err("a negative extent is not a shape");
    assert!(error.to_string().contains("negative extent"), "{error}");
}

/// MLS 3.6 §12.4.4: the declaration bindings "are executed in an order where a
/// variable is not used before its binding", and the only error the rule names
/// is that no such order exists. `Integer a = b + 1; Integer b = 2;` is legal
/// and acyclic — OMC folds it to `3` — so the order is topological, not written.
#[test]
fn declaration_bindings_run_in_topological_order() {
    let mut func = Function::new(
        "test.sortedBindings",
        rumoca_core::DefId::new(30_015),
        Span::DUMMY,
    );
    func.add_output(integer_param("y"));
    let mut first = integer_param("a");
    first.default = Some(Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(var_ref("b")),
        rhs: Box::new(integer_literal(1)),
        span: Span::DUMMY,
    });
    let mut second = integer_param("b");
    second.default = Some(integer_literal(2));
    func.add_local(first);
    func.add_local(second);
    func.pure = true;
    func.body = vec![assign(component_reference("y"), var_ref("a"))];

    assert_eq!(
        fold(&func).expect("acyclic bindings fold"),
        Value::Integer(3)
    );
}

/// The one case MLS §12.4.4 calls an error: no order exists.
#[test]
fn cyclic_declaration_bindings_are_reported_by_name() {
    let mut func = Function::new(
        "test.cyclicBindings",
        rumoca_core::DefId::new(30_016),
        Span::DUMMY,
    );
    func.add_output(integer_param("y"));
    let mut first = integer_param("a");
    first.default = Some(var_ref("b"));
    let mut second = integer_param("b");
    second.default = Some(var_ref("a"));
    func.add_local(first);
    func.add_local(second);
    func.pure = true;
    func.body = vec![assign(component_reference("y"), var_ref("a"))];

    let error = fold(&func).expect_err("a binding cycle has no execution order");
    assert!(
        matches!(error, EvalError::CircularDependency { .. }),
        "a cycle is reported as one: {error}"
    );
}

/// MLS 3.6 §10.6.13: an Integer written into a Real component is converted.
/// `Real y[2]` with `y[1] := 0` used to keep an `Integer` in the first slot, so
/// the folded array was `{Integer(0), Real(1.5)}` — structurally unequal to the
/// all-Real `{0.0, 1.5}` OMC returns, which silently broke the branch-agreement
/// comparison an if-expression fold depends on.
#[test]
fn element_write_coerces_to_the_declared_element_type() {
    let real = rumoca_core::TypeId::new(1);
    let effective_type = rumoca_core::EffectiveType::new(real, real, vec![2]).expect("vector type");
    let mut func = Function::new(
        "test.realCoerce",
        rumoca_core::DefId::new(30_017),
        Span::DUMMY,
    );
    func.add_output(
        rumoca_core::FunctionParam::new("y", "Real", effective_type, test_span())
            .with_def_id(fixture_def_id("y")),
    );
    func.pure = true;
    func.body = vec![
        assign(
            element_target(
                "y",
                vec![Subscript::Index {
                    value: 1,
                    span: test_span(),
                }],
            ),
            integer_literal(0),
        ),
        assign(
            element_target(
                "y",
                vec![Subscript::Index {
                    value: 2,
                    span: test_span(),
                }],
            ),
            real_literal(1.5),
        ),
    ];

    assert_eq!(
        fold(&func).expect("declared Real elements fold"),
        Value::Array(vec![Value::Real(0.0), Value::Real(1.5)])
    );
}

/// MLS 3.6 §10.5: `y[1] := 1.0; y[2] := 2.0;` writes two elements of the
/// declared `Real y[n]`, it does not replace `y` with the assigned scalar.
///
/// OMC 4.1.0 evaluates this function at `n = 3` to `{1.0, 2.0, 0.0}`; before
/// the target-dispatch fix the fold returned the scalar `2.0`, which is what
/// made `Modelica.Electrical.Polyphase.Examples.PolyphaseRectifier` read
/// `phase[4]` out of a three-element vector.
#[test]
fn element_assignment_writes_into_the_declared_extent() {
    let mut func = Function::new("test.fillTwo", rumoca_core::DefId::new(30_018), Span::DUMMY);
    func.add_input(integer_param("n"));
    func.add_output(real_vector_param("y", "n"));
    func.pure = true;
    func.body = vec![
        assign(
            element_target(
                "y",
                vec![Subscript::Index {
                    value: 1,
                    span: test_span(),
                }],
            ),
            real_literal(1.0),
        ),
        assign(
            element_target(
                "y",
                vec![Subscript::Index {
                    value: 2,
                    span: test_span(),
                }],
            ),
            real_literal(2.0),
        ),
    ];

    let result = eval_function(
        &func,
        vec![Value::Integer(3)],
        &EvalContext::structural_preidentity(),
        &EvalLimits::default(),
        0,
        Span::DUMMY,
    )
    .expect("fillTwo(3) folds");

    assert_eq!(
        result,
        Value::Array(vec![Value::Real(1.0), Value::Real(2.0), Value::Real(0.0)])
    );
}

/// The same rule for a range target: `y[1:2] := {1.0, 2.0}` writes the first
/// two elements of the declared `Real y[n]` and leaves the extent alone.
#[test]
fn slice_assignment_keeps_the_declared_extent() {
    let mut func = Function::new(
        "test.fillSlice",
        rumoca_core::DefId::new(30_019),
        Span::DUMMY,
    );
    func.add_input(integer_param("n"));
    func.add_output(real_vector_param("y", "n"));
    func.pure = true;
    func.body = vec![assign(
        element_target(
            "y",
            vec![Subscript::Expr {
                expr: Box::new(Expression::Range {
                    start: Box::new(integer_literal(1)),
                    step: None,
                    end: Box::new(integer_literal(2)),
                    span: Span::DUMMY,
                }),
                span: test_span(),
            }],
        ),
        Expression::Array {
            elements: vec![real_literal(1.0), real_literal(2.0)],
            is_matrix: false,
            span: Span::DUMMY,
        },
    )];

    let result = eval_function(
        &func,
        vec![Value::Integer(3)],
        &EvalContext::structural_preidentity(),
        &EvalLimits::default(),
        0,
        Span::DUMMY,
    )
    .expect("fillSlice(3) folds");

    assert_eq!(
        result,
        Value::Array(vec![Value::Real(1.0), Value::Real(2.0), Value::Real(0.0)])
    );
}

/// MLS 3.6 §12.4.4: a protected local's declaration equation is the value it
/// holds on entry, so `Integer k = 2*m + 1` is `7` at `m = 3` and the
/// function returns `70.0` — the value OMC 4.1.0 reports. Substituting the
/// type default folded it to `0.0`, and in
/// `Modelica.Electrical.Polyphase.Functions.factorY2DC` the same
/// substitution turned the local into the `0` divisor of `pi/mBasic`.
#[test]
fn local_declaration_binding_is_the_entry_value() {
    let mut func = Function::new("test.scale", rumoca_core::DefId::new(30_020), Span::DUMMY);
    func.add_input(integer_param("m"));
    func.add_output(real_param("y"));
    let mut local = integer_param("k");
    local.default = Some(Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(integer_literal(2)),
            rhs: Box::new(var_ref("m")),
            span: Span::DUMMY,
        }),
        rhs: Box::new(integer_literal(1)),
        span: Span::DUMMY,
    });
    func.add_local(local);
    func.pure = true;
    func.body = vec![assign(
        component_reference("y"),
        Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(real_literal(10.0)),
            rhs: Box::new(var_ref("k")),
            span: Span::DUMMY,
        },
    )];

    let result = eval_function(
        &func,
        vec![Value::Integer(3)],
        &EvalContext::structural_preidentity(),
        &EvalLimits::default(),
        0,
        Span::DUMMY,
    )
    .expect("scale(3) folds");

    assert_eq!(result, Value::Real(70.0));
}

/// A declaration equation this evaluator cannot settle refuses the whole
/// call instead of substituting a value the entry state never had, and the
/// refusal is typed as runtime-dependent so a caller folding bindings
/// opportunistically skips the binding rather than rejecting the model.
#[test]
fn unevaluable_local_binding_refuses_the_call() {
    let mut func = Function::new("test.opaque", rumoca_core::DefId::new(30_021), Span::DUMMY);
    func.add_output(real_param("y"));
    let mut local = real_param("k");
    local.default = Some(var_ref("notInScope"));
    func.add_local(local);
    func.pure = true;
    func.body = vec![assign(component_reference("y"), real_literal(1.0))];

    let error = eval_function(
        &func,
        Vec::new(),
        &EvalContext::structural_preidentity(),
        &EvalLimits::default(),
        0,
        Span::DUMMY,
    )
    .expect_err("an unsettled declaration equation refuses the fold");
    assert!(error.runtime_dependent_reason().is_some(), "{error}");
}

/// MLS 3.6 §12.6: a record constructor call folds to the record built from
/// its components, not to the tuple of the (absent) output formals. Reading
/// it as an ordinary function folded `Complex(re, im)` to `Array([])`, which
/// is what `Modelica.ComplexMath.exp` returned into a field access.
#[test]
fn record_constructor_folds_to_the_record_value() {
    let mut func = Function::new("Complex", rumoca_core::DefId::new(30_022), Span::DUMMY);
    func.add_input(real_param("re"));
    func.add_input(real_param("im"));
    func.is_constructor = true;
    func.pure = true;

    let result = eval_function(
        &func,
        vec![Value::Real(1.5), Value::Real(-2.5)],
        &EvalContext::structural_preidentity(),
        &EvalLimits::default(),
        0,
        Span::DUMMY,
    )
    .expect("record constructor folds");

    let fields = result.as_record().expect("record value");
    assert_eq!(fields.get("re"), Some(&Value::Real(1.5)));
    assert_eq!(fields.get("im"), Some(&Value::Real(-2.5)));
    assert_eq!(fields.len(), 2);
}

/// Flat gives a record-valued function call an explicit projection through
/// its sole output before projecting the record's own field. Constant
/// evaluation returns the sole output directly, so the output projection is
/// discharged by its exact call/output identities rather than searched as a
/// field inside the returned record.
#[test]
fn exact_record_output_projection_is_not_read_as_a_record_field() {
    let orientation = rumoca_core::DefId::new(100);
    let axes_id = rumoca_core::DefId::new(101);
    let output_r = rumoca_core::DefId::new(102);
    let field_t = rumoca_core::DefId::new(103);
    let field_w = rumoca_core::DefId::new(104);

    let mut constructor = Function::new("Orientation", orientation, test_span());
    constructor.def_id = Some(orientation);
    constructor.is_constructor = true;
    constructor.pure = true;
    constructor.add_input(real_param("T").with_def_id(field_t));
    constructor.add_input(real_param("w").with_def_id(field_w));

    let mut axes = Function::new("axes", axes_id, test_span());
    axes.def_id = Some(axes_id);
    axes.pure = true;
    axes.add_output(
        function_param("R", "Orientation", rumoca_core::TypeId::new(20))
            .with_def_id(output_r)
            .with_type_class(rumoca_core::ClassType::Record),
    );
    axes.body.push(rumoca_core::Statement::Assignment {
        comp: rumoca_core::ComponentReference::construct(
            false,
            test_span(),
            vec![rumoca_core::ComponentRefPart {
                ident: "R".to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id: output_r,
            }],
        )
        .unwrap(),
        value: Expression::FunctionCall {
            name: exact_reference("Orientation", orientation),
            args: vec![real_literal(1.5), real_literal(2.5)],
            is_constructor: true,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            span: test_span(),
        },
        span: test_span(),
    });

    let mut ctx = EvalContext::structural_preidentity();
    ctx.insert_direct_function_fixture(constructor);
    ctx.insert_direct_function_fixture(axes);
    let output = Expression::FieldAccess {
        base: Box::new(Expression::FunctionCall {
            name: exact_reference("axes", axes_id),
            args: Vec::new(),
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            span: test_span(),
        }),
        field: "R".to_string(),
        field_def_id: output_r,
        span: test_span(),
    };
    let field = Expression::FieldAccess {
        base: Box::new(output),
        field: "T".to_string(),
        field_def_id: field_t,
        span: test_span(),
    };

    assert_eq!(
        crate::constant::eval_expr(&field, &ctx).expect("exact output projection folds"),
        Value::Real(1.5)
    );
}

#[test]
fn uninitialized_structured_output_is_never_fabricated() {
    let mut function = Function::new(
        "uninitializedRecord",
        rumoca_core::DefId::new(30_023),
        test_span(),
    );
    function.pure = true;
    function.add_output(
        function_param("result", "Orientation", rumoca_core::TypeId::new(20))
            .with_type_class(rumoca_core::ClassType::Record),
    );

    assert!(matches!(
        eval_function(
            &function,
            Vec::new(),
            &EvalContext::structural_preidentity(),
            &EvalLimits::default(),
            0,
            test_span(),
        ),
        Err(EvalError::UnsupportedExpression { .. })
    ));
}

/// MLS 3.6 §12.2: a record local's field is read through the joined
/// reference Flat renders, so `z.im` is the field `im` of the bound `z`.
///
/// The qualified-enumeration fallback used to claim the reference first, so
/// reading a record field folded to the enumeration value `z.im` — a wrong
/// value that no later stage could tell from a real enumeration literal.
/// A reference whose head is not in scope must still have registered enum
/// identity; its dotted spelling cannot manufacture a semantic value.
#[test]
fn record_field_read_is_not_guessed_as_an_enumeration_literal() {
    let ctx = EvalContext::structural_preidentity();
    let limits = EvalLimits::default();
    let eval = EvalState {
        ctx: &ctx,
        limits: &limits,
        depth: 0,
        span: Span::DUMMY,
    };
    let mut locals = IndexMap::new();
    locals.insert(
        "z".to_string(),
        Value::Record(
            [
                ("re".to_string(), Value::Real(3.0)),
                ("im".to_string(), Value::Real(4.5)),
            ]
            .into_iter()
            .collect(),
        ),
    );
    let env = FunctionEnv {
        declarations: vec![(Some(fixture_def_id("z")), "z".to_string())],
        formal_extents: Vec::new(),
        loop_bindings: Vec::new(),
        inputs: IndexMap::new(),
        outputs: IndexMap::new(),
        locals,
        declared_outputs: IndexSet::new(),
        declared_locals: ["z".to_string()].into_iter().collect(),
    };

    let Ok(field) = eval_var_ref(
        &qualified_reference(
            "z",
            fixture_def_id("z"),
            "im",
            rumoca_core::DefId::new(90_001),
        ),
        &[],
        &env,
        &eval,
    ) else {
        panic!("record field must be readable");
    };
    assert_eq!(field, Value::Real(4.5));

    let missing = eval_var_ref(
        &qualified_reference(
            "z",
            fixture_def_id("z"),
            "absent",
            rumoca_core::DefId::new(90_002),
        ),
        &[],
        &env,
        &eval,
    )
    .expect_err("a bound head settles the reference");
    assert_eq!(
        missing.runtime_dependent_reason(),
        Some(crate::constant::RuntimeDependentReason::UnimplementedForm),
        "a field this evaluator cannot follow refuses; it never invents a value: {missing}"
    );

    // A registered literal still folds normally.
    let literal = eval_var_ref(
        &rumoca_core::Reference::new("Modelica.Types.Init.NoInit"),
        &[],
        &env,
        &eval,
    )
    .expect("registered enum literal");
    assert_eq!(
        literal,
        Value::Enum("Modelica.Types.Init".to_string(), "NoInit".to_string())
    );
}

/// `input Real x[<shape>]` as Flat lowers a written shape: every subscript is
/// retained in `shape_expr` and the effective type carries the literal
/// extents with `0` standing in for `:`.
fn written_shape_real_param(name: &str, shape: &[Option<i64>]) -> rumoca_core::FunctionParam {
    let real = rumoca_core::TypeId::new(1);
    let dimensions: Vec<i64> = shape.iter().map(|extent| extent.unwrap_or(0)).collect();
    let effective_type =
        rumoca_core::EffectiveType::new(real, real, dimensions).expect("array type");
    let shape_expr = shape
        .iter()
        .map(|extent| match extent {
            Some(extent) => Subscript::index(*extent, test_span()),
            None => Subscript::Colon { span: test_span() },
        })
        .collect();
    rumoca_core::FunctionParam::new(name, "Real", effective_type, test_span())
        .with_def_id(fixture_def_id(name))
        .with_shape_expr(shape_expr)
}

/// `function f input Real x[<shape>]; output Integer n; algorithm n := size(x, axis); end f;`
fn size_reader(exposure_def_id: rumoca_core::DefId, shape: &[Option<i64>], axis: i64) -> Function {
    let mut func = Function::new("test.sizeReader", exposure_def_id, Span::DUMMY);
    func.add_input(written_shape_real_param("x", shape));
    func.add_output(integer_param("n"));
    func.pure = true;
    func.body = vec![assign(
        component_reference("n"),
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args: vec![var_ref("x"), integer_literal(axis)],
            span: Span::DUMMY,
        },
    )];
    func
}

fn real_vector_value(len: usize) -> Value {
    Value::Array((0..len).map(|value| Value::Real(value as f64)).collect())
}

fn call_with(func: &Function, actual: Value) -> Result<Value, EvalError> {
    eval_function_with_call_args(
        func,
        vec![FunctionCallArg::positional(actual)],
        &EvalContext::structural_preidentity(),
        &EvalLimits::default(),
        0,
        Span::DUMMY,
    )
}

/// MLS §12.4.5: `input Real x[:]` takes its extent from the actual, so a
/// three-element vector is accepted and `size(x, 1)` in the body reads 3.
/// The written `:` survives lowering as `Subscript::Colon`; the evaluator
/// must classify that dimension from the subscript, not from the `0` the
/// effective type carries in its place.
#[test]
fn unspecified_input_extent_accepts_the_actual_and_binds_its_size() {
    let vector = size_reader(rumoca_core::DefId::new(30_031), &[None], 1);
    assert_eq!(
        call_with(&vector, real_vector_value(3)).expect("`x[:]` accepts a 3-vector"),
        Value::Integer(3)
    );
    let matrix = size_reader(rumoca_core::DefId::new(30_032), &[None, None], 2);
    let two_by_four = Value::Array(vec![real_vector_value(4), real_vector_value(4)]);
    assert_eq!(
        call_with(&matrix, two_by_four).expect("`x[:, :]` accepts a 2x4 matrix"),
        Value::Integer(4)
    );
    let three_rows = size_reader(rumoca_core::DefId::new(30_033), &[Some(3), None], 2);
    let three_by_five = Value::Array(vec![real_vector_value(5); 3]);
    assert_eq!(
        call_with(&three_rows, three_by_five).expect("`x[3, :]` accepts a 3x5 matrix"),
        Value::Integer(5)
    );
}

/// A literal extent is an obligation the actual has to meet; the
/// unspecified-extent rule does not relax it in any dimension.
#[test]
fn fixed_input_extent_still_refuses_a_different_actual() {
    let pair = size_reader(rumoca_core::DefId::new(30_034), &[Some(2)], 1);
    let error = call_with(&pair, real_vector_value(3)).expect_err("`x[2]` refuses a 3-vector");
    assert!(
        matches!(error, EvalError::FunctionError { .. }),
        "literal extent mismatch is a function error: {error}"
    );
    assert!(error.to_string().contains("expected [2]"), "{error}");

    let three_rows = size_reader(rumoca_core::DefId::new(30_035), &[Some(3), None], 2);
    let two_by_five = Value::Array(vec![real_vector_value(5); 2]);
    let error = call_with(&three_rows, two_by_five).expect_err("`x[3, :]` refuses a 2x5 matrix");
    assert!(
        matches!(error, EvalError::FunctionError { .. }),
        "leading literal extent mismatch is a function error: {error}"
    );
    assert!(error.to_string().contains("expected [3, :]"), "{error}");
}

/// An unspecified extent still declares a dimension, so rank stays exact: a
/// scalar against `x[:]` and a vector against `x[:, :]` are both refused.
#[test]
fn unspecified_input_extent_still_refuses_a_rank_mismatch() {
    let vector = size_reader(rumoca_core::DefId::new(30_036), &[None], 1);
    let error = call_with(&vector, Value::Real(1.0)).expect_err("`x[:]` refuses a scalar");
    assert!(
        matches!(error, EvalError::FunctionError { .. }),
        "scalar against a vector formal is a function error: {error}"
    );

    let matrix = size_reader(rumoca_core::DefId::new(30_037), &[None, None], 2);
    let error = call_with(&matrix, real_vector_value(3)).expect_err("`x[:, :]` refuses a vector");
    assert!(
        matches!(error, EvalError::FunctionError { .. }),
        "vector against a matrix formal is a function error: {error}"
    );
}
