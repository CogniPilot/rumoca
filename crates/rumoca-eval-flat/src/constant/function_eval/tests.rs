//! Unit tests for the user-function interpreter.

use super::*;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("function_eval_test.mo"),
        1,
        2,
    )
}

fn function_param(
    name: &str,
    type_name: &str,
    type_id: rumoca_core::TypeId,
) -> rumoca_core::FunctionParam {
    let effective_type = rumoca_core::EffectiveType::new(type_id, type_id, Vec::new())
        .expect("fixture function type is valid");
    rumoca_core::FunctionParam::new(name, type_name, effective_type, test_span())
}

fn real_param(name: &str) -> rumoca_core::FunctionParam {
    function_param(name, "Real", rumoca_core::TypeId::new(1))
}

fn integer_param(name: &str) -> rumoca_core::FunctionParam {
    function_param(name, "Integer", rumoca_core::TypeId::new(2))
}

fn component_reference(name: &str) -> rumoca_core::ComponentReference {
    let def_id = name.bytes().fold(1_u32, |hash, byte| {
        hash.wrapping_mul(16_777_619) ^ u32::from(byte)
    });
    rumoca_core::ComponentReference::construct(
        false,
        test_span(),
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: test_span(),
            subs: Vec::new(),
            def_id: rumoca_core::DefId::new(def_id.max(1)),
        }],
    )
    .expect("fixture assignment target is exact")
}

fn exact_reference(name: &str, def_id: rumoca_core::DefId) -> rumoca_core::Reference {
    rumoca_core::Reference::with_component_reference(
        name,
        rumoca_core::ComponentReference::construct(
            false,
            test_span(),
            vec![rumoca_core::ComponentRefPart {
                ident: name.to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id,
            }],
        )
        .expect("fixture reference has exact identity"),
    )
}

fn make_simple_function() -> Function {
    // function f(input Real x) output Real y; algorithm y := x * 2; end f;
    let mut func = Function::new("test.f", Span::DUMMY);
    func.add_input(real_param("x"));
    func.add_output(real_param("y"));
    func.pure = true;

    // y := x * 2
    func.body = vec![rumoca_core::Statement::Assignment {
        comp: component_reference("y"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new("x"),
                subscripts: Vec::new(),
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];

    func
}

#[test]
fn test_simple_function() {
    let func = make_simple_function();
    let mut ctx = EvalContext::new();
    ctx.functions.insert("test.f".to_string(), func.clone());

    let result = eval_function(
        &func,
        vec![Value::Real(5.0)],
        &ctx,
        &EvalLimits::default(),
        0,
        Span::DUMMY,
    )
    .unwrap();

    assert!((result.to_real().unwrap() - 10.0).abs() < 1e-10);
}

#[test]
fn test_named_argument_binding() {
    let mut func = Function::new("test.third", Span::DUMMY);
    func.add_input(integer_param("x"));
    func.add_input(integer_param("y"));
    func.add_input(integer_param("z"));
    func.add_output(integer_param("result"));
    func.pure = true;
    func.body = vec![rumoca_core::Statement::Assignment {
        comp: component_reference("result"),
        value: rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("z"),
            subscripts: Vec::new(),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
    }];

    let result = eval_function_with_call_args(
        &func,
        vec![
            FunctionCallArg::positional(Value::Integer(1)),
            FunctionCallArg::positional(Value::Integer(2)),
            FunctionCallArg::named("z".to_string(), Value::Integer(7)),
        ],
        &EvalContext::new(),
        &EvalLimits::default(),
        0,
        Span::DUMMY,
    )
    .unwrap();

    assert_eq!(result.as_integer(), Some(7));
}

#[test]
fn eval_var_ref_qualified_enum_split_ignores_dots_inside_indices() {
    let env = FunctionEnv {
        inputs: IndexMap::new(),
        outputs: IndexMap::new(),
        locals: IndexMap::new(),
    };
    let ctx = EvalContext::new();
    let limits = EvalLimits::default();
    let eval = EvalState {
        ctx: &ctx,
        limits: &limits,
        depth: 0,
        span: Span::DUMMY,
    };

    let enum_value = eval_var_ref(
        &rumoca_core::Reference::new("Modelica.Types.Color.red"),
        &[],
        &env,
        &eval,
    )
    .expect("qualified enum fallback");
    assert_eq!(
        enum_value,
        Value::Enum("Modelica.Types.Color".to_string(), "red".to_string())
    );

    assert!(
        eval_var_ref(
            &rumoca_core::Reference::new("data[index.with.dot]"),
            &[],
            &env,
            &eval
        )
        .is_err()
    );
}

/// A `Real y[n]` formal whose extent is only written in `shape_expr`.
///
/// This is the shape Flat gives
/// `Modelica.Electrical.Polyphase.Functions.symmetricOrientation`: the
/// declared dimension names an input, so `effective_type` carries `0` and
/// only `shape_expr` names `n`.
fn real_vector_param(name: &str, extent: &str) -> rumoca_core::FunctionParam {
    let real = rumoca_core::TypeId::new(1);
    let effective_type = rumoca_core::EffectiveType::new(real, real, vec![0]).expect("vector type");
    rumoca_core::FunctionParam::new(name, "Real", effective_type, test_span()).with_shape_expr(
        vec![Subscript::Expr {
            expr: Box::new(var_ref(extent)),
            span: test_span(),
        }],
    )
}

fn var_ref(name: &str) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    }
}

fn real_literal(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: Span::DUMMY,
    }
}

fn integer_literal(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: Span::DUMMY,
    }
}

/// `name[subscripts] := …` as an assignment target.
fn element_target(name: &str, subscripts: Vec<Subscript>) -> ComponentReference {
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

fn assign(comp: ComponentReference, value: Expression) -> Statement {
    Statement::Assignment {
        comp,
        value,
        span: Span::DUMMY,
    }
}

/// An `Integer y[extent]` formal whose extent is a literal dimension.
fn integer_vector_param(name: &str, extent: i64) -> rumoca_core::FunctionParam {
    let integer = rumoca_core::TypeId::new(2);
    let effective_type =
        rumoca_core::EffectiveType::new(integer, integer, vec![extent]).expect("vector type");
    rumoca_core::FunctionParam::new(name, "Integer", effective_type, test_span())
}

/// `start:step:end` (or `start:end`) as an assignment-target subscript.
fn range_subscript(start: i64, step: Option<i64>, end: i64) -> Subscript {
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

fn integer_vector(values: &[i64]) -> Expression {
    Expression::Array {
        elements: values.iter().copied().map(integer_literal).collect(),
        is_matrix: false,
        span: Span::DUMMY,
    }
}

/// `function f() output Integer y[extent]; algorithm y[<target>] := <value>;`
fn slice_writer(extent: i64, target: Subscript, value: Expression) -> Function {
    let mut func = Function::new("test.sliceWriter", Span::DUMMY);
    func.add_output(integer_vector_param("y", extent));
    func.pure = true;
    func.body = vec![assign(element_target("y", vec![target]), value)];
    func
}

fn fold(func: &Function) -> Result<Value, EvalError> {
    eval_function(
        func,
        Vec::new(),
        &EvalContext::new(),
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
    let oversized = slice_writer(3, range_subscript(2, None, 1), integer_vector(&[9]));
    let error = fold(&oversized).expect_err("an empty target cannot take one element");
    assert!(
        matches!(error, EvalError::FunctionError { .. }),
        "size mismatch is reported: {error}"
    );

    let exact = slice_writer(3, range_subscript(2, None, 1), integer_vector(&[]));
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
        2,
        Subscript::Colon { span: test_span() },
        integer_vector(&[7, 8]),
    );
    assert_eq!(
        fold(&exact).expect("a conformant `y[:]` folds"),
        Value::Array(vec![Value::Integer(7), Value::Integer(8)])
    );

    let scalar = slice_writer(
        2,
        Subscript::Colon { span: test_span() },
        integer_literal(5),
    );
    assert!(
        fold(&scalar).is_err(),
        "a scalar cannot be assigned to a whole dimension"
    );
}

/// MLS 3.6 §12.2 lets a declared extent name an input, and an extent this
/// evaluator cannot settle refuses the call — exactly as an unsettleable
/// binding does. Falling back to the declared `0` built an empty container that
/// `size()` and later loops read as the component's real extent.
#[test]
fn unsettleable_declared_extent_refuses_the_call() {
    let mut func = Function::new("test.unsettled", Span::DUMMY);
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
    let mut func = Function::new("test.negative", Span::DUMMY);
    func.add_input(integer_param("n"));
    func.add_output(real_vector_param("y", "n"));
    func.pure = true;
    func.body = vec![assign(component_reference("y"), real_literal(1.0))];

    let error = eval_function(
        &func,
        vec![Value::Integer(-2)],
        &EvalContext::new(),
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
    let mut func = Function::new("test.sortedBindings", Span::DUMMY);
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
    let mut func = Function::new("test.cyclicBindings", Span::DUMMY);
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
    let mut func = Function::new("test.realCoerce", Span::DUMMY);
    func.add_output(rumoca_core::FunctionParam::new(
        "y",
        "Real",
        effective_type,
        test_span(),
    ));
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
    let mut func = Function::new("test.fillTwo", Span::DUMMY);
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
        &EvalContext::new(),
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
    let mut func = Function::new("test.fillSlice", Span::DUMMY);
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
        &EvalContext::new(),
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
    let mut func = Function::new("test.scale", Span::DUMMY);
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
        &EvalContext::new(),
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
    let mut func = Function::new("test.opaque", Span::DUMMY);
    func.add_output(real_param("y"));
    let mut local = real_param("k");
    local.default = Some(var_ref("notInScope"));
    func.add_local(local);
    func.pure = true;
    func.body = vec![assign(component_reference("y"), real_literal(1.0))];

    let error = eval_function(
        &func,
        Vec::new(),
        &EvalContext::new(),
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
    let mut func = Function::new("Complex", Span::DUMMY);
    func.add_input(real_param("re"));
    func.add_input(real_param("im"));
    func.is_constructor = true;
    func.pure = true;

    let result = eval_function(
        &func,
        vec![Value::Real(1.5), Value::Real(-2.5)],
        &EvalContext::new(),
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

    let mut constructor = Function::new("Orientation", test_span());
    constructor.def_id = Some(orientation);
    constructor.is_constructor = true;
    constructor.pure = true;
    constructor.add_input(real_param("T").with_def_id(field_t));
    constructor.add_input(real_param("w").with_def_id(field_w));

    let mut axes = Function::new("axes", test_span());
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
            span: test_span(),
        },
        span: test_span(),
    });

    let mut ctx = EvalContext::new();
    ctx.add_function(constructor);
    ctx.add_function(axes);
    let output = Expression::FieldAccess {
        base: Box::new(Expression::FunctionCall {
            name: exact_reference("axes", axes_id),
            args: Vec::new(),
            is_constructor: false,
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
        super::super::eval_expr(&field, &ctx).expect("exact output projection folds"),
        Value::Real(1.5)
    );
}

/// MLS 3.6 §12.2: a record local's field is read through the joined
/// reference Flat renders, so `z.im` is the field `im` of the bound `z`.
///
/// The qualified-enumeration fallback used to claim the reference first, so
/// reading a record field folded to the enumeration value `z.im` — a wrong
/// value that no later stage could tell from a real enumeration literal.
/// The fallback still owns a reference whose head is *not* in scope.
#[test]
fn record_field_read_is_not_guessed_as_an_enumeration_literal() {
    let ctx = EvalContext::new();
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
        inputs: IndexMap::new(),
        outputs: IndexMap::new(),
        locals,
    };

    let field = eval_var_ref(&rumoca_core::Reference::new("z.im"), &[], &env, &eval)
        .expect("record field read");
    assert_eq!(field, Value::Real(4.5));

    let missing = eval_var_ref(&rumoca_core::Reference::new("z.absent"), &[], &env, &eval)
        .expect_err("a bound head settles the reference");
    assert_eq!(
        missing.runtime_dependent_reason(),
        Some(super::super::RuntimeDependentReason::UnimplementedForm),
        "a field this evaluator cannot follow refuses; it never invents a value: {missing}"
    );

    // A head that names nothing in scope is still read as a qualified
    // enumeration literal.
    let literal = eval_var_ref(
        &rumoca_core::Reference::new("Modelica.Types.Init.NoInit"),
        &[],
        &env,
        &eval,
    )
    .expect("qualified enum fallback");
    assert_eq!(
        literal,
        Value::Enum("Modelica.Types.Init".to_string(), "NoInit".to_string())
    );
}

#[test]
fn test_recursion_limit() {
    // Create a recursive function that will exceed the limit
    let mut func = Function::new("test.recurse", Span::DUMMY);
    func.add_input(integer_param("n"));
    func.add_output(integer_param("y"));
    func.pure = true;

    // Simple function that always returns 0 (to test limit checking)
    func.body = vec![rumoca_core::Statement::Assignment {
        comp: component_reference("y"),
        value: rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];

    let ctx = EvalContext::new();
    let limits = EvalLimits {
        recursion_depth: 5,
        max_iterations: 1000,
    };

    // This should succeed at depth 5
    let result = eval_function(
        &func,
        vec![Value::Integer(1)],
        &ctx,
        &limits,
        5,
        Span::DUMMY,
    );
    assert!(result.is_ok());

    // This should fail at depth 6
    let result = eval_function(
        &func,
        vec![Value::Integer(1)],
        &ctx,
        &limits,
        6,
        Span::DUMMY,
    );
    assert!(result.is_err());
}

#[test]
fn test_while_loop() {
    // Test: function countTo4() output Integer count; algorithm count := 0; while count < 4 loop count := count + 1; end while; end countTo4;
    let mut func = Function::new("test.countTo4", Span::DUMMY);
    func.add_output(integer_param("count"));
    func.pure = true;

    // count := 0
    let init_stmt = rumoca_core::Statement::Assignment {
        comp: component_reference("count"),
        value: rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    };

    // while count < 4 loop count := count + 1; end while
    let while_stmt = rumoca_core::Statement::While {
        block: rumoca_core::StatementBlock {
            cond: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Lt,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("count"),
                    subscripts: Vec::new(),
                    span: rumoca_core::Span::DUMMY,
                }),
                rhs: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(4),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            stmts: vec![rumoca_core::Statement::Assignment {
                comp: component_reference("count"),
                value: rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Add,
                    lhs: Box::new(rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::new("count"),
                        subscripts: Vec::new(),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    rhs: Box::new(rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    span: rumoca_core::Span::DUMMY,
                },
                span: rumoca_core::Span::DUMMY,
            }],
        },
        span: rumoca_core::Span::DUMMY,
    };

    func.body = vec![init_stmt, while_stmt];

    let ctx = EvalContext::new();
    let result =
        eval_function(&func, vec![], &ctx, &EvalLimits::default(), 0, Span::DUMMY).unwrap();

    assert_eq!(result.as_integer(), Some(4), "while loop should count to 4");
}
