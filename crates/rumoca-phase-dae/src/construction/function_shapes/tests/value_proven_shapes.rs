//! Discharges the acceptance contract on [`ShapeEnvironment`].
//!
//! MLS §12.2 gives a function's non-input array dimension sizes as expressions
//! over "inputs, constants, or parameter expressions", and MLS §4.4.2 restricts
//! every dimension to a scalar evaluable Integer/enumeration/Boolean
//! expression. These tests fix both sides of that rule: which value-dependent
//! dimensions the phase now proves, and which stay typed rejections.

use rumoca_core::{EffectiveType, FunctionParam, Literal, Reference, SourceMap, Subscript, TypeId};

use super::*;

fn real_type() -> TypeId {
    TypeId::new(1)
}

fn integer_type() -> TypeId {
    TypeId::new(2)
}

fn integer_literal(value: i64, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span,
    }
}

fn var_ref(name: &str, span: Span) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span,
    }
}

fn param(
    name: &str,
    type_name: &str,
    root: TypeId,
    dimensions: Vec<i64>,
    span: Span,
) -> FunctionParam {
    let value_type = EffectiveType::new(root, root, dimensions).expect("fixture type is resolved");
    FunctionParam::new(name, type_name, value_type, span)
}

/// `function f input Integer n; output Real y[n]; end f;`
fn value_shaped_function(span: Span) -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new("f", span);
    function.add_input(param("n", "Integer", integer_type(), Vec::new(), span));
    function.add_output(
        param("y", "Real", real_type(), vec![0], span)
            .with_shape_expr(vec![Subscript::expr(Box::new(var_ref("n", span)), span)]),
    );
    function
}

fn model_with_predefined_types() -> flat::Model {
    let mut model = flat::Model::new();
    model.predefined_types.real = real_type();
    model.predefined_types.integer = integer_type();
    model
}

fn call(argument: Expression, span: Span) -> flat::Equation {
    flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("f"),
            args: vec![argument],
            is_constructor: false,
            span,
        },
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    )
}

fn scalar_variable(name: &str, variability: Variability, span: Span) -> flat::Variable {
    let mut variable = flat::Variable::empty_with_span(span);
    variable.name = VarName::new(name);
    variable.variability = variability;
    variable
}

fn analyze(model: &flat::Model) -> Result<FunctionShapeAnalysis, ToDaeError> {
    FunctionShapeAnalysis::analyze(model, &EvalContext::new())
}

/// ACCEPTED: an Integer literal argument proves the result extent (MLS §12.2).
#[test]
fn integer_literal_argument_proves_a_value_dependent_result_extent() {
    let mut sources = SourceMap::new();
    let source = sources.add("literal_extent.mo", "f(3);");
    let span = Span::from_offsets(source, 0, 5);
    let mut model = model_with_predefined_types();
    model.add_function(value_shaped_function(span));
    model.add_equation(call(integer_literal(3, span), span));

    let analysis = analyze(&model).expect("a literal extent is an MLS §4.4.2 evaluable dimension");
    let [certificate] = analysis.certificates() else {
        panic!("one call has one specialization");
    };
    assert_eq!(certificate.results, vec![vec![3]]);
    assert_eq!(
        certificate.key.input_values,
        vec![Some(ProvenValue::Integer(3))]
    );
}

/// ACCEPTED: two distinct proven values own two distinct specializations, so
/// neither result is mis-shaped by the other's extent.
#[test]
fn distinct_proven_values_own_distinct_specializations() {
    let mut sources = SourceMap::new();
    let source = sources.add("two_extents.mo", "f(3); f(5);");
    let first = Span::from_offsets(source, 0, 5);
    let second = Span::from_offsets(source, 6, 11);
    let mut model = model_with_predefined_types();
    model.add_function(value_shaped_function(first));
    model.add_equation(call(integer_literal(3, first), first));
    model.add_equation(call(integer_literal(5, second), second));

    let analysis = analyze(&model).expect("both extents are evaluable");
    let certificates = analysis.certificates();
    assert_eq!(certificates.len(), 2);
    let mut results: Vec<_> = certificates
        .iter()
        .map(|certificate| certificate.results.clone())
        .collect();
    results.sort();
    assert_eq!(results, vec![vec![vec![3]], vec![vec![5]]]);
}

/// ACCEPTED: a model `parameter` whose binding the translation-time fixed point
/// settled is a parameter expression in the sense MLS §12.2 admits.
#[test]
fn settled_model_parameter_proves_a_value_dependent_result_extent() {
    let mut sources = SourceMap::new();
    let source = sources.add("parameter_extent.mo", "parameter Integer m = 3; f(m);");
    let span = Span::from_offsets(source, 0, 29);
    let mut model = model_with_predefined_types();
    model.add_function(value_shaped_function(span));
    let mut declaration = scalar_variable("m", Variability::Parameter(Default::default()), span);
    declaration.binding = Some(integer_literal(3, span));
    model.variables.insert(VarName::new("m"), declaration);
    model.add_equation(call(var_ref("m", span), span));

    let mut constants = EvalContext::new();
    constants.add_parameter("m", EvalValue::Integer(3));
    let analysis = FunctionShapeAnalysis::analyze(&model, &constants)
        .expect("a settled parameter is an MLS §4.5 evaluable value");
    let [certificate] = analysis.certificates() else {
        panic!("one call has one specialization");
    };
    assert_eq!(certificate.results, vec![vec![3]]);
}

/// ACCEPTED: exact Integer arithmetic over a proven value is still an extent.
#[test]
fn integer_arithmetic_over_a_proven_value_is_an_extent() {
    let mut sources = SourceMap::new();
    let source = sources.add("arithmetic_extent.mo", "f(2*3);");
    let span = Span::from_offsets(source, 0, 7);
    let mut model = model_with_predefined_types();
    model.add_function(value_shaped_function(span));
    model.add_equation(call(
        Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(integer_literal(2, span)),
            rhs: Box::new(integer_literal(3, span)),
            span,
        },
        span,
    ));

    let analysis = analyze(&model).expect("exact Integer arithmetic is evaluable");
    let [certificate] = analysis.certificates() else {
        panic!("one call has one specialization");
    };
    assert_eq!(certificate.results, vec![vec![6]]);
}

/// REJECTED: a continuous-time argument has no translation-time value, so the
/// dimension it would name stays the named `ED019` rejection rather than a
/// guessed extent.
#[test]
fn unsettled_argument_keeps_the_named_value_proof_rejection() {
    let mut sources = SourceMap::new();
    let source = sources.add("runtime_extent.mo", "Real u; f(u);");
    let span = Span::from_offsets(source, 0, 13);
    let mut model = model_with_predefined_types();
    model.add_function(value_shaped_function(span));
    model.variables.insert(
        VarName::new("u"),
        scalar_variable("u", Variability::Continuous(Default::default()), span),
    );
    model.add_equation(call(var_ref("u", span), span));

    let Err(error) = analyze(&model) else {
        panic!("an extent over a simulation-time value must not be accepted");
    };
    let ToDaeError::UnsupportedFlatSemantics {
        feature, detail, ..
    } = error
    else {
        panic!("the rejection must stay the function shape proof owner");
    };
    assert_eq!(feature, "function shape proof");
    assert!(
        detail.contains("extent depends on the value of scalar `n`"),
        "the rejection must name the scalar whose value is missing, got: {detail}"
    );
}

/// REJECTED: a formal parameter must not read an enclosing model coordinate's
/// value through a shadowed flat name. `n` is settled in the model scope and
/// the call argument is not evaluable, so the extent stays unproven.
#[test]
fn a_shadowing_formal_does_not_inherit_the_model_value() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "shadowed_extent.mo",
        "parameter Integer n = 7; Real u; f(u);",
    );
    let span = Span::from_offsets(source, 0, 37);
    let mut model = model_with_predefined_types();
    model.add_function(value_shaped_function(span));
    let mut settled = scalar_variable("n", Variability::Parameter(Default::default()), span);
    settled.binding = Some(integer_literal(7, span));
    model.variables.insert(VarName::new("n"), settled);
    model.variables.insert(
        VarName::new("u"),
        scalar_variable("u", Variability::Continuous(Default::default()), span),
    );
    model.add_equation(call(var_ref("u", span), span));

    let mut constants = EvalContext::new();
    constants.add_parameter("n", EvalValue::Integer(7));
    let Err(error) = FunctionShapeAnalysis::analyze(&model, &constants) else {
        panic!("the formal `n` must not fold to the model coordinate `n`");
    };
    let ToDaeError::UnsupportedFlatSemantics { detail, .. } = error else {
        panic!("the rejection must stay the function shape proof owner");
    };
    assert!(
        detail.contains("extent depends on the value of scalar `n`"),
        "shadowing must leave the formal unproven, got: {detail}"
    );
}

/// A `Real` value is never an extent: MLS §4.4.2 restricts a dimension to
/// Integer or enumeration/Boolean, so a whole-valued `Real` must not widen it.
#[test]
fn a_real_value_does_not_name_an_extent() {
    assert_eq!(ProvenValue::from_settled(&EvalValue::Real(3.0)), None);
    assert_eq!(ProvenValue::Boolean(true).extent(), None);
    assert_eq!(ProvenValue::Integer(3).extent(), Some(3));
}

/// `function g input Real x[:]; output Real y[2]; protected Integer m = size(x, 1);
///  Real phi[m]; end g;`
fn local_shaped_function(span: Span) -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new("g", span);
    function.add_input(
        param("x", "Real", real_type(), vec![0], span)
            .with_shape_expr(vec![Subscript::colon(span)]),
    );
    function.add_output(param("y", "Real", real_type(), vec![2], span));
    function.add_local(
        param("m", "Integer", integer_type(), Vec::new(), span).with_default(
            Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Size,
                args: vec![var_ref("x", span), integer_literal(1, span)],
                span,
            },
        ),
    );
    function.add_local(
        param("phi", "Real", real_type(), vec![0], span)
            .with_shape_expr(vec![Subscript::expr(Box::new(var_ref("m", span)), span)]),
    );
    function
}

fn simple_target(name: &str, span: Span) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference::construct(
        true,
        span,
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span,
            subs: Vec::new(),
            def_id: rumoca_core::DefId::new(1),
        }],
    )
    .expect("fixture assignment target is well formed")
}

fn array_argument(extent: usize, span: Span) -> Expression {
    Expression::Array {
        elements: (0..extent)
            .map(|ordinal| Expression::Literal {
                value: Literal::Real(ordinal as f64),
                span,
            })
            .collect(),
        is_matrix: false,
        span,
    }
}

/// ACCEPTED: MLS §12.4.4 makes a protected declaration equation the value that
/// holds on entry, so a later local's extent may be written over it.
#[test]
fn an_unassigned_local_declaration_value_proves_a_later_local_extent() {
    let mut sources = SourceMap::new();
    let source = sources.add("local_extent.mo", "g({1.0,2.0,3.0});");
    let span = Span::from_offsets(source, 0, 16);
    let mut model = model_with_predefined_types();
    model.add_function(local_shaped_function(span));
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("g"),
            args: vec![array_argument(3, span)],
            is_constructor: false,
            span,
        },
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let analysis = analyze(&model).expect("`size(x, 1)` settles `m` on entry");
    let [certificate] = analysis.certificates() else {
        panic!("one call has one specialization");
    };
    assert_eq!(certificate.values[&VarName::new("phi")], vec![3]);
}

/// REJECTED: a local the body assigns has no single entry value, so its
/// declaration equation must not name a later extent.
#[test]
fn an_assigned_local_does_not_prove_a_later_extent() {
    let mut sources = SourceMap::new();
    let source = sources.add("assigned_local.mo", "g({1.0,2.0,3.0});");
    let span = Span::from_offsets(source, 0, 16);
    let mut model = model_with_predefined_types();
    let mut function = local_shaped_function(span);
    function.body.push(rumoca_core::Statement::Assignment {
        comp: simple_target("m", span),
        value: integer_literal(1, span),
        span,
    });
    model.add_function(function);
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("g"),
            args: vec![array_argument(3, span)],
            is_constructor: false,
            span,
        },
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let Err(error) = analyze(&model) else {
        panic!("a reassigned local must not settle a later extent");
    };
    let ToDaeError::UnsupportedFlatSemantics { detail, .. } = error else {
        panic!("the rejection must stay the function shape proof owner");
    };
    assert!(
        detail.contains("extent depends on the value of scalar `m`"),
        "a reassigned local must leave the extent unproven, got: {detail}"
    );
}

/// REJECTED: value recursion without a translation-time fixed point is bounded
/// and reported, never left to exhaust the process stack.
#[test]
fn value_recursion_without_a_fixed_point_is_bounded() {
    let mut sources = SourceMap::new();
    let source = sources.add("unbounded.mo", "f(1);");
    let span = Span::from_offsets(source, 0, 5);
    let mut model = model_with_predefined_types();
    let mut function = value_shaped_function(span);
    // `y := f(n + 1)` — every activation proves a strictly larger argument.
    function.body.push(rumoca_core::Statement::Assignment {
        comp: simple_target("y", span),
        value: Expression::FunctionCall {
            name: Reference::new("f"),
            args: vec![Expression::Binary {
                op: rumoca_core::OpBinary::Add,
                lhs: Box::new(var_ref("n", span)),
                rhs: Box::new(integer_literal(1, span)),
                span,
            }],
            is_constructor: false,
            span,
        },
        span,
    });
    model.add_function(function);
    model.add_equation(call(integer_literal(1, span), span));

    let Err(error) = analyze(&model) else {
        panic!("unbounded value recursion must be reported");
    };
    let ToDaeError::UnsupportedFlatSemantics {
        feature, detail, ..
    } = error
    else {
        panic!("the rejection must be a typed unsupported-semantics report");
    };
    assert_eq!(feature, "function shape specialization");
    assert!(
        detail.contains("nested value-proven specializations"),
        "the rejection must state the bound it exceeded, got: {detail}"
    );
    assert!(
        !detail.contains("no translation-time fixed point"),
        "the report must not claim a proof this analysis does not have: {detail}"
    );
}

/// `function f input Integer n; output Real y; algorithm y := 1 + f(n - 1);`
///
/// The result is a scalar, so no declared dimension reads `n`.
fn scalar_recursive_function(span: Span) -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new("f", span);
    function.add_input(param("n", "Integer", integer_type(), Vec::new(), span));
    function.add_output(param("y", "Real", real_type(), Vec::new(), span));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: simple_target("y", span),
        value: Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(Expression::Literal {
                value: Literal::Real(1.0),
                span,
            }),
            rhs: Box::new(Expression::FunctionCall {
                name: Reference::new("f"),
                args: vec![Expression::Binary {
                    op: rumoca_core::OpBinary::Sub,
                    lhs: Box::new(var_ref("n", span)),
                    rhs: Box::new(integer_literal(1, span)),
                    span,
                }],
                is_constructor: false,
                span,
            }),
            span,
        },
        span,
    });
    function
}

/// ACCEPTED: a value no declared dimension reads stays out of the key, so a
/// recursive call repeats its key and the analysis terminates on the memo.
///
/// This is the property the depth bound must never be asked to supply: nothing
/// about `f(n) = 1 + f(n - 1)` is value-dependent, so it must keep the single
/// specialization it had before values entered the key at all.
#[test]
fn scalar_valued_recursion_reuses_one_specialization() {
    let mut sources = SourceMap::new();
    let source = sources.add("scalar_recursion.mo", "f(3);");
    let span = Span::from_offsets(source, 0, 5);
    let mut model = model_with_predefined_types();
    model.add_function(scalar_recursive_function(span));
    model.add_equation(call(integer_literal(3, span), span));

    let analysis =
        analyze(&model).expect("a recursion whose shapes read no value keeps one specialization");
    assert_eq!(analysis.certificates().len(), 1);
    assert_eq!(analysis.certificates()[0].key.input_values, vec![None]);
}

/// ACCEPTED: a value-keyed recursion whose argument converges reaches its base
/// value and stops, well inside the bound.
#[test]
fn converging_value_keyed_recursion_terminates() {
    let mut sources = SourceMap::new();
    let source = sources.add("converging.mo", "q(8);");
    let span = Span::from_offsets(source, 0, 5);
    let mut model = model_with_predefined_types();
    // `function q input Integer m; output Real y[m]; algorithm y[1] := q(integer(m/2))[1];`
    let mut function = rumoca_core::Function::new("q", span);
    function.add_input(param("m", "Integer", integer_type(), Vec::new(), span));
    function.add_output(
        param("y", "Real", real_type(), vec![0], span)
            .with_shape_expr(vec![Subscript::expr(Box::new(var_ref("m", span)), span)]),
    );
    let halved = Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Integer,
        args: vec![Expression::Binary {
            op: rumoca_core::OpBinary::Div,
            lhs: Box::new(var_ref("m", span)),
            rhs: Box::new(integer_literal(2, span)),
            span,
        }],
        span,
    };
    function.body.push(rumoca_core::Statement::Assignment {
        comp: simple_target("y", span),
        value: Expression::FunctionCall {
            name: Reference::new("q"),
            args: vec![halved],
            is_constructor: false,
            span,
        },
        span,
    });
    model.add_function(function);
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("q"),
            args: vec![integer_literal(8, span)],
            is_constructor: false,
            span,
        },
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let analysis = analyze(&model).expect("a halving argument reaches its base value");
    // 8, 4, 2, 1, 0 — the chain stops when `integer(0/2)` repeats the key.
    let mut extents: Vec<_> = analysis
        .certificates()
        .iter()
        .map(|certificate| certificate.results.clone())
        .collect();
    extents.sort();
    assert_eq!(
        extents,
        vec![
            vec![vec![0]],
            vec![vec![1]],
            vec![vec![2]],
            vec![vec![4]],
            vec![vec![8]]
        ]
    );
}

/// A value no dimension reads must not split specializations: ten calls that
/// differ only in such a value are one DAE function, not ten identical ones.
#[test]
fn a_value_no_dimension_reads_does_not_split_specializations() {
    let mut sources = SourceMap::new();
    let source = sources.add("unsplit.mo", "g(1,u); g(2,u);");
    let span = Span::from_offsets(source, 0, 15);
    let mut model = model_with_predefined_types();
    // `function g input Integer n; input Real u; output Real y; algorithm y := u;`
    let mut function = rumoca_core::Function::new("g", span);
    function.add_input(param("n", "Integer", integer_type(), Vec::new(), span));
    function.add_input(param("u", "Real", real_type(), Vec::new(), span));
    function.add_output(param("y", "Real", real_type(), Vec::new(), span));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: simple_target("y", span),
        value: var_ref("u", span),
        span,
    });
    model.add_function(function);
    model.variables.insert(
        VarName::new("u"),
        scalar_variable("u", Variability::Continuous(Default::default()), span),
    );
    for ordinal in 1..=10 {
        model.add_equation(flat::Equation::new(
            Expression::FunctionCall {
                name: Reference::new("g"),
                args: vec![integer_literal(ordinal, span), var_ref("u", span)],
                is_constructor: false,
                span,
            },
            span,
            flat::EquationOrigin::ComponentEquation {
                component: String::new(),
            },
        ));
    }

    let analysis = analyze(&model).expect("no declared dimension reads `n`");
    assert_eq!(analysis.certificates().len(), 1);
}
