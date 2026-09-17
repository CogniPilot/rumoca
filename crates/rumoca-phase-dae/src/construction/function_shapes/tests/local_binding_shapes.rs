//! Sizing of colon-declared locals and outputs from their bindings.
//!
//! MLS §12.4.5 gives a local or output array dimension declared with `:` the
//! size of the array bound or assigned to it, so these fixtures fix which of
//! those bindings the phase now proves a size from, and that a colon axis with
//! neither a binding nor an assignment stays a typed rejection. The reduced
//! reproduction mirrors `Modelica.Mechanics.MultiBody.Visualizers.Colors`
//! `.ColorMaps.jet`, whose four protected colon locals are sized by a
//! comprehension, an ascending Real range, a descending Real range, and a
//! comprehension over an Integer local derived from Real intermediates.

use rumoca_core::{
    ComprehensionIndex, EffectiveType, FunctionParam, Literal, OpBinary, Reference, SourceMap,
    Subscript, TypeId,
};

use super::*;

fn real_type() -> TypeId {
    TypeId::new(1)
}

fn integer_type() -> TypeId {
    TypeId::new(2)
}

fn model_with_predefined_types() -> flat::Model {
    let mut model = flat::Model::new();
    model.predefined_types.real = real_type();
    model.predefined_types.integer = integer_type();
    model
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

fn var_ref(name: &str, span: Span) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span,
    }
}

fn int_lit(value: i64, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span,
    }
}

fn real_lit(value: f64, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span,
    }
}

fn builtin(function: BuiltinFunction, args: Vec<Expression>, span: Span) -> Expression {
    Expression::BuiltinCall {
        function,
        args,
        span,
    }
}

fn binary(op: OpBinary, lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

/// `{element for name in 1:count}`.
fn comprehension(name: &str, count: Expression, element: Expression, span: Span) -> Expression {
    Expression::ArrayComprehension {
        expr: Box::new(element),
        indices: vec![ComprehensionIndex {
            name: name.to_string(),
            range: Expression::Range {
                start: Box::new(int_lit(1, span)),
                step: None,
                end: Box::new(count),
                span,
            },
        }],
        filter: None,
        span,
    }
}

fn range(start: Expression, step: Expression, end: Expression, span: Span) -> Expression {
    Expression::Range {
        start: Box::new(start),
        step: Some(Box::new(step)),
        end: Box::new(end),
        span,
    }
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

fn analyze(model: &flat::Model) -> Result<FunctionShapeAnalysis, ToDaeError> {
    FunctionShapeAnalysis::analyze(model, &EvalContext::new())
}

fn call(name: &str, args: Vec<Expression>, span: Span) -> flat::Equation {
    flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new(name),
            args,
            is_constructor: false,
            span,
        },
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    )
}

/// `Real name[:]` sized by `binding`.
fn colon_local(name: &str, binding: Expression, span: Span) -> FunctionParam {
    param(name, "Real", real_type(), vec![0], span)
        .with_shape_expr(vec![Subscript::colon(span)])
        .with_default(binding)
}

/// The reduced reproduction of the Surfaces ToDae rejection.
///
/// ```modelica
/// function colonLocal
///   input Integer n = 8;
///   output Real s;
/// protected
///   Integer b = integer(n/2);
///   Real v1[:] = {1.0*i for i in 1:b};
/// algorithm
///   s := sum(v1);
/// end colonLocal;
/// ```
fn colon_local_function(span: Span) -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new("colonLocal", span);
    function.add_input(param("n", "Integer", integer_type(), Vec::new(), span));
    function.add_output(param("s", "Real", real_type(), Vec::new(), span));
    function.add_local(
        param("b", "Integer", integer_type(), Vec::new(), span).with_default(builtin(
            BuiltinFunction::Integer,
            vec![binary(
                OpBinary::Div,
                var_ref("n", span),
                int_lit(2, span),
                span,
            )],
            span,
        )),
    );
    function.add_local(colon_local(
        "v1",
        comprehension(
            "i",
            var_ref("b", span),
            binary(OpBinary::Mul, real_lit(1.0, span), var_ref("i", span), span),
            span,
        ),
        span,
    ));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: simple_target("s", span),
        value: builtin(BuiltinFunction::Sum, vec![var_ref("v1", span)], span),
        span,
    });
    function
}

/// `integer(round(a/2))`, the b/c form (`round` is `ceil` or `floor`).
fn half_of(name: &str, round: BuiltinFunction, span: Span) -> FunctionParam {
    param(name, "Integer", integer_type(), Vec::new(), span).with_default(builtin(
        BuiltinFunction::Integer,
        vec![builtin(
            round,
            vec![binary(
                OpBinary::Div,
                var_ref("a", span),
                int_lit(2, span),
                span,
            )],
            span,
        )],
        span,
    ))
}

/// The `(count - i)*d` factor shared by the jet comprehension elements.
fn scaled_offset(count: &str, span: Span) -> Expression {
    binary(
        OpBinary::Mul,
        binary(
            OpBinary::Sub,
            var_ref(count, span),
            var_ref("i", span),
            span,
        ),
        var_ref("d", span),
        span,
    )
}

fn jet_scalar_locals(span: Span) -> [FunctionParam; 4] {
    let a = param("a", "Real", real_type(), Vec::new(), span).with_default(builtin(
        BuiltinFunction::Ceil,
        vec![binary(
            OpBinary::Div,
            var_ref("n_colors", span),
            int_lit(4, span),
            span,
        )],
        span,
    ));
    let d = param("d", "Real", real_type(), Vec::new(), span).with_default(binary(
        OpBinary::Div,
        int_lit(1, span),
        var_ref("a", span),
        span,
    ));
    [
        a,
        d,
        half_of("b", BuiltinFunction::Ceil, span),
        half_of("c", BuiltinFunction::Floor, span),
    ]
}

fn jet_colon_locals(span: Span) -> [FunctionParam; 4] {
    let v1 = colon_local(
        "v1",
        comprehension(
            "i",
            var_ref("b", span),
            binary(
                OpBinary::Sub,
                int_lit(1, span),
                scaled_offset("b", span),
                span,
            ),
            span,
        ),
        span,
    );
    let v2 = colon_local(
        "v2",
        range(
            binary(OpBinary::Add, int_lit(0, span), var_ref("d", span), span),
            var_ref("d", span),
            int_lit(1, span),
            span,
        ),
        span,
    );
    let v3 = colon_local(
        "v3",
        range(
            binary(OpBinary::Sub, int_lit(1, span), var_ref("d", span), span),
            Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs: Box::new(var_ref("d", span)),
                span,
            },
            int_lit(0, span),
            span,
        ),
        span,
    );
    let v4 = colon_local(
        "v4",
        comprehension(
            "i",
            var_ref("c", span),
            binary(
                OpBinary::Add,
                real_lit(0.5, span),
                scaled_offset("c", span),
                span,
            ),
            span,
        ),
        span,
    );
    [v1, v2, v3, v4]
}

/// The four jet color-map colon locals in their declaration order.
///
/// ```modelica
/// function jet
///   input Integer n_colors = 64;
///   output Real colorMap[n_colors, 3];
/// protected
///   Real    a = ceil(n_colors/4);
///   Real    d = 1/a;
///   Integer b = integer(ceil(a/2));
///   Integer c = integer(floor(a/2));
///   Real    v1[:] = {1 - (b - i)*d for i in 1:b};
///   Real    v2[:] = 0 + d:d:1;
///   Real    v3[:] = 1 - d:-d:0;
///   Real    v4[:] = {0.5 + (c - i)*d for i in 1:c};
/// end jet;
/// ```
fn jet_style_function(span: Span) -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new("jet", span);
    function.add_input(param(
        "n_colors",
        "Integer",
        integer_type(),
        Vec::new(),
        span,
    ));
    function.add_output(
        param("colorMap", "Real", real_type(), vec![0, 3], span).with_shape_expr(vec![
            Subscript::expr(Box::new(var_ref("n_colors", span)), span),
            Subscript::index(3, span),
        ]),
    );
    for local in jet_scalar_locals(span) {
        function.add_local(local);
    }
    for local in jet_colon_locals(span) {
        function.add_local(local);
    }
    function
}

fn local_shape<'a>(certificate: &'a FunctionShapeCertificate, name: &str) -> &'a ValueShape {
    certificate
        .values
        .get(&VarName::new(name))
        .unwrap_or_else(|| panic!("`{name}` has a proven shape"))
}

/// ACCEPTED: `colonLocal(8)` sizes `v1[:]` from its comprehension binding
/// `{1.0*i for i in 1:b}` with `b = integer(8/2) = 4`, so the local proves
/// shape `[4]`. OMC simulates the same source with `s = sum({1,2,3,4}) = 10.0`.
#[test]
fn colon_local_is_sized_from_its_comprehension_binding() {
    let mut sources = SourceMap::new();
    let source = sources.add("colon_local.mo", "colonLocal(8);");
    let span = Span::from_offsets(source, 0, 14);
    let mut model = model_with_predefined_types();
    model.add_function(colon_local_function(span));
    model.add_equation(call("colonLocal", vec![int_lit(8, span)], span));

    let analysis = analyze(&model)
        .expect("a colon local sized by its comprehension binding is an MLS §12.4.5 dimension");
    let [certificate] = analysis.certificates() else {
        panic!("one call has one specialization");
    };
    assert_eq!(
        certificate.key.input_values,
        vec![Some(ProvenValue::Integer(8))]
    );
    assert_eq!(local_shape(certificate, "v1"), &vec![4]);
    // The scalar output keeps the empty shape it is declared with.
    assert_eq!(certificate.results, vec![Vec::<u32>::new()]);
}

/// ACCEPTED: the four jet colon locals for `jet(64)` each prove their expected
/// length. With `a = 16`, `d = 1/16`, `b = c = 8`: the comprehensions `v1` and
/// `v4` are `[8]`, and the Real ranges `0+d:d:1` and `1-d:-d:0` are `[16]`.
/// OMC accepts and simulates `jet` at its default `n_colors = 64`.
#[test]
fn jet_style_colon_locals_prove_their_lengths() {
    let mut sources = SourceMap::new();
    let source = sources.add("jet_style.mo", "jet(64);");
    let span = Span::from_offsets(source, 0, 8);
    let mut model = model_with_predefined_types();
    model.add_function(jet_style_function(span));
    model.add_equation(call("jet", vec![int_lit(64, span)], span));

    let analysis =
        analyze(&model).expect("the jet colon locals are sized by their bindings (MLS §12.4.5)");
    let [certificate] = analysis.certificates() else {
        panic!("one call has one specialization");
    };
    assert_eq!(
        certificate.key.input_values,
        vec![Some(ProvenValue::Integer(64))]
    );
    // Comprehension over the Integer local b = integer(ceil(a/2)) = 8.
    assert_eq!(local_shape(certificate, "v1"), &vec![8]);
    // Ascending Real range 0.0625:0.0625:1 has 16 elements.
    assert_eq!(local_shape(certificate, "v2"), &vec![16]);
    // Descending Real range 0.9375:-0.0625:0 has 16 elements.
    assert_eq!(local_shape(certificate, "v3"), &vec![16]);
    // Comprehension over the Integer local c = integer(floor(a/2)) = 8.
    assert_eq!(local_shape(certificate, "v4"), &vec![8]);
    // The output extent reads the keyed input value directly.
    assert_eq!(certificate.results, vec![vec![64, 3]]);
}

/// REJECTED: a colon local with neither a binding nor a body assignment has no
/// MLS §12.4.5 size, so it stays the named `ED019` rejection rather than being
/// defaulted to any extent.
#[test]
fn colon_local_without_a_size_stays_a_typed_rejection() {
    let mut sources = SourceMap::new();
    let source = sources.add("unsized_colon.mo", "unsized();");
    let span = Span::from_offsets(source, 0, 10);
    let mut function = rumoca_core::Function::new("unsized", span);
    function.add_output(param("s", "Real", real_type(), Vec::new(), span));
    function.add_local(
        param("v", "Real", real_type(), vec![0], span)
            .with_shape_expr(vec![Subscript::colon(span)]),
    );
    let mut model = model_with_predefined_types();
    model.add_function(function);
    model.add_equation(call("unsized", Vec::new(), span));

    let Err(error) = analyze(&model) else {
        panic!("an unsized colon local must stay a typed rejection");
    };
    assert!(
        matches!(
            &error,
            ToDaeError::UnsupportedFlatSemantics { feature, detail, .. }
                if feature == "function shape proof"
                    && detail
                        == "`v`: axis 1 is variable-size and neither a binding nor an assignment \
                            sizes it"
        ),
        "unexpected error: {error:?}"
    );
}

fn assign(name: &str, value: Expression, span: Span) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: simple_target(name, span),
        value,
        span,
    }
}

/// `Real v[:]` written by two whole-array assignments of `first` and `second`
/// elements, then read into a scalar output.
fn resized_local_function(
    name: &str,
    first: usize,
    second: usize,
    span: Span,
) -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new(name, span);
    function.add_output(param("s", "Real", real_type(), Vec::new(), span));
    function.add_local(
        param("v", "Real", real_type(), vec![0], span)
            .with_shape_expr(vec![Subscript::colon(span)]),
    );
    function.body.push(assign("v", array(first, span), span));
    function.body.push(assign("v", array(second, span), span));
    function.body.push(assign(
        "s",
        builtin(BuiltinFunction::Sum, vec![var_ref("v", span)], span),
        span,
    ));
    function
}

/// ACCEPTED: MLS §12.4.5 sizes a colon local from the array assigned to it and
/// allows more than one assignment; when every whole-array assignment proves
/// the same shape, that shape sizes the local. Two `Real[3]` assignments prove
/// `v[:] = [3]`.
#[test]
fn colon_local_sized_from_agreeing_body_assignments() {
    let mut sources = SourceMap::new();
    let source = sources.add("agree_assign.mo", "twoAssignAgree();");
    let span = Span::from_offsets(source, 0, 17);
    let mut model = model_with_predefined_types();
    model.add_function(resized_local_function("twoAssignAgree", 3, 3, span));
    model.add_equation(call("twoAssignAgree", Vec::new(), span));

    let analysis = analyze(&model)
        .expect("agreeing whole-array assignments size the colon local (MLS §12.4.5)");
    let [certificate] = analysis.certificates() else {
        panic!("one call has one specialization");
    };
    assert_eq!(local_shape(certificate, "v"), &vec![3]);
}

/// REJECTED: two whole-array assignments of provably different shapes are a
/// runtime resize of a variable-size local, which the canonical single-extent
/// DAE form does not represent. It is rejected by name with both shapes rather
/// than silently sized from either.
#[test]
fn colon_local_resized_by_differing_assignments_is_rejected() {
    let mut sources = SourceMap::new();
    let source = sources.add("differ_assign.mo", "twoAssignDiffer();");
    let span = Span::from_offsets(source, 0, 18);
    let mut model = model_with_predefined_types();
    model.add_function(resized_local_function("twoAssignDiffer", 3, 2, span));
    model.add_equation(call("twoAssignDiffer", Vec::new(), span));

    let Err(error) = analyze(&model) else {
        panic!("a resized colon local must stay a typed rejection");
    };
    assert!(
        matches!(
            &error,
            ToDaeError::UnsupportedFlatSemantics { feature, detail, .. }
                if feature == "function shape proof"
                    && detail
                        == "`v`: is assigned arrays of differing shapes [3] and [2]; resizing a \
                            variable-size local is unsupported"
        ),
        "unexpected error: {error:?}"
    );
}
