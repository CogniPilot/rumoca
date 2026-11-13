//! Record outputs assigned field by field against their declared layout.

use super::*;

/// `head.field` as an assignment target: one exact part per component.
fn field_target(record: &str, field: &str) -> ComponentReference {
    let part = |ident: &str, id: u32| rumoca_core::ComponentRefPart {
        ident: ident.to_string(),
        span: test_span(),
        subs: Vec::new(),
        def_id: rumoca_core::DefId::new(id),
    };
    ComponentReference::construct(false, test_span(), vec![part(record, 31), part(field, 32)])
        .expect("fixture field target is exact")
}

/// The record constructor of `test.R` (MLS 3.6 §12.6): its inputs are the
/// record's declared fields, in declaration order.
fn record_constructor(fields: Vec<rumoca_core::FunctionParam>) -> Function {
    let mut constructor = Function::new("test.R", Span::DUMMY);
    constructor.is_constructor = true;
    constructor.pure = true;
    for field in fields {
        constructor.add_input(field);
    }
    constructor
}

fn record_param(name: &str) -> rumoca_core::FunctionParam {
    function_param(name, "test.R", rumoca_core::TypeId::new(20))
        .with_type_class(rumoca_core::ClassType::Record)
}

fn record_fold(
    func: &Function,
    fields: Vec<rumoca_core::FunctionParam>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    let mut ctx = super::declared_types::typed_context();
    ctx.add_function(record_constructor(fields));
    eval_function(func, args, &ctx, &EvalLimits::default(), 0, Span::DUMMY)
}

/// `function f input Real total; output test.R r; algorithm <body>; end f;`
fn record_writer(body: Vec<Statement>) -> Function {
    let mut func = Function::new("test.split", Span::DUMMY);
    func.add_input(real_param("total"));
    func.add_output(record_param("r"));
    func.pure = true;
    func.body = body;
    func
}

/// MLS 3.6 §11.2.1 and §12.2: a record output is assigned field by field, and
/// a later statement reads the field it wrote (the MSL-style
/// `head.m := total - jaw; head.c := ... / head.m` of a mass split). The
/// returned record lists its fields in declaration order, whatever order the
/// statements wrote them in.
#[test]
fn record_output_is_assigned_field_by_field() {
    let func = record_writer(vec![
        assign(
            field_target("r", "m"),
            binary(
                rumoca_core::OpBinary::Sub,
                var_ref("total"),
                real_literal(0.5),
            ),
        ),
        assign(
            field_target("r", "c"),
            binary(
                rumoca_core::OpBinary::Mul,
                var_ref("r.m"),
                real_literal(2.0),
            ),
        ),
    ]);
    let result = record_fold(
        &func,
        vec![real_param("c"), real_param("m")],
        vec![Value::Real(2.0)],
    )
    .expect("a record output assigned field by field folds");
    let fields = result.as_record().expect("record value");
    assert_eq!(
        fields.iter().collect::<Vec<_>>(),
        [
            (&"c".to_string(), &Value::Real(3.0)),
            (&"m".to_string(), &Value::Real(1.5)),
        ]
    );
}

/// MLS 3.6 §10.6.13: an Integer written into a Real field is converted when it
/// is written, so a later read of the field computes in Real arithmetic:
/// `r.a := n; r.b := r.a - m` with n = 2^53 + 1, m = 2^53 gives b = 0, the
/// Real difference, not the Integer difference 1.
#[test]
fn record_field_write_converts_to_the_declared_field_type() {
    let mut func = Function::new("test.convert", Span::DUMMY);
    func.add_input(integer_param("n"));
    func.add_input(integer_param("m"));
    func.add_output(record_param("r"));
    func.pure = true;
    func.body = vec![
        assign(field_target("r", "a"), var_ref("n")),
        assign(
            field_target("r", "b"),
            binary(rumoca_core::OpBinary::Sub, var_ref("r.a"), var_ref("m")),
        ),
    ];
    let result = record_fold(
        &func,
        vec![real_param("a"), real_param("b")],
        vec![
            Value::Integer(9_007_199_254_740_993),
            Value::Integer(9_007_199_254_740_992),
        ],
    )
    .expect("the record folds");
    let fields = result.as_record().expect("record value");
    assert_eq!(fields.get("a"), Some(&Value::Real(9_007_199_254_740_992.0)));
    assert_eq!(fields.get("b"), Some(&Value::Real(0.0)));
}

/// A record is a value only once every declared field has one (MLS 3.6
/// §12.4.4): a partial record, a field left to a declared default this
/// interpreter does not evaluate, a nested field whose enclosing record was
/// never built, and a record with no declared field layout in scope are all
/// refused rather than returned as a complete value.
#[test]
fn incomplete_or_unproven_records_are_refused() {
    let partial = record_writer(vec![assign(field_target("r", "m"), var_ref("total"))]);
    assert!(
        record_fold(
            &partial,
            vec![real_param("m"), real_param("c")],
            vec![Value::Real(1.0)],
        )
        .is_err()
    );

    let mut defaulted = real_param("c");
    defaulted.default = Some(real_literal(1.0));
    assert!(
        record_fold(
            &partial,
            vec![real_param("m"), defaulted],
            vec![Value::Real(1.0)],
        )
        .is_err()
    );

    let nested_target = ComponentReference::construct(
        false,
        test_span(),
        ["r", "inner", "x"]
            .iter()
            .zip(41..)
            .map(|(ident, id)| rumoca_core::ComponentRefPart {
                ident: (*ident).to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id: rumoca_core::DefId::new(id),
            })
            .collect(),
    )
    .expect("fixture nested target is exact");
    let nested = record_writer(vec![assign(nested_target, var_ref("total"))]);
    assert!(
        record_fold(
            &nested,
            vec![
                function_param("inner", "test.Inner", rumoca_core::TypeId::new(21))
                    .with_type_class(rumoca_core::ClassType::Record)
            ],
            vec![Value::Real(1.0)],
        )
        .is_err()
    );

    let complete = record_writer(vec![assign(field_target("r", "m"), var_ref("total"))]);
    let no_layout = eval_function(
        &complete,
        vec![Value::Real(1.0)],
        &EvalContext::new(),
        &EvalLimits::default(),
        0,
        Span::DUMMY,
    );
    assert!(
        no_layout.is_err(),
        "no constructor, no field layout: {no_layout:?}"
    );

    let untouched = record_writer(Vec::new());
    assert!(record_fold(&untouched, vec![real_param("m")], vec![Value::Real(1.0)]).is_err());
}

/// A field read before any statement wrote it has no value: the record
/// output is not given an invented default.
#[test]
fn unassigned_record_output_field_is_not_invented() {
    let mut func = record_writer(Vec::new());
    func.add_output(real_param("y"));
    func.body = vec![assign(component_reference("y"), var_ref("r.m"))];
    assert!(record_fold(&func, vec![real_param("m")], vec![Value::Real(1.0)]).is_err());
}

/// `test.R(a, b)`, the record constructor called as a declaration binding.
fn constructed(a: f64, b: f64) -> Expression {
    Expression::FunctionCall {
        name: rumoca_core::Reference::new("test.R"),
        args: vec![real_literal(a), real_literal(b)],
        is_constructor: true,
        span: Span::DUMMY,
    }
}

/// A record declared with a binding keeps the binding's fields and takes a
/// later field write (`output R r = R(1, 2); algorithm r.a := 3;`); an Integer
/// written to its Real field reads back as a Real.
#[test]
fn an_initialised_record_output_takes_a_field_update() {
    let mut output = record_param("r");
    output.default = Some(constructed(1.0, 2.0));
    let mut func = Function::new("test.update", Span::DUMMY);
    func.add_input(real_param("total"));
    func.add_output(output);
    func.pure = true;
    func.body = vec![assign(field_target("r", "a"), integer_literal(3))];
    let result = record_fold(
        &func,
        vec![real_param("a"), real_param("b")],
        vec![Value::Real(1.0)],
    )
    .expect("the field update folds");
    let fields = result.as_record().expect("record value");
    assert_eq!(fields.get("a"), Some(&Value::Real(3.0)));
    assert_eq!(fields.get("b"), Some(&Value::Real(2.0)));
}

/// The same for a protected local record read back through a Real output.
#[test]
fn an_initialised_record_local_takes_a_field_update() {
    let mut local = record_param("q");
    local.default = Some(constructed(1.0, 2.0));
    let mut func = Function::new("test.localUpdate", Span::DUMMY);
    func.add_input(real_param("total"));
    func.add_local(local);
    func.add_output(real_param("y"));
    func.pure = true;
    func.body = vec![
        assign(field_target("q", "b"), integer_literal(5)),
        assign(
            component_reference("y"),
            binary(rumoca_core::OpBinary::Add, var_ref("q.a"), var_ref("q.b")),
        ),
    ];
    assert_eq!(
        record_fold(
            &func,
            vec![real_param("a"), real_param("b")],
            vec![Value::Real(1.0)],
        )
        .expect("the local field update folds"),
        Value::Real(6.0)
    );
}
