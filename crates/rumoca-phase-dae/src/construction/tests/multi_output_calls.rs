//! MLS §11.2.1.1 multi-result call statements, and the MLS §10.4.2.1 `[ ]`
//! shape rule the same call sites depend on.
//!
//! Every case goes through the public `construct` entry so it asserts the
//! constructed DAE (or the exact rejection), not an internal planner state.

use super::super::*;
use super::support::*;
use rumoca_core::Reference;

/// The constructed function of that name, by walking the DAE's function ids.
fn constructed_function<'dae>(view: dae::DaeView<'dae>, name: &str) -> dae::FunctionView<'dae> {
    (0..)
        .map_while(|index| view.function_id(index))
        .filter_map(|id| view.function(id))
        .find(|function| function.name().as_str() == name)
        .unwrap_or_else(|| panic!("`{name}` reaches the DAE"))
}

/// `function two input Real x; output Real a; output Real b; ... end two;`
///
/// `a := x`, `b := x` — two whole scalar results, which is the smallest body
/// that gives a multi-result call statement something to assign.
fn two_result_function(source: &TestSource) -> rumoca_core::Function {
    let span = source.span("function two", 0);
    let mut function = rumoca_core::Function::new("two", span);
    function.add_input(real_function_param(
        "x",
        Vec::new(),
        source.span("input Real x", 0),
    ));
    function.add_output(real_function_param(
        "a",
        Vec::new(),
        source.span("output Real a", 0),
    ));
    function.add_output(real_function_param(
        "b",
        Vec::new(),
        source.span("output Real b", 0),
    ));
    let first = source.span("a := x", 0);
    let second = source.span("b := x", 0);
    function.body = vec![
        rumoca_core::Statement::Assignment {
            comp: test_component_reference("a", first),
            value: Expression::VarRef {
                name: Reference::new("x"),
                subscripts: Vec::new(),
                span: source.span("x", 1),
            },
            span: first,
        },
        rumoca_core::Statement::Assignment {
            comp: test_component_reference("b", second),
            value: Expression::VarRef {
                name: Reference::new("x"),
                subscripts: Vec::new(),
                span: source.span("x", 2),
            },
            span: second,
        },
    ];
    function
}

/// The same two-result callee, but as an MLS §12.9 external body that declared
/// no purity — the form MLS 3.7 §12.3 treats as impure and WR001 reports.
fn external_two_result_function(source: &TestSource) -> rumoca_core::Function {
    let mut function = two_result_function(source);
    function.body = Vec::new();
    function.purity_declared = false;
    function.external = Some(rumoca_core::ExternalFunction {
        language: "C".to_string(),
        function_name: Some("two".to_string()),
        output_name: None,
        args: Vec::new(),
        annotations: Vec::new(),
    });
    function
}

/// A caller whose body is `<call statement>; y := p;`, with locals `p` and `q`.
fn caller_function(
    source: &TestSource,
    call: rumoca_core::Statement,
    locals: &[&str],
) -> rumoca_core::Function {
    let span = source.span("function caller", 0);
    let mut function = rumoca_core::Function::new("caller", span);
    function.add_input(real_function_param(
        "u",
        Vec::new(),
        source.span("input Real u", 0),
    ));
    function.add_output(real_function_param(
        "y",
        Vec::new(),
        source.span("output Real y", 0),
    ));
    for local in locals {
        function.add_local(real_function_param(
            local,
            Vec::new(),
            source.span(&format!("Real {local}"), 0),
        ));
    }
    let result = source.span("y := p", 0);
    function.body = vec![
        call,
        rumoca_core::Statement::Assignment {
            comp: test_component_reference("y", result),
            value: Expression::VarRef {
                name: Reference::new("p"),
                subscripts: Vec::new(),
                span: source.span("p", 2),
            },
            span: result,
        },
    ];
    function
}

fn multi_output_call(
    source: &TestSource,
    outputs: Vec<Option<&str>>,
    span: Span,
) -> rumoca_core::Statement {
    rumoca_core::Statement::FunctionCall {
        comp: test_component_reference("two", span),
        args: vec![Expression::VarRef {
            name: Reference::new("u"),
            subscripts: Vec::new(),
            span: source.span("u", 1),
        }],
        outputs: outputs
            .into_iter()
            .map(|target| target.map(|name| test_component_reference(name, span)))
            .collect(),
        span,
    }
}

/// Assemble the model `caller(1.0)` over the two functions above.
fn model_with(caller: rumoca_core::Function, two: rumoca_core::Function) -> flat::Model {
    let mut model = test_model();
    model.add_function(two);
    model.add_function(caller);
    model.is_partial = true;
    model
}

fn add_caller_equation(model: &mut flat::Model, source: &TestSource) {
    let call_span = source.span("caller(1.0)", 0);
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("caller"),
            args: vec![Expression::Literal {
                value: Literal::Real(1.0),
                span: source.span("1.0", 0),
            }],
            is_constructor: false,
            span: call_span,
        },
        call_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
}

const TEXT: &str = "function two input Real x; output Real a; output Real b; algorithm a := x; b := x; end two; \
     function caller input Real u; output Real y; protected Real p; Real q; algorithm (p, q) := two(u); y := p; end caller; caller(1.0);";

/// MLS §11.2.1.1: every receiving variable takes its result, left to right.
#[test]
fn multi_result_call_defines_every_receiving_variable() {
    let source = TestSource::new(TEXT);
    let call_span = source.span("(p, q) := two(u)", 0);
    let call = multi_output_call(&source, vec![Some("p"), Some("q")], call_span);
    let caller = caller_function(&source, call, &["p", "q"]);
    let mut model = model_with(caller, two_result_function(&source));
    add_caller_equation(&mut model, &source);

    let dae =
        construct(&model, source.map).expect("a multi-result call statement is constructible");
    dae.inspect(|view| {
        let caller = constructed_function(view, "caller");
        // `p` and `q` are both defined, so both locals survive with the
        // statement that owns them.
        let locals = caller
            .values()
            .filter(|value| value.role() == dae::FunctionValueRole::Local)
            .map(|value| value.name().as_str().to_string())
            .collect::<Vec<_>>();
        assert_eq!(locals, vec!["p".to_string(), "q".to_string()]);
        // Two receivers plus the result assignment.
        assert_eq!(caller.statements().count(), 3);
    });
}

/// MLS §11.2.1.1: "It is possible to omit receiving variables from this list".
/// An omitted slot reads no result, so it defines nothing and mints nothing.
#[test]
fn omitted_receiving_variable_defines_nothing() {
    let source = TestSource::new(TEXT);
    let call_span = source.span("(p, q) := two(u)", 0);
    let call = multi_output_call(&source, vec![Some("p"), None], call_span);
    let caller = caller_function(&source, call, &["p"]);
    let mut model = model_with(caller, two_result_function(&source));
    add_caller_equation(&mut model, &source);

    let dae = construct(&model, source.map).expect("an omitted receiver is constructible");
    dae.inspect(|view| {
        let caller = constructed_function(view, "caller");
        // Only the bound receiver and the result assignment: the omitted slot
        // adds no statement.
        assert_eq!(caller.statements().count(), 2);
    });
}

/// MLS §11.2.1.1: "A function with n results needs m≤n receiving variables",
/// so a single receiver for a two-result callee reads only the first result.
#[test]
fn fewer_receivers_than_results_reads_the_leading_results() {
    let source = TestSource::new(TEXT);
    let call_span = source.span("(p, q) := two(u)", 0);
    let call = multi_output_call(&source, vec![Some("p")], call_span);
    let caller = caller_function(&source, call, &["p"]);
    let mut model = model_with(caller, two_result_function(&source));
    add_caller_equation(&mut model, &source);

    let dae = construct(&model, source.map).expect("m < n receivers is constructible");
    dae.inspect(|view| {
        let caller = constructed_function(view, "caller");
        assert_eq!(caller.statements().count(), 2);
    });
}

/// MLS §11.2.1.1 bounds the receiving list by the result count, so m > n has no
/// result to assign from and is named as that, not left to a later owner.
#[test]
fn more_receivers_than_results_is_rejected_by_name() {
    let source = TestSource::new(TEXT);
    let call_span = source.span("(p, q) := two(u)", 0);
    let call = multi_output_call(&source, vec![Some("p"), Some("q"), Some("y")], call_span);
    let caller = caller_function(&source, call, &["p", "q"]);
    let mut model = model_with(caller, two_result_function(&source));
    add_caller_equation(&mut model, &source);

    let error = construct(&model, source.map).expect_err("m > n has no result to read");
    let message = error.to_string();
    assert!(
        message.contains("MLS §11.2.1.1 admits at most one receiving variable per result"),
        "unexpected rejection: {message}"
    );
    assert!(
        message.contains("declares 2 but the call site writes 3"),
        "rejection does not name the counts: {message}"
    );
}

/// A call statement that reads no result defines nothing, and MLS §12.3 leaves
/// a pure Modelica body no other effect to carry.
#[test]
fn call_statement_without_a_receiver_is_rejected_by_name() {
    let source = TestSource::new(TEXT);
    let call_span = source.span("(p, q) := two(u)", 0);
    let call = multi_output_call(&source, vec![None, None], call_span);
    let caller = caller_function(&source, call, &["p"]);
    let mut model = model_with(caller, two_result_function(&source));
    add_caller_equation(&mut model, &source);

    let error = construct(&model, source.map).expect_err("a resultless call statement is refused");
    let message = error.to_string();
    assert!(
        message.contains("without reading a result"),
        "unexpected rejection: {message}"
    );
}

/// MLS §12.4.3 evaluates the right-hand call once, but the canonical DAE reads
/// each result as its own call. For an impure external callee those are not the
/// same thing, so the statement is refused by name instead of invoking the
/// external body once per receiver.
#[test]
fn multi_result_call_to_an_impure_external_callee_is_rejected_by_name() {
    let source = TestSource::new(TEXT);
    let call_span = source.span("(p, q) := two(u)", 0);
    let call = multi_output_call(&source, vec![Some("p"), Some("q")], call_span);
    let caller = caller_function(&source, call, &["p", "q"]);
    let mut model = model_with(caller, external_two_result_function(&source));
    add_caller_equation(&mut model, &source);

    let error = construct(&model, source.map)
        .expect_err("an impure external callee owns no multi-result statement");
    let message = error.to_string();
    assert!(
        message.contains("MLS §12.4.3 evaluates a multi-result call once")
            && message.contains("is an impure external function"),
        "unexpected rejection: {message}"
    );
}

/// MLS §12.4.3 / SPEC_0022 FUNC-025: a receiving variable must agree with the
/// result it takes, and the rejection says which shapes disagreed.
#[test]
fn receiving_variable_shape_disagreement_is_rejected_by_name() {
    let source = TestSource::new(TEXT);
    let call_span = source.span("(p, q) := two(u)", 0);
    let call = multi_output_call(&source, vec![Some("p"), Some("q")], call_span);
    let mut caller = caller_function(&source, call, &["p"]);
    // `q` is declared a 3-vector while result 1 of `two` is a scalar.
    caller.add_local(real_function_param("q", vec![3], source.span("Real q", 0)));
    let mut model = model_with(caller, two_result_function(&source));
    add_caller_equation(&mut model, &source);

    let error =
        construct(&model, source.map).expect_err("a 3-vector receiver cannot take a scalar result");
    let message = error.to_string();
    assert!(
        message.contains("receiving variable `q` has shape") && message.contains("but result 1 of"),
        "unexpected rejection: {message}"
    );
}

// ===========================================================================
// MLS §10.4.2.1 `[ ]` concatenation shape
// ===========================================================================

const MATRIX_TEXT: &str = "function rank2 input Real M[:, :]; output Real s; algorithm s := 1.0; end rank2; rank2([0, 1, 1, 0, 0]);";

fn rank2_function(source: &TestSource) -> rumoca_core::Function {
    let span = source.span("function rank2", 0);
    let mut function = rumoca_core::Function::new("rank2", span);
    // `Real M[:, :]`: two symbolic extents the call site settles.
    let declaration = source.span("input Real M[:, :]", 0);
    function.add_input(
        real_function_param("M", vec![0, 0], declaration).with_shape_expr(vec![
            rumoca_core::Subscript::colon(declaration),
            rumoca_core::Subscript::colon(declaration),
        ]),
    );
    function.add_output(real_function_param(
        "s",
        Vec::new(),
        source.span("output Real s", 0),
    ));
    let assignment = source.span("s := 1.0", 0);
    function.body = vec![rumoca_core::Statement::Assignment {
        comp: test_component_reference("s", assignment),
        value: Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1.0", 0),
        },
        span: assignment,
    }];
    function
}

fn scalar(source: &TestSource, needle: &str, occurrence: usize) -> Expression {
    Expression::Literal {
        value: Literal::Real(0.0),
        span: source.span(needle, occurrence),
    }
}

fn rank2_call_model(source: &TestSource, argument: Expression) -> flat::Model {
    let mut model = test_model();
    model.add_function(rank2_function(source));
    model.is_partial = true;
    let call_span = source.span("rank2([0, 1, 1, 0, 0])", 0);
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("rank2"),
            args: vec![argument],
            is_constructor: false,
            span: call_span,
        },
        call_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
    model
}

/// MLS §10.4.2.1 concatenates `[A, B, C]` along dimension 2 over `promote(_, 2)`,
/// so a row of scalars is a 1 x n *matrix* and satisfies a `Real[:,:]` formal.
#[test]
fn matrix_row_of_scalars_proves_rank_two() {
    let source = TestSource::new(MATRIX_TEXT);
    let elements = (0..5)
        .map(|index| scalar(&source, "0", index.min(1)))
        .collect::<Vec<_>>();
    let argument = Expression::Array {
        elements,
        is_matrix: true,
        span: source.span("[0, 1, 1, 0, 0]", 0),
    };
    let model = rank2_call_model(&source, argument);
    construct(&model, source.map)
        .expect("a `[ ]` row of scalars is the rank-2 argument MLS §10.4.2.1 gives it");
}

/// The same elements written as the MLS §10.4 array constructor `{ }` stay a
/// vector, so the rank-2 formal still reports the call-site rank it received.
#[test]
fn array_constructor_of_scalars_still_reports_the_rank_mismatch() {
    let source = TestSource::new(MATRIX_TEXT);
    let elements = (0..5)
        .map(|index| scalar(&source, "0", index.min(1)))
        .collect::<Vec<_>>();
    let argument = Expression::Array {
        elements,
        is_matrix: false,
        span: source.span("[0, 1, 1, 0, 0]", 0),
    };
    let model = rank2_call_model(&source, argument);
    let error = construct(&model, source.map).expect_err("a vector is not a matrix");
    let message = error.to_string();
    assert!(
        message.contains("declared rank 2 does not match call-site rank 1"),
        "unexpected rejection: {message}"
    );
}

/// MLS §12.4.2.1 partial application denotes a *function*, not an array, so it
/// is named as the unimplemented construct it is rather than reported as the
/// arity mismatch of a full call.
#[test]
fn function_partial_application_is_rejected_by_name() {
    let source = TestSource::new(MATRIX_TEXT);
    // A call to `rank2` supplying one named association for its two formals is
    // the shape flatten leaves a partial application in.
    let argument_span = source.span("[0, 1, 1, 0, 0]", 0);
    let named = Expression::FunctionCall {
        name: Reference::new(format!("{}M", rumoca_core::NAMED_FUNCTION_ARG_PREFIX)),
        args: vec![scalar(&source, "0", 0)],
        is_constructor: true,
        span: argument_span,
    };
    let mut model = test_model();
    let mut rank2 = rank2_function(&source);
    // Give the callee a second formal so one supplied argument is partial.
    rank2.add_input(real_function_param(
        "eps",
        Vec::new(),
        source.span("output Real s", 0),
    ));
    model.add_function(rank2);
    model.is_partial = true;
    let call_span = source.span("rank2([0, 1, 1, 0, 0])", 0);
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("rank2"),
            args: vec![named],
            is_constructor: false,
            span: call_span,
        },
        call_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let error =
        construct(&model, source.map).expect_err("a partial application has no value shape");
    let message = error.to_string();
    assert!(
        message.contains("MLS §12.4.2.1 partial application of `rank2`")
            && message.contains("binds 1 of 2 formal parameters"),
        "unexpected rejection: {message}"
    );
}

/// Each unshaped expression form names itself rather than sharing one message.
///
/// `FieldAccess` is the reachable one — `ShowImpedance` and `SinglePhaseInductance`
/// both land on it through a symbolically indexed record array element.
/// `Tuple` and `Empty` are defensive, and are pinned so a future producer that
/// does emit one gets a message that says which construct was refused.
#[test]
fn each_unshaped_expression_form_names_its_own_construct() {
    for (argument_of, expected) in [
        ("field", "MLS §12.2 record-field projection"),
        (
            "tuple",
            "an MLS §11.2.1.1 result tuple has no shape of its own",
        ),
        ("empty", "an absent expression has no shape"),
    ] {
        let source = TestSource::new(MATRIX_TEXT);
        let span = source.span("[0, 1, 1, 0, 0]", 0);
        let argument = match argument_of {
            "field" => Expression::FieldAccess {
                base: Box::new(scalar(&source, "0", 0)),
                field: "re".to_string(),
                field_def_id: rumoca_core::DefId::new(1),
                span,
            },
            "tuple" => Expression::Tuple {
                elements: vec![scalar(&source, "0", 0)],
                span,
            },
            _ => Expression::Empty { span },
        };
        let model = rank2_call_model(&source, argument);
        let Err(error) = construct(&model, source.map) else {
            panic!("`{argument_of}` owns no checked shape");
        };
        let message = error.to_string();
        assert!(
            message.contains(expected),
            "`{argument_of}` did not name its construct: {message}"
        );
    }
}

/// A `[ ]` operand that is not a scalar needs the promoting `cat` the canonical
/// DAE has no owner for, and says so rather than proving a shape it cannot build.
#[test]
fn matrix_with_a_non_scalar_operand_is_rejected_by_name() {
    let source = TestSource::new(MATRIX_TEXT);
    let inner = Expression::Array {
        elements: vec![scalar(&source, "0", 0), scalar(&source, "1", 0)],
        is_matrix: false,
        span: source.span("0, 1", 0),
    };
    let argument = Expression::Array {
        elements: vec![inner],
        is_matrix: true,
        span: source.span("[0, 1, 1, 0, 0]", 0),
    };
    let model = rank2_call_model(&source, argument);
    let error = construct(&model, source.map).expect_err("a vector operand needs `cat` promotion");
    let message = error.to_string();
    assert!(
        message.contains("MLS §10.4.2.1")
            && message.contains("ambiguous horizontal")
            && message.contains("rank-1 operand"),
        "unexpected rejection: {message}"
    );
}

/// The `;` spelling has an unambiguous nested-row owner, so its vector
/// operands are promoted to columns and concatenated through the checked DAE
/// operation instead of being rejected or scalarized during ToDAE.
#[test]
fn semicolon_matrix_of_vectors_constructs_with_promoted_shape() {
    let source = TestSource::new(MATRIX_TEXT);
    let vector = |occurrence| Expression::Array {
        elements: vec![
            scalar(&source, "0", occurrence),
            scalar(&source, "1", occurrence),
        ],
        is_matrix: false,
        span: source.span("0, 1", 0),
    };
    let row = |occurrence| Expression::Array {
        elements: vec![vector(occurrence)],
        is_matrix: true,
        span: source.span("[0, 1, 1, 0, 0]", 0),
    };
    let argument = Expression::Array {
        elements: vec![row(0), row(1)],
        is_matrix: true,
        span: source.span("[0, 1, 1, 0, 0]", 0),
    };
    let model = rank2_call_model(&source, argument);
    construct(&model, source.map)
        .expect("two promoted length-2 columns concatenate to a checked 4 x 1 matrix");
}

/// The same MLS §10.4.2.1 refusal must reach a `[ ]` written in a *model*
/// equation, where no call-argument shape rule runs. `[v1, v2]` over declared
/// vectors is syntactically a scalar-operand row, so without this the
/// constructor reported a bare `ED020` naming no construct — and the base
/// compiler silently transposed it (y = 2 where OMC gives 3).
#[test]
fn model_scope_matrix_row_of_vectors_is_rejected_by_name() {
    let source =
        TestSource::new("model M Real v1[3]; Real v2[3]; equation 0 = sum([v1, v2]); end M;");
    let mut model = test_model();
    add_primitive_variable(&mut model, &source, "v1", "Real v1[3]", 11, vec![3], false);
    add_primitive_variable(&mut model, &source, "v2", "Real v2[3]", 12, vec![3], false);
    model.is_partial = true;
    let row_span = source.span("[v1, v2]", 0);
    let row = Expression::Array {
        elements: vec![
            Expression::VarRef {
                name: test_reference("v1"),
                subscripts: Vec::new(),
                span: source.span("v1", 1),
            },
            Expression::VarRef {
                name: test_reference("v2"),
                subscripts: Vec::new(),
                span: source.span("v2", 1),
            },
        ],
        is_matrix: true,
        span: row_span,
    };
    model.add_equation(flat::Equation::new(
        row,
        row_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let error = construct(&model, source.map)
        .expect_err("a model-scope row of vectors needs `cat` promotion");
    let message = error.to_string();
    assert!(
        message.contains("MLS §10.4.2.1")
            && message.contains("ambiguous horizontal")
            && message.contains("rank-1 operand"),
        "unexpected rejection: {message}"
    );
}
