use super::super::preservation_corpus::{assign, binary, branch, element, integer, span, var};
use super::super::preservation_interpreter::interpret;
use super::super::preservation_values::{Environment, Value};
use super::*;

fn loop_source(lower: i64, step: i64, upper: i64, body: Vec<Statement>) -> Vec<Statement> {
    vec![Statement::For {
        indices: vec![rumoca_core::ForIndex {
            binder: rumoca_core::IndexBinderProvenance::Generated,
            ident: "i".into(),
            range: Expression::Range {
                start: Box::new(integer(lower)),
                step: Some(Box::new(integer(step))),
                end: Box::new(integer(upper)),
                span: span(),
            },
        }],
        equations: body,
        span: span(),
    }]
}

fn leading_break(condition: Expression) -> Statement {
    branch(
        vec![(condition, vec![Statement::Break { span: span() }])],
        None,
    )
}

fn entry() -> Environment {
    Environment::from([
        (VarName::new("i"), Value::Integer(42)),
        (VarName::new("u"), Value::Real(1.0)),
        (VarName::new("y"), Value::Real(0.0)),
        (VarName::new("t"), Value::Array(vec![Value::Real(0.0); 3])),
    ])
}

fn compare_execution(
    range: (i64, i64, i64),
    operator: &OpBinary,
    threshold: i64,
    reverse: bool,
) -> bool {
    let (lower, step, upper) = range;
    let (lhs, rhs) = if reverse {
        (integer(threshold), var("i"))
    } else {
        (var("i"), integer(threshold))
    };
    let source = loop_source(
        lower,
        step,
        upper,
        vec![
            leading_break(binary(operator.clone(), lhs, rhs)),
            assign(
                "y",
                binary(
                    OpBinary::Add,
                    var("y"),
                    binary(OpBinary::Add, var("u"), var("i")),
                ),
            ),
        ],
    );
    let after = truncate_prefix_breaks(&source);
    let expected = interpret(&source, &entry()).expect("bounded source executes its break");
    let actual = interpret(&after, &entry()).expect("bounded compact prefix executes");
    assert_eq!(
        actual, expected,
        "{lower}:{step}:{upper}, {operator:?}, {threshold}, reverse={reverse}"
    );
    source != after
}

#[test]
fn finite_prefix_breaks_preserve_observable_execution_in_both_directions() {
    let operators = [
        OpBinary::Eq,
        OpBinary::Neq,
        OpBinary::Lt,
        OpBinary::Le,
        OpBinary::Gt,
        OpBinary::Ge,
    ];
    let mut rewritten = 0;
    for (lower, step, upper) in [
        (-8, 1, 8),
        (-8, 3, 8),
        (8, -1, -8),
        (8, -3, -8),
        (2, 1, 1),
        (1, -1, 2),
    ] {
        for operator in &operators {
            for (threshold, reverse) in
                (-10..=10).flat_map(|threshold| [false, true].map(|reverse| (threshold, reverse)))
            {
                rewritten += usize::from(compare_execution(
                    (lower, step, upper),
                    operator,
                    threshold,
                    reverse,
                ));
            }
        }
    }
    assert_eq!(
        rewritten, 1512,
        "every tested prefix received its exact rewrite"
    );
}

#[test]
fn breaking_first_does_not_evaluate_an_invalid_suffix() {
    let source = loop_source(
        1,
        1,
        5,
        vec![
            leading_break(binary(OpBinary::Eq, var("i"), integer(1))),
            assign("y", element("t", integer(999))),
        ],
    );
    let after = truncate_prefix_breaks(&source);
    assert!(after.is_empty());
    assert_eq!(
        interpret(&source, &entry()).unwrap(),
        interpret(&after, &entry()).unwrap()
    );
}

#[test]
fn runtime_or_nonleading_breaks_keep_their_source_owner() {
    let update = assign("y", binary(OpBinary::Add, var("y"), integer(1)));
    for body in [
        vec![
            leading_break(binary(OpBinary::Gt, var("u"), integer(0))),
            update.clone(),
        ],
        vec![
            update,
            leading_break(binary(OpBinary::Eq, var("i"), integer(4))),
        ],
    ] {
        let source = loop_source(1, 1, 5, body);
        assert_eq!(truncate_prefix_breaks(&source), source);
    }
}

#[test]
fn nested_shadowed_binders_break_only_their_own_loop() {
    let inner = loop_source(
        1,
        1,
        5,
        vec![
            leading_break(binary(OpBinary::Eq, var("i"), integer(3))),
            assign("y", binary(OpBinary::Add, var("y"), var("i"))),
        ],
    );
    let source = loop_source(1, 1, 4, inner);
    let after = truncate_prefix_breaks(&source);
    let actual = interpret(&after, &entry()).unwrap();
    assert_eq!(actual, interpret(&source, &entry()).unwrap());
    assert_eq!(actual[&VarName::new("y")], Value::Real(12.0));
    assert_eq!(actual[&VarName::new("i")], Value::Integer(42));
}

#[test]
fn oversized_progression_metadata_is_refused_without_traversal() {
    let source = loop_source(
        0,
        1,
        i64::MAX,
        vec![
            leading_break(binary(OpBinary::Eq, var("i"), integer(4))),
            assign("y", integer(1)),
        ],
    );
    assert_eq!(truncate_prefix_breaks(&source), source);
}
