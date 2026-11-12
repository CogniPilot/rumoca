use super::*;
use crate::construction::analysis::loop_compaction::preservation_corpus::{
    assign, binary, branch, integer, span, var,
};
use rumoca_core::BuiltinFunction;

fn division(divisor: i64) -> Expression {
    divided_by(integer(divisor))
}

fn divided_by(divisor: Expression) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Div,
        args: vec![var("k"), divisor],
        span: span(),
    }
}

fn bound(
    start: Option<i64>,
    condition: Expression,
    body: Vec<rumoca_core::Statement>,
) -> Option<i64> {
    let shapes = ShapeEnvironment::default();
    bound_with_shapes(start, condition, body, &shapes)
}

/// Run `check` in a scope where `k` is the only Integer.
fn with_scope<T>(shapes: &ShapeEnvironment, check: impl FnOnce(FactScope<'_>) -> T) -> T {
    let integers = HashSet::from([VarName::new("k")]);
    let reals = HashSet::new();
    check(FactScope {
        shapes,
        integers: &integers,
        reals: &reals,
        generated: &[],
    })
}

/// `k ± delta`.
fn step(op: OpBinary, delta: i64) -> rumoca_core::Statement {
    assign("k", binary(op, var("k"), integer(delta)))
}

fn bound_with_shapes(
    start: Option<i64>,
    condition: Expression,
    body: Vec<rumoca_core::Statement>,
    shapes: &ShapeEnvironment,
) -> Option<i64> {
    with_scope(shapes, |scope| {
        let mut entry = GuardFacts::entry();
        if let Some(start) = start {
            entry.after(&assign("k", integer(start)), scope);
        }
        let block = StatementBlock {
            cond: condition,
            stmts: body,
        };
        iteration_bound(&block, &entry, &entry.while_head(&block, scope), scope)
    })
}

#[test]
fn division_proves_the_exact_worst_case_pass_count() {
    for (start, divisor, expected) in [(1, 2, 1), (16, 2, 5), (17, 3, 3), (0, 2, 0)] {
        assert_eq!(
            bound(
                Some(start),
                binary(OpBinary::Gt, var("k"), integer(0)),
                vec![assign("k", division(divisor))]
            ),
            Some(expected)
        );
    }
    assert_eq!(
        bound(
            Some(i64::MAX),
            binary(OpBinary::Lt, integer(0), var("k")),
            vec![assign("k", division(2))]
        ),
        Some(63)
    );
}

#[test]
fn division_rejects_nonprogress_and_additional_counter_writes() {
    for divisor in [-2, 0, 1] {
        assert_eq!(
            bound(
                Some(8),
                binary(OpBinary::Gt, var("k"), integer(0)),
                vec![assign("k", division(divisor))]
            ),
            None
        );
    }
    for body in [
        vec![assign("k", integer(8)), assign("k", division(2))],
        vec![assign("k", division(2)), assign("k", integer(8))],
    ] {
        assert_eq!(
            bound(Some(8), binary(OpBinary::Gt, var("k"), integer(0)), body),
            None
        );
    }
    assert_eq!(
        bound(
            None,
            binary(OpBinary::Gt, var("k"), integer(0)),
            vec![assign("k", division(2))]
        ),
        None
    );
    assert_eq!(
        bound(
            Some(8),
            binary(OpBinary::Ge, var("k"), integer(0)),
            vec![assign("k", division(2))]
        ),
        None,
        "division reaches zero but cannot falsify k >= 0"
    );
}

#[test]
fn a_false_entry_condition_proves_zero_passes_in_either_direction() {
    for (start, condition, update) in [
        (
            1,
            binary(OpBinary::Gt, var("k"), integer(1)),
            step(OpBinary::Sub, 1),
        ),
        (
            5,
            binary(OpBinary::Le, var("k"), integer(3)),
            step(OpBinary::Add, 1),
        ),
    ] {
        assert_eq!(bound(Some(start), condition, vec![update]), Some(0));
    }
}

#[test]
fn an_invalid_condition_access_is_not_a_zero_pass_proof() {
    use crate::construction::analysis::loop_compaction::preservation_corpus::element;
    let mut shapes = ShapeEnvironment::default();
    shapes.insert(VarName::new("samples"), vec![4]);
    assert_eq!(
        bound_with_shapes(
            Some(0),
            binary(OpBinary::Gt, element("samples", var("k")), integer(0)),
            vec![step(OpBinary::Sub, 1)],
            &shapes
        ),
        None
    );
}

#[test]
fn an_unreachable_loop_never_peels_its_body() {
    let shapes = ShapeEnvironment::default();
    let statement = rumoca_core::Statement::While {
        block: StatementBlock {
            cond: binary(OpBinary::Gt, var("k"), integer(1)),
            stmts: vec![step(OpBinary::Sub, 1)],
        },
        span: span(),
    };
    let normalized = with_scope(&shapes, |scope| {
        bound_statement(&statement, scope, &GuardFacts::join(&[]))
    });
    let [rumoca_core::Statement::For { indices, .. }] = normalized.as_slice() else {
        panic!("an unreachable loop has an empty compact domain, not a peeled body");
    };
    let Expression::Range { end, .. } = &indices[0].range else {
        panic!("the compact loop has a fixed range");
    };
    assert_eq!(shapes.proven_extent(end), Some(0));
}

#[test]
fn a_decreasing_counter_has_a_finite_progress_bound() {
    assert_eq!(
        bound(
            Some(8),
            binary(OpBinary::Ge, var("k"), integer(0)),
            vec![step(OpBinary::Sub, 1)]
        ),
        Some(9)
    );
}

#[test]
fn progress_rejects_stalling_or_opposite_directions_on_continuing_paths() {
    let condition = binary(
        OpBinary::And,
        binary(OpBinary::Ge, var("k"), integer(0)),
        binary(OpBinary::Le, var("k"), integer(8)),
    );
    for delta in [0, 1] {
        let body = vec![rumoca_core::Statement::If {
            cond_blocks: vec![StatementBlock {
                cond: var("choose"),
                stmts: vec![step(OpBinary::Sub, 1)],
            }],
            else_block: Some(vec![step(OpBinary::Add, delta)]),
            span: span(),
        }];
        assert_eq!(bound(Some(4), condition.clone(), body), None);
    }
}

#[test]
fn division_requires_its_fixed_expressions_to_stay_invariant() {
    let mut shapes = ShapeEnvironment::default();
    shapes.bind_scalar_value(
        VarName::new("d"),
        rumoca_eval_flat::constant::Value::Integer(2),
    );
    shapes.bind_scalar_value(
        VarName::new("zero"),
        rumoca_eval_flat::constant::Value::Integer(0),
    );
    let update = assign("k", divided_by(var("d")));
    let condition = binary(OpBinary::Gt, var("k"), var("zero"));
    assert_eq!(
        bound_with_shapes(Some(8), condition.clone(), vec![update.clone()], &shapes),
        Some(4)
    );
    for (name, value) in [("d", 1), ("zero", -1)] {
        assert_eq!(
            bound_with_shapes(
                Some(8),
                condition.clone(),
                vec![assign(name, integer(value)), update.clone()],
                &shapes
            ),
            None
        );
    }
}

#[test]
fn a_write_of_the_enclosing_record_is_a_counter_write() {
    let written = HashSet::from([VarName::new("r")]);
    assert!(writes_name(&written, &VarName::new("r.k")));
    assert!(writes_name(
        &HashSet::from([VarName::new("r.k")]),
        &VarName::new("r")
    ));
    assert!(!writes_name(&written, &VarName::new("rk")));
    assert!(!writes_name(&written, &VarName::new("q.r")));
}

/// `if condition then <exit> end if`.
fn exit_when(condition: Expression, exit: rumoca_core::Statement) -> rumoca_core::Statement {
    branch(vec![(condition, vec![exit])], None)
}

fn leave_when(condition: Expression) -> rumoca_core::Statement {
    exit_when(condition, rumoca_core::Statement::Break { span: span() })
}

/// MLS §11.2.4: a pass that breaks is the last one. The counter form keeps
/// its bound, and the progress form counts that pass once more, as it counts
/// the pass an exit flag ends.
#[test]
fn a_break_keeps_the_counter_bound_and_adds_its_pass_to_progress() {
    let counted = binary(OpBinary::Lt, var("k"), integer(5));
    let leave = leave_when(binary(OpBinary::Eq, var("k"), integer(2)));
    let rise = step(OpBinary::Add, 1);
    assert_eq!(bound(Some(1), counted.clone(), vec![rise.clone()]), Some(5));
    assert_eq!(bound(Some(1), counted, vec![leave.clone(), rise]), Some(5));
    let window = binary(
        OpBinary::And,
        binary(OpBinary::Ge, var("k"), integer(0)),
        binary(OpBinary::Le, var("k"), integer(8)),
    );
    let fall = step(OpBinary::Sub, 1);
    assert_eq!(bound(Some(8), window.clone(), vec![fall.clone()]), Some(9));
    assert_eq!(bound(Some(8), window, vec![leave, fall]), Some(10));
}

/// MLS §11.2.5: a pass that returns is the last one too, so the counter
/// form keeps its bound and the progress form counts that pass once more.
#[test]
fn a_return_keeps_the_counter_bound_and_adds_its_pass_to_progress() {
    let ret = exit_when(
        binary(OpBinary::Eq, var("k"), integer(2)),
        rumoca_core::Statement::Return { span: span() },
    );
    assert_eq!(
        bound(
            Some(1),
            binary(OpBinary::Lt, var("k"), integer(5)),
            vec![ret.clone(), step(OpBinary::Add, 1)]
        ),
        Some(5)
    );
    let window = binary(
        OpBinary::And,
        binary(OpBinary::Ge, var("k"), integer(0)),
        binary(OpBinary::Le, var("k"), integer(8)),
    );
    assert_eq!(
        bound(Some(8), window, vec![ret, step(OpBinary::Sub, 1)]),
        Some(10)
    );
}

/// A loop that holds on entry has its first pass written out, but not when
/// the pass can break: the `break` would then leave no loop.
#[test]
fn a_loop_that_can_break_is_not_peeled() {
    let shapes = ShapeEnvironment::default();
    let lowered = |stmts| {
        let statement = rumoca_core::Statement::While {
            block: StatementBlock {
                cond: binary(OpBinary::Lt, var("k"), integer(5)),
                stmts,
            },
            span: span(),
        };
        with_scope(&shapes, |scope| {
            let mut entry = GuardFacts::entry();
            entry.after(&assign("k", integer(1)), scope);
            bound_statement(&statement, scope, &entry)
        })
    };
    let rise = step(OpBinary::Add, 1);
    assert!(
        lowered(vec![rise.clone()]).len() > 1,
        "a bounded loop peels"
    );
    let leave = leave_when(binary(OpBinary::Eq, var("k"), integer(2)));
    assert!(matches!(
        lowered(vec![leave, rise]).as_slice(),
        [rumoca_core::Statement::For { .. }]
    ));
}

/// `while not done` ends once `done` is true, as `while going` ends once
/// `going` is false: both prove the same progress bound.
#[test]
fn a_negated_exit_flag_proves_the_same_progress_bound() {
    let walk = |flag: &str, ends_with: bool| {
        vec![branch(
            vec![(
                binary(OpBinary::Ge, var("k"), integer(4)),
                vec![assign(
                    flag,
                    Expression::Literal {
                        value: Literal::Boolean(ends_with),
                        span: span(),
                    },
                )],
            )],
            Some(vec![step(OpBinary::Add, 1)]),
        )]
    };
    let positive = bound(Some(0), var("going"), walk("going", false));
    let negated = bound(
        Some(0),
        Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs: Box::new(var("done")),
            span: span(),
        },
        walk("done", true),
    );
    assert!(positive.is_some(), "the exit flag form proves a bound");
    assert_eq!(negated, positive);
    // Storing the value that keeps the loop running ends nothing.
    let stalled = bound(
        Some(0),
        Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs: Box::new(var("done")),
            span: span(),
        },
        walk("done", false),
    );
    assert_eq!(stalled, None);
}
