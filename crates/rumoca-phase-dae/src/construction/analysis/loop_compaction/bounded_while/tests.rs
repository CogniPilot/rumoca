use super::*;
use crate::construction::analysis::loop_compaction::preservation_corpus::{
    assign, binary, integer, span, var,
};
use rumoca_core::BuiltinFunction;

fn division(divisor: i64) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Div,
        args: vec![var("k"), integer(divisor)],
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

fn bound_with_shapes(
    start: Option<i64>,
    condition: Expression,
    body: Vec<rumoca_core::Statement>,
    shapes: &ShapeEnvironment,
) -> Option<i64> {
    let integers = HashSet::from([VarName::new("k")]);
    let reals = HashSet::new();
    let scope = FactScope {
        shapes,
        integers: &integers,
        reals: &reals,
        generated: &[],
    };
    let mut entry = GuardFacts::entry();
    if let Some(start) = start {
        entry.after(&assign("k", integer(start)), scope);
    }
    let block = StatementBlock {
        cond: condition,
        stmts: body,
    };
    iteration_bound(&block, &entry, &entry.while_head(&block, scope), scope)
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
fn a_decreasing_counter_has_a_finite_progress_bound() {
    assert_eq!(
        bound(
            Some(8),
            binary(OpBinary::Ge, var("k"), integer(0)),
            vec![assign("k", binary(OpBinary::Sub, var("k"), integer(1)))]
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
                stmts: vec![assign("k", binary(OpBinary::Sub, var("k"), integer(1)))],
            }],
            else_block: Some(vec![assign(
                "k",
                binary(OpBinary::Add, var("k"), integer(delta)),
            )]),
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
    let update = assign(
        "k",
        Expression::BuiltinCall {
            function: BuiltinFunction::Div,
            args: vec![var("k"), var("d")],
            span: span(),
        },
    );
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
