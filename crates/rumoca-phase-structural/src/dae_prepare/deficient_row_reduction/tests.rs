//! Equation-driven nomination tests.
//!
//! The fixtures are scalar by construction so the rank witness's row widths are
//! exactly one, which keeps each test about nomination rather than about shape
//! inference.

use rumoca_core::{BuiltinFunction, Literal, OpBinary, Span, VarName};
use rumoca_ir_dae::{Dae, Equation, Variable};

use super::{
    Expression, Nomination, Prolongation, ProlongationOutcome, RankOutcome,
    alternating_path_shells, expression_node_count, index_reduce_deficient_constraint_rows,
    index_reduce_deficient_constraint_rows_with_metadata, nominate_deficient_rows,
    row_gains_information, row_is_vacuous,
};

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("deficient_row_reduction_test.mo"),
        1,
        2,
    )
}

fn continuous_variable(name: &str) -> Variable {
    let mut variable = Variable::new(VarName::new(name), test_span());
    variable.source_span = test_span();
    variable
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![],
        span: test_span(),
    }
}

fn der(name: &str) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var(name)],
        span: test_span(),
    }
}

fn literal(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: test_span(),
    }
}

fn binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(),
    }
}

fn eq(rhs: Expression) -> Equation {
    Equation {
        lhs: None,
        rhs,
        span: test_span(),
        origin: "connection equation".to_string(),
        scalar_count: 1,
    }
}

/// A closed loop whose deficient row assigns no state.
///
/// ```text
/// 0 = der(x) - v      // the only ODE
/// 0 = y - 2*x         // definition of the algebraic the loop is written on
/// 0 = y - 4           // loop closure: assigns no state, has no free column
/// ```
///
/// `v` has no defining row, so the matching is one short. State-driven
/// nomination never looks at row 2 because it assigns nothing; equation-driven
/// nomination differentiates it into `0 = der(y) - 0 = 2*der(x)`, which after
/// the derivative closure reaches `v` and closes the matching.
fn loop_closure_dae() -> Dae {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), continuous_variable("x"));
    for name in ["y", "v"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), continuous_variable(name));
    }
    dae.continuous.equations = vec![
        eq(binary(OpBinary::Sub, der("x"), var("v"))),
        eq(binary(
            OpBinary::Sub,
            var("y"),
            binary(OpBinary::Mul, literal(2.0), var("x")),
        )),
        eq(binary(OpBinary::Sub, var("y"), literal(4.0))),
    ];
    dae
}

#[test]
fn differentiates_the_constraint_row_that_assigns_no_state() {
    let mut dae = loop_closure_dae();
    let changed = index_reduce_deficient_constraint_rows(&mut dae);
    assert_eq!(changed, 1, "exactly the loop-closure row is differentiated");
    assert!(
        dae.continuous.equations[2]
            .origin
            .contains("d_dt_deficient_constraint_row"),
        "row origin records the pass: {}",
        dae.continuous.equations[2].origin
    );
    assert_eq!(
        dae.initialization.equations.len(),
        1,
        "the undifferentiated constraint is retained for initialization"
    );
}

#[test]
fn a_balanced_system_is_left_alone() {
    let mut dae = loop_closure_dae();
    // Replace the loop closure with a definition of `v`: three rows over the
    // three columns `der(x)`, `y` and `v`, so the matching is already perfect.
    dae.continuous.equations[2] = eq(binary(OpBinary::Sub, var("v"), literal(1.0)));
    let before = dae.continuous.equations.clone();
    let changed = index_reduce_deficient_constraint_rows(&mut dae);
    assert_eq!(
        changed, 0,
        "nothing is nominated when the matching is perfect"
    );
    assert_eq!(dae.continuous.equations.len(), before.len());
    assert!(
        dae.initialization.equations.is_empty(),
        "no row is moved to initialization"
    );
}

#[test]
fn shells_order_the_deficient_block_by_alternating_path_depth() {
    let dae = loop_closure_dae();
    let view = super::scalar_rank_view::build(&dae).expect("scalar rank view");
    let (match_eq, match_var) =
        crate::matching::maximum_matching(view.n_eq, view.n_var, &view.rows, &[]);
    let shells = alternating_path_shells(&view, &match_eq, &match_var);
    assert!(
        shells.len() >= 2,
        "the loop closure is reached before the rows that own its columns: {shells:?}"
    );
    assert_eq!(
        shells[0].len(),
        1,
        "shell 0 is exactly the unmatched row: {shells:?}"
    );
    assert!(
        !shells[0].contains(&0),
        "the ODE row is not the deficient row: {shells:?}"
    );
}

#[test]
fn a_derivative_that_names_nothing_new_is_not_information() {
    let original = binary(OpBinary::Sub, var("y"), literal(4.0));
    let same = binary(OpBinary::Sub, var("y"), literal(2.0));
    assert!(
        !row_gains_information(&original, &same, &[]),
        "a derivative over the same columns adds no rank"
    );
    let wider = binary(OpBinary::Sub, var("y"), var("v"));
    assert!(
        row_gains_information(&original, &wider, &[]),
        "a derivative that reaches a new column adds rank"
    );
}

#[test]
fn node_count_walks_the_whole_expression() {
    assert_eq!(expression_node_count(&literal(1.0)), 1);
    assert_eq!(
        expression_node_count(&binary(OpBinary::Sub, var("y"), literal(4.0))),
        3
    );
}

/// A system that is over-determined no matter how often it is differentiated.
///
/// ```text
/// 0 = der(x) - v      // the only ODE
/// 0 = y - 2*x         // definition of `y`
/// 0 = y - 4           // one loop closure
/// 0 = y - 5           // a second closure over the same column
/// ```
///
/// Four rows over three columns (`der(x)`, `y`, `v`): the deficiency is
/// structural and no prolongation can remove it.
fn permanently_deficient_dae() -> Dae {
    let mut dae = loop_closure_dae();
    dae.continuous
        .equations
        .push(eq(binary(OpBinary::Sub, var("y"), literal(5.0))));
    dae
}

/// Identity of a row for revert comparisons: its origin *and* its residual.
///
/// `rumoca_ir_dae::Equation` is not `PartialEq`, and comparing origins alone
/// cannot see the failure this transaction exists to prevent — a revert that
/// restores the origin marker but leaves the differentiated residual in place.
/// That model still integrates and produces a trajectory that looks plausible
/// and drifts open, so the residual is what has to be pinned.
fn row_identity(equation: &Equation) -> (String, String) {
    (equation.origin.clone(), format!("{:?}", equation.rhs))
}

#[test]
fn a_prolongation_that_never_reaches_a_perfect_matching_is_fully_reverted() {
    let mut dae = permanently_deficient_dae();
    let before = dae.continuous.equations.clone();
    let changed = index_reduce_deficient_constraint_rows(&mut dae);
    assert_eq!(
        changed, 0,
        "an iteration that cannot exhibit a perfect matching keeps nothing"
    );
    let restored: Vec<_> = dae.continuous.equations.iter().map(row_identity).collect();
    let original: Vec<_> = before.iter().map(row_identity).collect();
    assert_eq!(
        restored, original,
        "every rewritten row is restored — origin and residual — not just the last round's"
    );
    assert!(
        dae.initialization.equations.is_empty(),
        "a reverted iteration leaves nothing behind in initialization"
    );
}

/// A revert that restores the origin but keeps the differentiated residual is
/// caught.
///
/// The fixture is the reverted one above with the differentiated residual of its
/// loop-closure row put back by hand: exactly the state a half-applied undo
/// record leaves behind, and the state the assertion above must reject.
#[test]
fn restoring_only_the_origin_marker_does_not_count_as_a_revert() {
    let before = permanently_deficient_dae();
    let mut half_reverted = permanently_deficient_dae();
    // `0 = y - 4` differentiates to `0 = der(y)`; the origin is untouched, which
    // is what an undo record that forgot the residual would leave.
    half_reverted.continuous.equations[2].rhs = der("y");
    let restored: Vec<_> = half_reverted
        .continuous
        .equations
        .iter()
        .map(row_identity)
        .collect();
    let original: Vec<_> = before
        .continuous
        .equations
        .iter()
        .map(row_identity)
        .collect();
    assert_ne!(
        restored, original,
        "a residual left at velocity level is not a restored row"
    );
    let origins_only: Vec<_> = half_reverted
        .continuous
        .equations
        .iter()
        .map(|equation| equation.origin.clone())
        .collect();
    let original_origins: Vec<_> = before
        .continuous
        .equations
        .iter()
        .map(|equation| equation.origin.clone())
        .collect();
    assert_eq!(
        origins_only, original_origins,
        "and comparing origins alone would have called it restored"
    );
}

/// An index-3 chain: a constraint stated at position level whose force is
/// determined at acceleration level.
///
/// ```text
/// 0 = der(x) - v          // position is integrated from a velocity state
/// 0 = der(v) - lambda     // the velocity state is driven by a constraint force
/// 0 = x - 1               // the closure, stated at position level
/// ```
///
/// `x` and `v` are states, so the view's columns are `der(x)`, `der(v)` and
/// `lambda`. The closure names none of them, so it is unmatched in every maximum
/// matching and `lambda` is left with no defining row.
///
/// One differentiation takes the closure to velocity level (`0 = v`), which
/// still names no column — the plateau that makes judging a single round
/// useless. The second takes it to acceleration level (`0 = lambda`), which
/// completes the matching. Two rounds, both retained, which is what
/// [`super::commit_differentiated_row`] claims and nothing exercised.
fn index_three_chain_dae() -> Dae {
    let mut dae = Dae::new();
    for name in ["x", "v"] {
        dae.variables
            .states
            .insert(VarName::new(name), continuous_variable(name));
    }
    dae.variables
        .algebraics
        .insert(VarName::new("lambda"), continuous_variable("lambda"));
    dae.continuous.equations = vec![
        eq(binary(OpBinary::Sub, der("x"), var("v"))),
        eq(binary(OpBinary::Sub, der("v"), var("lambda"))),
        eq(binary(OpBinary::Sub, var("x"), literal(1.0))),
    ];
    dae
}

/// Variable names a residual reads, sorted and deduplicated.
fn refs_of(expr: &Expression) -> Vec<String> {
    let mut refs = Vec::new();
    expr.collect_var_refs(&mut refs);
    let mut names: Vec<String> = refs.iter().map(|name| name.as_str().to_string()).collect();
    names.sort();
    names.dedup();
    names
}

#[test]
fn a_twice_differentiated_constraint_retains_both_its_position_and_velocity_form() {
    let mut dae = index_three_chain_dae();
    let result = index_reduce_deficient_constraint_rows_with_metadata(&mut dae);
    assert_eq!(
        result.differentiated_rows, 2,
        "the closure is differentiated once per round and both rounds are retained"
    );
    assert_eq!(result.constraints.len(), 1);
    let retained = &result.constraints[0];
    assert_eq!(retained.source_row, 2);
    assert_eq!(
        format!("{:?}", retained.holonomic.rhs),
        format!("{:?}", index_three_chain_dae().continuous.equations[2].rhs),
        "the sidecar preserves the position-level manifold residual"
    );
    assert_eq!(
        refs_of(
            &retained
                .velocity
                .as_ref()
                .expect("two differentiations retain a velocity constraint")
                .rhs
        ),
        vec!["v".to_string()],
        "the sidecar preserves the velocity-level manifold residual"
    );
    assert_eq!(
        refs_of(&dae.continuous.equations[2].rhs),
        vec!["lambda".to_string()],
        "the continuous row now constrains the acceleration-level unknown"
    );

    // Both retained forms are needed: an acceleration-level closure constrains
    // only the second derivative of the loop error, so the position form alone
    // leaves the velocity error free and the velocity form alone leaves a
    // constant position offset.
    assert_eq!(
        dae.initialization.equations.len(),
        2,
        "one retained row per round: {:?}",
        dae.initialization
            .equations
            .iter()
            .map(|equation| refs_of(&equation.rhs))
            .collect::<Vec<_>>()
    );
    let position_form = &dae.initialization.equations[0];
    let velocity_form = &dae.initialization.equations[1];
    assert_eq!(
        format!("{:?}", position_form.rhs),
        format!("{:?}", index_three_chain_dae().continuous.equations[2].rhs),
        "round 0 retains the position constraint exactly as it was stated"
    );
    assert!(
        !position_form
            .origin
            .contains("d_dt_deficient_constraint_row"),
        "the position form is the row the pass found, not one it made: {}",
        position_form.origin
    );
    assert_eq!(
        refs_of(&velocity_form.rhs),
        vec!["v".to_string()],
        "round 1 retains the velocity form the first differentiation produced"
    );
    assert!(
        velocity_form
            .origin
            .contains("d_dt_deficient_constraint_row"),
        "the velocity form is a row this pass produced: {}",
        velocity_form.origin
    );
}

#[test]
fn a_derivative_that_folds_to_a_constant_is_not_a_prolonged_constraint() {
    // `row_is_vacuous` is what stands between "the constraint moved to velocity
    // level" and "the constraint was deleted and the row count did not notice".
    assert!(
        row_is_vacuous(&binary(OpBinary::Sub, literal(0.0), literal(0.0))),
        "a residual over no variables constrains nothing"
    );
    assert!(
        !row_is_vacuous(&binary(OpBinary::Sub, var("v"), literal(0.0))),
        "a residual that still reads a variable is a constraint"
    );
}

/// A model the rank witness cannot describe at all.
///
/// The only continuous variable is a parameter, so the view has no columns and
/// [`super::scalar_rank_view::build`] declines. That is *not* a perfect matching,
/// and the outcome must say so.
fn viewless_dae() -> Dae {
    let mut dae = Dae::new();
    dae.variables
        .parameters
        .insert(VarName::new("p"), continuous_variable("p"));
    dae.continuous.equations = vec![eq(binary(OpBinary::Sub, var("p"), literal(1.0)))];
    dae
}

#[test]
fn a_view_that_cannot_be_built_is_not_reported_as_a_perfect_matching() {
    assert!(
        matches!(
            nominate_deficient_rows(&viewless_dae()),
            RankOutcome::Indeterminate
        ),
        "a model with no columns yields no rank evidence in either direction"
    );
    let mut balanced = loop_closure_dae();
    balanced.continuous.equations[2] = eq(binary(OpBinary::Sub, var("v"), literal(1.0)));
    assert!(
        matches!(
            nominate_deficient_rows(&balanced),
            RankOutcome::Perfect { .. }
        ),
        "a matched view is the one outcome that means converged"
    );
    assert!(
        matches!(
            nominate_deficient_rows(&loop_closure_dae()),
            RankOutcome::Deficient(_)
        ),
        "an unmatched row is a deficient block"
    );
}

#[test]
fn only_a_perfect_matching_that_keeps_every_determined_column_is_retained() {
    let perfect = |determined_columns: Vec<bool>| Prolongation {
        rows: 1,
        outcome: ProlongationOutcome::Perfect { determined_columns },
    };
    assert!(
        perfect(vec![true, true]).is_accepted(&[true, false]),
        "a matching that determines more columns than it started with is retained"
    );
    assert!(
        !perfect(vec![false, true]).is_accepted(&[true, true]),
        "a column that lost its defining row reverts the whole iteration"
    );
    assert!(
        !perfect(vec![true]).is_accepted(&[true, true]),
        "matchings of different widths are not about the same system"
    );
    assert!(
        !Prolongation {
            rows: 1,
            outcome: ProlongationOutcome::Indeterminate,
        }
        .is_accepted(&[]),
        "an iteration whose rank witness declined proves nothing and is reverted"
    );
    assert!(
        !Prolongation {
            rows: 0,
            outcome: ProlongationOutcome::Perfect {
                determined_columns: vec![true],
            },
        }
        .is_accepted(&[true]),
        "an iteration that rewrote nothing has nothing to retain"
    );
}

#[test]
fn carrying_a_round_forward_merges_into_shell_zero_without_duplicating() {
    let mut nomination = Nomination {
        shells: vec![vec![3, 1], vec![7]],
        deficiency: 2,
        determined_columns: vec![true, false],
    };
    nomination.carry_forward(&[1, 5]);
    assert_eq!(
        nomination.shells[0],
        vec![1, 3, 5],
        "the previous round's rows join shell 0 exactly once"
    );
    assert_eq!(nomination.shells[1], vec![7], "deeper shells are untouched");
}

#[test]
fn carrying_nothing_forward_leaves_the_nomination_alone() {
    let mut nomination = Nomination {
        shells: vec![vec![2]],
        deficiency: 1,
        determined_columns: vec![true],
    };
    nomination.carry_forward(&[]);
    assert_eq!(nomination.shells, vec![vec![2]]);
}
