//! A torn block's tangent issued as one assignment-row schedule runs as one
//! module call and reproduces the per-row tangent bit for bit: its Jacobian,
//! its stop at a non-finite coefficient, and its fault.
use super::*;
use rumoca_eval_solve::{
    EvalSolveError, RowEvalContext, TangentPoint, TornTangentEvaluator, TornTangentJacobian,
    torn_tangent_schedule_outcome,
};
use solve::{BinaryOp, CompareOp, LinearOp as L};

fn binary(dst: u32, op: BinaryOp, lhs: u32, rhs: u32) -> L {
    L::Binary { dst, op, lhs, rhs }
}

const Y: usize = 6;

/// Tangents of a torn block over Y = (tears y0, y1; steps y2, y3, y4; y5
/// outside the block), one output per implicit row:
///
/// * row 0, `2 y2 - y0^2`: `2 d2 - 2 (y0 d0)` (a literal coefficient), read
///   through a dual tensor load;
/// * row 1, `y3 - p0 y1 - max(0, y2)`: `d3 - p0 d1 - (y2 > 0 ? d2 : 0)`;
/// * row 2, `(1 + 0 y5) y4 - y2^2`: `(1 + 0 y5) d4 + y4 (0 d5) - 2 y2 d2`,
///   a coefficient that is not finite when `y5` is infinite;
/// * row 3, `y4 - y0 + y3` (with `faulting`, plus `0 * z` for the 1x1 dense
///   solve `p2 z = 1`, which faults at `p2 = 0`);
/// * row 4, `y1 - p1`: `d1`, structurally zero in the first tear column.
///
/// Rows 1 and 2 share one program (one step group), rows 3 and 4 another
/// (one residual group).
fn jvp(faulting: bool) -> solve::ScalarProgramBlock {
    // Row 0 reads its point and seed through one dual tensor load:
    // registers 0..6 hold (y0, d0, y1, d1, y2, d2).
    let row0 = vec![
        L::TensorLoad {
            dst_start: 0,
            input: solve::TensorInputKind::Y,
            input_start: 0,
            count: 3,
            seed_start: Some(0),
            lanes: 2,
        },
        L::Const { dst: 6, value: 2.0 },
        binary(7, BinaryOp::Mul, 6, 5),
        binary(8, BinaryOp::Mul, 0, 1),
        binary(9, BinaryOp::Mul, 6, 8),
        binary(10, BinaryOp::Sub, 7, 9),
        L::StoreOutput { src: 10 },
    ];
    let rows12 = vec![
        L::LoadSeed { dst: 0, index: 3 },
        L::LoadP { dst: 1, index: 0 },
        L::LoadSeed { dst: 2, index: 1 },
        binary(3, BinaryOp::Mul, 1, 2),
        binary(4, BinaryOp::Sub, 0, 3),
        L::LoadY { dst: 5, index: 2 },
        L::Const { dst: 6, value: 0.0 },
        L::Compare {
            dst: 7,
            op: CompareOp::Gt,
            lhs: 5,
            rhs: 6,
        },
        L::LoadSeed { dst: 8, index: 2 },
        L::Select {
            dst: 9,
            cond: 7,
            if_true: 8,
            if_false: 6,
        },
        binary(10, BinaryOp::Sub, 4, 9),
        L::StoreOutput { src: 10 },
        L::Const {
            dst: 11,
            value: 1.0,
        },
        L::LoadY { dst: 12, index: 5 },
        binary(13, BinaryOp::Mul, 6, 12),
        binary(14, BinaryOp::Add, 11, 13),
        L::LoadSeed { dst: 15, index: 4 },
        binary(16, BinaryOp::Mul, 14, 15),
        L::LoadY { dst: 17, index: 4 },
        L::LoadSeed { dst: 18, index: 5 },
        binary(19, BinaryOp::Mul, 6, 18),
        binary(20, BinaryOp::Mul, 17, 19),
        binary(21, BinaryOp::Add, 16, 20),
        L::Const {
            dst: 22,
            value: 2.0,
        },
        binary(23, BinaryOp::Mul, 22, 5),
        binary(24, BinaryOp::Mul, 23, 8),
        binary(25, BinaryOp::Sub, 21, 24),
        L::StoreOutput { src: 25 },
    ];
    let mut rows34 = vec![
        L::LoadSeed { dst: 0, index: 4 },
        L::LoadSeed { dst: 1, index: 0 },
        binary(2, BinaryOp::Sub, 0, 1),
        L::LoadSeed { dst: 3, index: 3 },
        binary(4, BinaryOp::Add, 2, 3),
    ];
    if faulting {
        rows34.extend([
            L::LoadP { dst: 5, index: 2 },
            L::Const { dst: 6, value: 1.0 },
            L::LinearSolveComponent {
                dst: 7,
                matrix_start: 5,
                rhs_start: 6,
                n: 1,
                component: 0,
            },
            L::Const { dst: 8, value: 0.0 },
            binary(9, BinaryOp::Mul, 7, 8),
            binary(10, BinaryOp::Add, 4, 9),
            L::StoreOutput { src: 10 },
        ]);
    } else {
        rows34.push(L::StoreOutput { src: 4 });
    }
    rows34.extend([
        L::LoadSeed { dst: 11, index: 1 },
        L::StoreOutput { src: 11 },
    ]);
    solve::ScalarProgramBlock::with_program_spans(
        vec![row0, rows12, rows34],
        (0..3).map(fixture::span).collect(),
    )
    .unwrap()
}

fn tearing() -> solve::BlockTearing {
    solve::BlockTearing {
        tear_y_indices: vec![0, 1],
        residual_rows: vec![3, 4],
        causal_steps: [(0, 2), (1, 3), (2, 4)]
            .into_iter()
            .map(|(row, y_index)| solve::CausalStep {
                row,
                y_index,
                ..Default::default()
            })
            .collect(),
    }
}

struct Case {
    jvp: solve::ScalarProgramBlock,
    plan: solve::TornTangentPlan,
    schedule: solve::TornTangentSchedule,
    runner: runner::Runner,
}

impl Case {
    fn new(faulting: bool) -> Self {
        let jvp = jvp(faulting);
        let plan = solve::TornTangentPlan::derive_directional(&tearing(), &jvp).unwrap();
        let schedule = plan.issue_schedule(&jvp, Y).unwrap();
        let layout = solve::VarLayout::from_parts(Default::default(), schedule.slots().total, 3);
        let compiled = rumoca_exec_wasm::compile_issued_assignment_rows_wasm(
            schedule.rows(),
            &layout,
            &solve::SolvePureCallTable::default(),
        )
        .unwrap();
        Self {
            runner: runner::Runner::new(&compiled, &layout),
            jvp,
            plan,
            schedule,
        }
    }

    /// The per-row tangent: the one-direction evaluator over the JVP rows.
    fn per_row(&self, y: &[f64], p: &[f64]) -> Result<Option<TornTangentJacobian>, EvalSolveError> {
        TornTangentEvaluator::new(self.plan.clone(), &self.jvp)
            .unwrap()
            .eval(TangentPoint {
                y,
                p,
                t: 0.0,
                context: RowEvalContext::default(),
            })
    }

    /// One module call over the issued slots, answered by the host contract.
    fn batched(
        &mut self,
        y: &[f64],
        p: &[f64],
    ) -> (i32, Result<Option<TornTangentJacobian>, EvalSolveError>) {
        let mut slots = vec![0.0; self.schedule.slots().total];
        slots[..Y].copy_from_slice(y);
        let (status, after) = self.runner.run(&slots, p);
        assert_eq!(
            runner::bytes(&after[..Y]),
            runner::bytes(y),
            "the schedule never writes the point"
        );
        let called = if status == 0 {
            Ok(())
        } else {
            Err(EvalSolveError::InvalidRow {
                message: format!("schedule status {status}"),
                span: None,
            })
        };
        (
            status,
            torn_tangent_schedule_outcome(&self.schedule, &after, called),
        )
    }
}

fn bits(jacobian: &TornTangentJacobian) -> (Vec<u8>, Vec<u8>) {
    (
        runner::bytes(&jacobian.residual),
        runner::bytes(&jacobian.recovered),
    )
}

fn points() -> Vec<([f64; Y], [f64; 3])> {
    let mut points = Vec::new();
    for y0 in [0.0, -0.0, 0.75, -2.5, f64::MIN_POSITIVE] {
        for y2 in [1.5, -1.5, 0.0, -0.0] {
            for p0 in [0.5, -3.0] {
                points.push(([y0, -1.25, y2, 4.0, -0.5, 7.0], [p0, 0.25, 2.0]));
            }
        }
    }
    points
}

#[test]
fn the_issued_tangent_schedule_has_the_per_row_jacobian_bits() {
    let mut case = Case::new(false);
    let groups = case
        .plan
        .steps()
        .iter()
        .filter(|step| step.group > 0)
        .count();
    assert_eq!(groups, 2, "rows 1 and 2 form one step group");
    // One leading seed row, the coefficient groups, then each tear column's
    // step groups and its one residual group.
    assert_eq!(
        case.schedule.rows().rows().programs().len(),
        1 + groups + 2 * (groups + 1)
    );
    for (y, p) in points() {
        let expected = case.per_row(&y, &p).unwrap().unwrap();
        let (status, actual) = case.batched(&y, &p);
        assert_eq!(status, 0);
        let actual = actual.unwrap().unwrap();
        assert_eq!(bits(&actual), bits(&expected), "y = {y:?}, p = {p:?}");
        // Step y2 reads no tangent of the second tear: -0 / 2 in every point.
        assert_eq!(actual.recovered[1].to_bits(), (-0.0f64).to_bits());
        // Row 4 reads no tangent of the first tear.
        assert_eq!(actual.residual[2].to_bits(), 0.0f64.to_bits());
    }
}

#[test]
fn a_non_finite_coefficient_stops_like_the_per_row_tangent() {
    let mut case = Case::new(false);
    for y5 in [f64::INFINITY, f64::NEG_INFINITY, f64::NAN] {
        let y = [0.75, -1.25, 1.5, 4.0, -0.5, y5];
        let p = [0.5, 0.25, 2.0];
        assert!(case.per_row(&y, &p).unwrap().is_none());
        let (status, actual) = case.batched(&y, &p);
        assert_eq!(status, 0);
        assert!(actual.unwrap().is_none());
    }
}

#[test]
fn a_fault_is_returned_unless_a_committed_coefficient_stopped_first() {
    let mut case = Case::new(true);
    let p = [0.5, 0.25, 0.0];
    // All coefficients finite: the per-row tangent reaches the faulting row.
    let y = [0.75, -1.25, 1.5, 4.0, -0.5, 7.0];
    assert!(matches!(
        case.per_row(&y, &p),
        Err(EvalSolveError::LinearSolve { .. })
    ));
    let (status, actual) = case.batched(&y, &p);
    assert_ne!(status, 0);
    assert!(actual.is_err());
    // A non-finite coefficient: the per-row tangent stops before that row.
    let y = [0.75, -1.25, 1.5, 4.0, -0.5, f64::INFINITY];
    assert!(case.per_row(&y, &p).unwrap().is_none());
    let (status, actual) = case.batched(&y, &p);
    assert_ne!(status, 0);
    assert!(actual.unwrap().is_none());
    // A regular point of the same rows is still exact.
    let (y, p) = ([0.75, -1.25, 1.5, 4.0, -0.5, 7.0], [0.5, 0.25, 2.0]);
    let expected = case.per_row(&y, &p);
    let (status, actual) = case.batched(&y, &p);
    assert_eq!(status, 0);
    assert_eq!(
        bits(&actual.unwrap().unwrap()),
        bits(&expected.unwrap().unwrap())
    );
}

#[test]
fn the_issuer_refuses_a_multi_lane_plan_and_missing_programs() {
    let jvp = jvp(false);
    let lanes = solve::TornTangentPlan::derive(&tearing(), &jvp).unwrap();
    if !lanes.directional() {
        assert!(matches!(
            lanes.issue_schedule(&jvp, Y),
            Err(solve::TornTangentScheduleRefusal::NotDirectional)
        ));
    }
    let directional = solve::TornTangentPlan::derive_directional(&tearing(), &jvp).unwrap();
    assert!(matches!(
        directional.issue_schedule(&solve::ScalarProgramBlock::default(), Y),
        Err(solve::TornTangentScheduleRefusal::MissingProgram { .. })
    ));
}

/// The host reads a Jacobian only from slots a call left in the issued shape:
/// another slot extent, a progress that is not a committed group count, and a
/// successful return before every coefficient committed are each the typed
/// defect; a committed coefficient that is not finite still stops first.
#[test]
fn a_call_that_leaves_malformed_slots_is_a_typed_schedule_defect() {
    let mut case = Case::new(false);
    let (y, p) = ([0.75, -1.25, 1.5, 4.0, -0.5, 7.0], [0.5, 0.25, 2.0]);
    let layout = case.schedule.slots();
    let mut slots = vec![0.0; layout.total];
    slots[..Y].copy_from_slice(&y);
    let (status, after) = case.runner.run(&slots, &p);
    assert_eq!(status, 0);
    let outcome = |slots: &[f64]| {
        torn_tangent_schedule_outcome::<EvalSolveError>(&case.schedule, slots, Ok(()))
    };
    let defect = |slots: &[f64]| match outcome(slots) {
        Err(EvalSolveError::InvalidRow {
            message,
            span: None,
        }) => message,
        other => panic!("expected a schedule defect, got {other:?}"),
    };
    let expected = case.per_row(&y, &p).unwrap().unwrap();
    assert_eq!(bits(&outcome(&after).unwrap().unwrap()), bits(&expected));

    assert_eq!(
        defect(&after[..layout.total - 1]),
        "torn tangent schedule slots differ from the issued layout"
    );
    let committed = after[layout.progress];
    assert!(committed >= 2.0, "every group committed: {committed}");
    for progress in [0.5, -1.0, f64::NAN, committed + 1.0] {
        let mut malformed = after.clone();
        malformed[layout.progress] = progress;
        assert_eq!(
            defect(&malformed),
            "torn tangent schedule progress is not a committed group count",
            "progress {progress}"
        );
    }
    let mut early = after.clone();
    early[layout.progress] = 1.0;
    assert_eq!(
        defect(&early),
        "torn tangent schedule returned before committing every coefficient"
    );
    early[layout.coefficient] = f64::NAN;
    assert!(outcome(&early).unwrap().is_none());
}
