//! The one-direction torn tangent through committing sweeps: a complete
//! owner's sweeps give the program-by-program values bit for bit, a first
//! decline falls back, and every stop, failure or owner defect leaves the
//! evaluator's seed restored for the next point.

use rumoca_eval_solve::{
    DirectionCommit, EvalSolveError, RowEvalContext, TornTangentEvaluator, TornTangentJacobian,
};
use rumoca_ir_solve::TornTangentPlan;

use super::tangent_jacobian::{
    LOOPS, Random, Rows, lower_source, point, prepared_rows, random_point,
};

/// How the scripted owner departs from a complete sweep at sweep `at`.
#[derive(Clone, Copy, PartialEq)]
enum Fault {
    None,
    /// Decline sweep `at` before running anything.
    Decline {
        at: usize,
    },
    /// Report every output of position `position` of sweep `at` as NaN.
    NonFinite {
        at: usize,
        position: usize,
    },
    /// As `NonFinite`, then keep running and report success.
    IgnoredStop {
        at: usize,
        position: usize,
    },
    /// Fail before running position `position` of sweep `at`.
    Fail {
        at: usize,
        position: usize,
    },
    /// Run the first two programs of sweep `at` in swapped order.
    Swap {
        at: usize,
    },
    /// Leave the last program of sweep `at` unrun and report success.
    Truncate {
        at: usize,
    },
}

/// An owner that sweeps the JVP rows program by program as a native sweep
/// would, departing from that as its fault says.
struct ScriptedOwner<'a> {
    rows: &'a Rows<'a>,
    y: &'a [f64],
    fault: Fault,
    sweeps: Vec<Vec<usize>>,
    executed: usize,
}

impl ScriptedOwner<'_> {
    fn sweep(
        &mut self,
        programs: &[usize],
        seed: &mut [f64],
        commit: &mut DirectionCommit<'_>,
    ) -> Result<bool, EvalSolveError> {
        let at = self.sweeps.len();
        self.sweeps.push(programs.to_vec());
        if self.fault == (Fault::Decline { at }) {
            return Ok(false);
        }
        let mut order = programs.to_vec();
        if self.fault == (Fault::Swap { at }) {
            order.swap(0, 1);
        }
        if self.fault == (Fault::Truncate { at }) {
            order.pop();
        }
        let mut out = Vec::new();
        for (position, &program) in order.iter().enumerate() {
            self.run(at, position, program, seed, &mut out)?;
            let committed = commit(program, &out, seed);
            if committed.is_err() && self.fault == (Fault::IgnoredStop { at, position }) {
                continue;
            }
            committed?;
        }
        Ok(true)
    }

    fn run(
        &mut self,
        at: usize,
        position: usize,
        program: usize,
        seed: &[f64],
        out: &mut Vec<f64>,
    ) -> Result<(), EvalSolveError> {
        if self.fault == (Fault::Fail { at, position }) {
            return Err(scripted_failure());
        }
        self.rows
            .prepared_jvp
            .eval_row_outputs_unchecked_with_context(
                program,
                self.y,
                &self.rows.model.parameters,
                0.0,
                RowEvalContext {
                    seed: Some(seed),
                    ..self.rows.context()
                },
                out,
            )?;
        self.executed += 1;
        if matches!(self.fault, Fault::NonFinite { at: a, position: p } | Fault::IgnoredStop { at: a, position: p } if (a, p) == (at, position))
        {
            out.fill(f64::NAN);
        }
        Ok(())
    }
}

fn scripted_failure() -> EvalSolveError {
    EvalSolveError::InvalidRow {
        message: "scripted program failure".to_string(),
        span: None,
    }
}

/// One evaluation through a scripted owner: the result, the owner after it
/// and how often the per-program call was asked.
struct Outcome<'a> {
    result: Result<Option<TornTangentJacobian>, EvalSolveError>,
    owner: ScriptedOwner<'a>,
    calls: usize,
}

fn sweep_with<'a>(
    evaluator: &TornTangentEvaluator,
    rows: &'a Rows<'a>,
    y: &'a [f64],
    fault: Fault,
) -> Outcome<'a> {
    let mut owner = ScriptedOwner {
        rows,
        y,
        fault,
        sweeps: Vec::new(),
        executed: 0,
    };
    let mut calls = 0;
    let result = evaluator.eval_through_sweeps(
        point(rows, y),
        &mut |_, _, _| {
            calls += 1;
            Ok(false)
        },
        &mut |programs, seed, commit| owner.sweep(programs, seed, commit),
    );
    Outcome {
        result,
        owner,
        calls,
    }
}

fn bits(jacobian: &TornTangentJacobian) -> (Vec<u64>, Vec<u64>) {
    let bits = |values: &[f64]| values.iter().map(|value| value.to_bits()).collect();
    (bits(&jacobian.residual), bits(&jacobian.recovered))
}

/// A directional evaluator and its program-by-program reference at one point.
struct Restored<'a> {
    label: &'a str,
    evaluator: &'a TornTangentEvaluator,
    rows: &'a Rows<'a>,
    y: &'a [f64],
    reference: &'a TornTangentJacobian,
}

impl Restored<'_> {
    /// The error `fault` makes the evaluation report, after checking that
    /// the evaluator is restored for the next point.
    fn defect(&self, fault: Fault) -> String {
        let outcome = sweep_with(self.evaluator, self.rows, self.y, fault);
        self.restored();
        match outcome.result {
            Err(error) => error.to_string(),
            Ok(result) => panic!("{}: expected an error, got {result:?}", self.label),
        }
    }

    /// The next point evaluates as the program-by-program reference.
    fn restored(&self) {
        let again = sweep_with(self.evaluator, self.rows, self.y, Fault::None);
        let again = again.result.expect("a complete sweep").expect("finite");
        assert_eq!(
            bits(&again),
            bits(self.reference),
            "{}: the seed is restored after every return",
            self.label
        );
    }
}

/// Check every route of one directional torn block at one point.
fn check_block_routes(label: &str, rows: &Rows<'_>, plan: TornTangentPlan, y: &[f64]) {
    let evaluator = TornTangentEvaluator::new(plan, &rows.jvp).expect("prepare the plan");
    let reference = evaluator
        .eval(point(rows, y))
        .expect("evaluate program by program")
        .expect("finite coefficients at the sample point");
    let check = Restored {
        label,
        evaluator: &evaluator,
        rows,
        y,
        reference: &reference,
    };
    let heads: Vec<_> = evaluator
        .plan()
        .steps()
        .iter()
        .filter(|step| step.group > 0)
        .map(|step| step.source.program)
        .collect();
    assert!(!heads.is_empty(), "{label}: the block has causal steps");
    check_complete_route(&check, &heads);
    check_fallback_and_stop_routes(&check);
    check_defect_routes(&check, heads.len());
}

/// A complete owner answers every pass as one sweep with the reference values.
fn check_complete_route(check: &Restored<'_>, heads: &[usize]) {
    let label = check.label;
    let complete = sweep_with(check.evaluator, check.rows, check.y, Fault::None);
    let swept = complete.result.expect("a complete sweep").expect("finite");
    assert_eq!(bits(&swept), bits(check.reference), "{label}: swept values");
    assert_eq!(
        complete.calls, 0,
        "{label}: a complete owner is never asked per program"
    );
    let tears = check.evaluator.plan().tear_targets().len();
    assert_eq!(
        complete.owner.sweeps.len(),
        1 + tears,
        "{label}: one sweep per pass"
    );
    assert_eq!(
        complete.owner.sweeps[0], heads,
        "{label}: the coefficient sweep"
    );
    for column in &complete.owner.sweeps[1..] {
        assert_eq!(
            column[..heads.len()],
            heads[..],
            "{label}: steps lead each column"
        );
    }
    check.restored();
}

/// A first decline falls back program by program; a non-finite coefficient
/// stops the sweep and declines the torn solve without any replay.
fn check_fallback_and_stop_routes(check: &Restored<'_>) {
    let label = check.label;
    let declined = sweep_with(
        check.evaluator,
        check.rows,
        check.y,
        Fault::Decline { at: 0 },
    );
    let fallen_back = declined.result.expect("the fallback").expect("finite");
    assert_eq!(
        bits(&fallen_back),
        bits(check.reference),
        "{label}: fallback values"
    );
    assert!(
        declined.calls > 0,
        "{label}: a first decline falls back per program"
    );
    assert_eq!(declined.owner.executed, 0);
    assert_eq!(
        declined.owner.sweeps.len(),
        1,
        "{label}: no sweep after a decline"
    );
    check.restored();

    let stopped = sweep_with(
        check.evaluator,
        check.rows,
        check.y,
        Fault::NonFinite { at: 0, position: 0 },
    );
    assert_eq!(
        stopped.owner.executed, 1,
        "{label}: nothing runs after the stop"
    );
    assert_eq!(
        stopped.owner.sweeps.len(),
        1,
        "{label}: no column after the stop"
    );
    assert_eq!(stopped.calls, 0, "{label}: a stop is never replayed");
    assert!(
        matches!(stopped.result, Ok(None)),
        "{label}: a non-finite coefficient declines the torn solve"
    );
    check.restored();
}

/// Owner failures pass unchanged and owner defects are errors, each leaving
/// the evaluator restored.
fn check_defect_routes(check: &Restored<'_>, coefficient_programs: usize) {
    let failure = scripted_failure().to_string();
    for (fault, expected) in [
        (
            Fault::Decline { at: 1 },
            "declined after an earlier sweep ran",
        ),
        (
            Fault::IgnoredStop {
                at: 0,
                position: coefficient_programs - 1,
            },
            "continued after its commit stopped it",
        ),
        (Fault::Fail { at: 0, position: 0 }, failure.as_str()),
        (Fault::Fail { at: 1, position: 1 }, failure.as_str()),
        (Fault::Swap { at: 1 }, "committed outside its program order"),
        (
            Fault::Truncate { at: 1 },
            "ended without committing every program",
        ),
    ] {
        let reported = check.defect(fault);
        assert!(reported.contains(expected), "{}: {reported}", check.label);
        if expected == failure {
            assert_eq!(reported, failure, "{}: errors pass unchanged", check.label);
        }
    }
}

#[test]
fn torn_tangent_sweeps_equal_the_per_program_passes_on_every_route() {
    let model = lower_source(LOOPS, "TangentLoops", &[]);
    let rows = prepared_rows(&model);
    let y = random_point(&model, &mut Random(0x9e37_79b9_7f4a_7c15));
    let mut checked = 0;
    for (index, block) in model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .scalar_blocks()
        .iter()
        .enumerate()
    {
        let Some(tearing) = &block.tearing else {
            continue;
        };
        let plan = TornTangentPlan::derive_directional(tearing, &rows.jvp)
            .expect("the one-direction plan");
        check_block_routes(&format!("TangentLoops block {index}"), &rows, plan, &y);
        checked += 1;
    }
    assert_eq!(checked, 2);
}
