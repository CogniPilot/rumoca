//! The committing sweep of an algebraic projection block's split dependent
//! parts: one sweep answers the issued split programs in selection order, and
//! every decline or failure leaves the program-by-program pass's values, first
//! error and placements exactly as they were.

use super::*;
use std::cell::Cell;

/// Rows `y[a] - y[b] - p[0]` and `y[a] + y[b] + p[0]`; the parameter load is
/// invariant, so a block residual split divides the program.
fn split_pair(a: usize, b: usize, derivative: bool) -> Vec<solve::LinearOp> {
    use solve::{BinaryOp as B, LinearOp as L};
    if derivative {
        return vec![
            L::LoadSeed { dst: 0, index: a },
            L::LoadSeed { dst: 1, index: b },
            L::Binary {
                dst: 2,
                op: B::Sub,
                lhs: 0,
                rhs: 1,
            },
            L::Binary {
                dst: 3,
                op: B::Add,
                lhs: 0,
                rhs: 1,
            },
            L::StoreOutput { src: 2 },
            L::StoreOutput { src: 3 },
        ];
    }
    vec![
        L::LoadY { dst: 0, index: a },
        L::LoadY { dst: 1, index: b },
        L::LoadP { dst: 2, index: 0 },
        L::Binary {
            dst: 3,
            op: B::Sub,
            lhs: 0,
            rhs: 1,
        },
        L::Binary {
            dst: 4,
            op: B::Sub,
            lhs: 3,
            rhs: 2,
        },
        L::Binary {
            dst: 5,
            op: B::Add,
            lhs: 0,
            rhs: 1,
        },
        L::Binary {
            dst: 6,
            op: B::Add,
            lhs: 5,
            rhs: 2,
        },
        L::StoreOutput { src: 4 },
        L::StoreOutput { src: 6 },
    ]
}

/// Rows `y[a] - y[b]` and `y[a] + y[b]`: every operation reads a block
/// unknown, so no split divides the program.
fn unsplit_pair(a: usize, b: usize, derivative: bool) -> Vec<solve::LinearOp> {
    use solve::{BinaryOp as B, LinearOp as L};
    let load = |dst, index| {
        if derivative {
            L::LoadSeed { dst, index }
        } else {
            L::LoadY { dst, index }
        }
    };
    vec![
        load(0, a),
        load(1, b),
        L::Binary {
            dst: 2,
            op: B::Sub,
            lhs: 0,
            rhs: 1,
        },
        L::Binary {
            dst: 3,
            op: B::Add,
            lhs: 0,
            rhs: 1,
        },
        L::StoreOutput { src: 2 },
        L::StoreOutput { src: 3 },
    ]
}

/// Rows `y[b] - y[a]` when `0 < p[0]`, else `y[a] - y[b]`, and `y[a] + y[b]`:
/// the parameter condition is invariant and keeps the block affine, and the
/// select gives the program a lazy row plan.
fn lazy_split_pair(a: usize, b: usize, derivative: bool) -> Vec<solve::LinearOp> {
    use solve::{BinaryOp as B, CompareOp as C, LinearOp as L};
    let load = |dst, index| {
        if derivative {
            L::LoadSeed { dst, index }
        } else {
            L::LoadY { dst, index }
        }
    };
    vec![
        L::LoadP { dst: 0, index: 0 },
        L::Const { dst: 1, value: 0.0 },
        L::Compare {
            dst: 2,
            op: C::Lt,
            lhs: 1,
            rhs: 0,
        },
        load(3, a),
        load(4, b),
        L::Binary {
            dst: 5,
            op: B::Sub,
            lhs: 3,
            rhs: 4,
        },
        L::Binary {
            dst: 6,
            op: B::Sub,
            lhs: 4,
            rhs: 3,
        },
        L::Select {
            dst: 7,
            cond: 2,
            if_true: 6,
            if_false: 5,
        },
        L::Binary {
            dst: 8,
            op: B::Add,
            lhs: 3,
            rhs: 4,
        },
        L::StoreOutput { src: 7 },
        L::StoreOutput { src: 8 },
    ]
}

type Program = fn(usize, usize, bool) -> Vec<solve::LinearOp>;

/// A model of two-row programs over consecutive unknowns, with algebraic
/// projection blocks over the given `(start, end)` row ranges.
fn sweep_model(programs: &[Program], blocks: &[(usize, usize)]) -> solve::SolveModel {
    let rows = programs.len() * 2;
    let primal = |derivative| {
        programs
            .iter()
            .enumerate()
            .map(|(index, program)| program(2 * index, 2 * index + 1, derivative))
            .collect::<Vec<_>>()
    };
    let names = (0..rows).map(|index| format!("y{index}")).collect();
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            layout: crate::fixture_layout::named_y_layout(
                rumoca_ir_solve::VarLayout::from_parts(Default::default(), 0, 1),
                names,
            ),
            solve_layout: solve::SolveLayout {
                algebraic_scalar_count: rows,
                parameter_count: 1,
                compiled_parameter_len: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    primal(false),
                    "split_sweep.mo",
                )),
                implicit_row_targets: (0..rows)
                    .map(|row| Some(solve::scalar_slot_y(row)))
                    .collect::<Vec<_>>()
                    .into(),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: blocks
                        .iter()
                        .map(|&(start, end)| solve::AlgebraicProjectionBlock {
                            rows: (start..end).collect(),
                            y_indices: (start..end).collect(),
                            tearing: None,
                            alternate_charts: Vec::new(),
                        })
                        .collect::<Vec<_>>()
                        .into(),
                },
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0; rows].into(),
        ..Default::default()
    };
    set_test_implicit_jvp(&mut model, primal(true), "split_sweep_jvp.mo");
    derive_test_structural_artifacts(&mut model);
    model
}

/// Calls of the sweeping backend's compiled entries, and its faults.
#[derive(Default)]
struct Counts {
    sweeps: Cell<usize>,
    dependent_calls: Cell<usize>,
    whole_calls: Cell<usize>,
    no_sweep: Cell<bool>,
    /// Ordinal whose dependent part fails, and whether as a compiled
    /// execution error (otherwise an error the evaluation declines).
    fault: Cell<Option<(usize, bool)>>,
}

impl Counts {
    fn fault_at(&self, ordinal: usize) -> Result<(), crate::RuntimeSolveError> {
        match self.fault.get() {
            Some((at, compiled)) if at == ordinal && compiled => Err(
                crate::RuntimeSolveError::from("dependent part failed".to_string()),
            ),
            Some((at, _)) if at == ordinal => Err(crate::RuntimeSolveError::solve_ir(
                "dependent part declined",
            )),
            _ => Ok(()),
        }
    }
}

fn eval_program(
    block: &PreparedScalarProgramBlock,
    program: usize,
    (y, p, t): (&[f64], &[f64], f64),
    seed: Option<&[f64]>,
    tables: &[rumoca_core::ExternalTableData],
    out: &mut Vec<f64>,
) -> Result<(), crate::RuntimeSolveError> {
    block
        .eval_row_outputs_unchecked_with_context(
            program,
            y,
            p,
            t,
            RowEvalContext {
                seed,
                external_tables: Some(tables),
                ..Default::default()
            },
            out,
        )
        .map_err(Into::into)
}

/// A compiled expression that also answers single programs.
struct Programs {
    block: PreparedScalarProgramBlock,
    counts: Rc<Counts>,
}

impl CompiledSolveExpression for Programs {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError> {
        self.counts
            .whole_calls
            .set(self.counts.whole_calls.get() + 1);
        self.block
            .eval_with_context(
                y,
                p,
                t,
                RowEvalContext {
                    external_tables: Some(tables),
                    ..Default::default()
                },
                out,
            )
            .map_err(Into::into)
    }

    fn call_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, crate::RuntimeSolveError> {
        eval_program(&self.block, program, (y, p, t), None, tables, out)?;
        Ok(true)
    }
}

/// A compiled directional expression whose programs run one at a time or as
/// one committing sweep, with the configured fault in both.
struct Sweeping {
    block: PreparedScalarProgramBlock,
    counts: Rc<Counts>,
}

impl CompiledSolveJacobianExpression for Sweeping {
    fn programs_committing(self: Rc<Self>) -> Option<Rc<dyn crate::CompiledProgramsCommitting>> {
        (!self.counts.no_sweep.get()).then_some(self as Rc<_>)
    }

    fn call_program_outputs(
        &self,
        program: usize,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, crate::RuntimeSolveError> {
        let counts = &self.counts;
        counts.dependent_calls.set(counts.dependent_calls.get() + 1);
        counts.fault_at(program)?;
        let point = (inputs.y, inputs.p, inputs.t);
        eval_program(&self.block, program, point, Some(inputs.seed), tables, out)?;
        Ok(true)
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError> {
        self.block
            .eval_with_context(
                y,
                p,
                t,
                RowEvalContext {
                    seed: Some(seed),
                    external_tables: Some(tables),
                    ..Default::default()
                },
                out,
            )
            .map_err(Into::into)
    }
}

impl crate::CompiledProgramsCommitting for Sweeping {
    fn call_selected_programs_committing(
        &self,
        programs: &[usize],
        point: (&[f64], &[f64], f64),
        seed: &mut [f64],
        tables: &[rumoca_core::ExternalTableData],
        commit: &mut crate::ProgramOutputCommit<'_>,
    ) -> Result<(), crate::RuntimeSolveError> {
        self.counts.sweeps.set(self.counts.sweeps.get() + 1);
        let mut staged = Vec::new();
        for &program in programs {
            self.counts.fault_at(program)?;
            eval_program(&self.block, program, point, Some(seed), tables, &mut staged)?;
            commit(program, &staged, seed)?;
        }
        Ok(())
    }
}

struct SweepingBackend(Rc<Counts>);

impl SolveExecutionBackend for SweepingBackend {
    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, crate::CompileRefusal> {
        Ok(Rc::new(Programs {
            block: PreparedScalarProgramBlock::new(source.clone())
                .map_err(|error| error.to_string())?,
            counts: Rc::clone(&self.0),
        }))
    }

    fn compile_jacobian_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, crate::CompileRefusal> {
        Ok(Rc::new(Sweeping {
            block: PreparedScalarProgramBlock::new(source.clone())
                .map_err(|error| error.to_string())?,
            counts: Rc::clone(&self.0),
        }))
    }

    fn compile_assignment_schedule(
        &self,
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        super::super::test_kernels::assignment(source, owners, schedule)
    }
}

const Y: [f64; 8] = [1.0, 2.0, 3.0, 5.0, 7.0, 11.0, 13.0, 17.0];
const P: [f64; 1] = [0.5];

fn sweep_runtime(model: &solve::SolveModel, counts: &Rc<Counts>) -> SolveRuntime {
    SolveRuntime::new_fixture_with_backend(model, Rc::new(SweepingBackend(Rc::clone(counts))))
        .unwrap()
}

/// Two fully split programs in block 0 (rows 0-3) and one more in block 1
/// (rows 4-5).
fn two_block_model() -> solve::SolveModel {
    sweep_model(&[split_pair, split_pair, split_pair], &[(0, 4), (4, 6)])
}

fn begin_split(runtime: &SolveRuntime, block: usize) {
    runtime
        .scalar_execution()
        .unwrap()
        .begin_block_residual_split(block, &Y, &P, 0.0)
        .unwrap();
}

fn block_values(runtime: &SolveRuntime, block: usize) -> Vec<f64> {
    runtime.scalar_execution().unwrap().scalar.block_splits[block]
        .as_ref()
        .unwrap()
        .values_snapshot()
}

/// One residual pass of `block` through its issued residual selection.
fn residual_pass(
    runtime: &SolveRuntime,
    block: usize,
    rows: usize,
) -> Result<Vec<f64>, crate::RuntimeSolveError> {
    let projection = super::native_projection_jvp::selected_projection(runtime, false);
    assert!(projection.algebraic_projection_block_is_affine(block));
    let selection = runtime
        .scalar_execution()
        .unwrap()
        .scalar
        .continuous_structural
        .algebraic_projection()[block]
        .residual_output_evaluation()
        .unwrap();
    let mut out = vec![0.0; rows];
    assert!(projection.eval_implicit_residual_outputs(block, selection, &Y, &P, 0.0, &mut out)?);
    Ok(out)
}

fn reset(counts: &Counts) {
    for count in [&counts.sweeps, &counts.dependent_calls, &counts.whole_calls] {
        count.set(0);
    }
}

fn bits(values: &[f64]) -> Vec<u64> {
    values.iter().map(|value| value.to_bits()).collect()
}

/// `y0 - y1 - p0, y0 + y1 + p0, y2 - y3 - p0, y2 + y3 + p0` at `Y`, `P`.
const BLOCK0: [f64; 4] = [-1.5, 3.5, -2.5, 8.5];

/// Per-program dependent calls the debug-only bit-compare adds after a swept
/// pass of `programs` programs.
fn compare_calls(programs: usize) -> usize {
    if cfg!(debug_assertions) { programs } else { 0 }
}

#[test]
fn one_sweep_answers_a_fully_split_selection_and_leaves_the_seed() {
    let counts = Rc::new(Counts::default());
    let runtime = sweep_runtime(&two_block_model(), &counts);
    let scalar = &runtime.scalar_execution().unwrap().scalar;
    let split = scalar.continuous_structural.algebraic_projection()[0]
        .split_residual_evaluation()
        .expect("both programs of block 0 are split");
    assert_eq!(split.selection().programs().len(), 2);
    assert!(scalar.split_sweeps[0].is_some());
    begin_split(&runtime, 0);
    let seed = block_values(&runtime, 0);
    reset(&counts);
    let out = residual_pass(&runtime, 0, 4).unwrap();
    assert_eq!(bits(&out), bits(&BLOCK0));
    assert_eq!(counts.sweeps.get(), 1);
    assert_eq!(counts.dependent_calls.get(), compare_calls(2));
    assert_eq!(
        bits(&block_values(&runtime, 0)),
        bits(&seed),
        "the sweep never writes the seed"
    );
}

#[test]
fn the_sweep_declines_without_this_blocks_active_split() {
    let counts = Rc::new(Counts::default());
    let runtime = sweep_runtime(&two_block_model(), &counts);
    reset(&counts);
    let out = residual_pass(&runtime, 0, 4).unwrap();
    assert_eq!(
        bits(&out),
        bits(&BLOCK0),
        "no active split: the unsplit programs"
    );
    assert_eq!(counts.sweeps.get(), 0);
    begin_split(&runtime, 1);
    reset(&counts);
    let out = residual_pass(&runtime, 0, 4).unwrap();
    assert_eq!(bits(&out), bits(&BLOCK0), "another block's split is active");
    assert_eq!(counts.sweeps.get(), 0);
    assert_eq!(
        counts.dependent_calls.get(),
        0,
        "block 1's split has no ordinal here"
    );
}

#[test]
fn the_sweep_declines_while_the_split_values_are_held() {
    let counts = Rc::new(Counts::default());
    let runtime = sweep_runtime(&two_block_model(), &counts);
    begin_split(&runtime, 0);
    let splits = runtime.scalar_execution().unwrap().scalar.block_splits[0]
        .as_ref()
        .unwrap();
    let held = splits.hold_values();
    reset(&counts);
    let out = residual_pass(&runtime, 0, 4).unwrap();
    drop(held);
    assert_eq!(bits(&out), bits(&BLOCK0));
    assert_eq!(counts.sweeps.get(), 0);
    assert_eq!(
        counts.dependent_calls.get(),
        2,
        "the program-by-program pass"
    );
}

#[test]
fn a_compiled_failure_stops_the_sweep_with_the_earlier_program_placed() {
    let counts = Rc::new(Counts::default());
    let runtime = sweep_runtime(&two_block_model(), &counts);
    begin_split(&runtime, 0);
    let seed = block_values(&runtime, 0);
    counts.fault.set(Some((1, true)));
    reset(&counts);
    let projection = super::native_projection_jvp::selected_projection(&runtime, false);
    let selection = runtime
        .scalar_execution()
        .unwrap()
        .scalar
        .continuous_structural
        .algebraic_projection()[0]
        .residual_output_evaluation()
        .unwrap();
    let mut out = [f64::NAN; 4];
    let error = projection
        .eval_implicit_residual_outputs(0, selection, &Y, &P, 0.0, &mut out)
        .unwrap_err();
    assert!(matches!(
        error,
        crate::RuntimeSolveError::CompiledExecution { .. }
    ));
    assert_eq!(
        bits(&out[..2]),
        bits(&BLOCK0[..2]),
        "program 0 committed first"
    );
    assert!(
        out[2].is_nan() && out[3].is_nan(),
        "the failing program places nothing"
    );
    assert_eq!(counts.sweeps.get(), 1);
    assert_eq!(
        counts.dependent_calls.get(),
        0,
        "no program-by-program retry"
    );
    assert_eq!(bits(&block_values(&runtime, 0)), bits(&seed));
}

#[test]
fn a_declined_dependent_part_resumes_program_by_program_from_it() {
    let counts = Rc::new(Counts::default());
    let runtime = sweep_runtime(&two_block_model(), &counts);
    begin_split(&runtime, 0);
    let seed = block_values(&runtime, 0);
    counts.fault.set(Some((1, false)));
    reset(&counts);
    let out = residual_pass(&runtime, 0, 4).unwrap();
    assert_eq!(
        bits(&out),
        bits(&BLOCK0),
        "program 1 falls back to its unsplit program"
    );
    assert_eq!(counts.sweeps.get(), 1);
    // Program 1 alone retries its dependent part, which declines again; the
    // debug-only compare re-runs both.
    assert_eq!(counts.dependent_calls.get(), 1 + compare_calls(2));
    assert_eq!(bits(&block_values(&runtime, 0)), bits(&seed));
}

#[test]
fn no_sweep_is_bound_without_the_owners_sweep_or_for_other_programs() {
    let counts = Rc::new(Counts::default());
    counts.no_sweep.set(true);
    let runtime = sweep_runtime(&two_block_model(), &counts);
    let scalar = &runtime.scalar_execution().unwrap().scalar;
    assert!(scalar.split_sweeps[0].is_none());
    begin_split(&runtime, 0);
    reset(&counts);
    let out = residual_pass(&runtime, 0, 4).unwrap();
    assert_eq!(bits(&out), bits(&BLOCK0));
    assert_eq!(counts.dependent_calls.get(), 2);
    counts.no_sweep.set(false);
    let runtime = sweep_runtime(&two_block_model(), &counts);
    let scalar = &runtime.scalar_execution().unwrap().scalar;
    let structures = scalar.continuous_structural.algebraic_projection();
    let other = spanned_block(
        (0..3)
            .map(|index| split_pair(2 * index, 2 * index + 1, false))
            .collect(),
        "split_sweep.mo",
    );
    let canonical = structures[0]
        .split_residual_evaluation()
        .unwrap()
        .primal_source()
        .clone();
    let issued = super::super::block_residual_split::split_sweeps(
        structures,
        &scalar.block_splits,
        &canonical,
    );
    assert!(issued[0].is_some(), "the issuing programs get the sweep");
    let foreign =
        super::super::block_residual_split::split_sweeps(structures, &scalar.block_splits, &other);
    assert!(
        foreign[0].is_none(),
        "equal but separately owned programs do not"
    );
}

#[test]
fn a_lazily_planned_split_program_shifts_the_ordinals_and_binds_no_sweep() {
    let counts = Rc::new(Counts::default());
    let model = sweep_model(&[lazy_split_pair, split_pair, split_pair], &[(0, 6)]);
    let runtime = sweep_runtime(&model, &counts);
    let scalar = &runtime.scalar_execution().unwrap().scalar;
    assert!(scalar.implicit_scalar_rhs.source().has_lazy_row_plan(0));
    let split = scalar.continuous_structural.algebraic_projection()[0]
        .split_residual_evaluation()
        .unwrap();
    let programs: Vec<_> = split
        .selection()
        .programs()
        .iter()
        .map(solve::ProjectionProgramOutputs::program)
        .collect();
    assert_eq!(programs, [0, 1, 2], "Solve IR issues every split program");
    assert!(
        scalar.split_sweeps[0].is_none(),
        "the runtime keeps program 0 unsplit, so its ordinals differ"
    );
}

#[test]
fn the_fused_entry_and_one_sweep_answer_a_mixed_selection() {
    let counts = Rc::new(Counts::default());
    let model = sweep_model(
        &[unsplit_pair, split_pair, unsplit_pair, split_pair],
        &[(0, 8)],
    );
    let runtime = sweep_runtime(&model, &counts);
    let scalar = &runtime.scalar_execution().unwrap().scalar;
    assert!(scalar.fused_residuals[0].is_some());
    assert!(scalar.split_sweeps[0].is_some());
    begin_split(&runtime, 0);
    reset(&counts);
    let out = residual_pass(&runtime, 0, 8).unwrap();
    // y0 - y1, y0 + y1, y2 - y3 - p0, y2 + y3 + p0, then the same for y4..y7.
    let expected = [-1.0, 3.0, -2.5, 8.5, -4.0, 18.0, -4.5, 30.5];
    assert_eq!(bits(&out), bits(&expected));
    assert_eq!(counts.whole_calls.get(), 1, "one fused entry call");
    assert_eq!(counts.sweeps.get(), 1, "one sweep of both split programs");
    assert_eq!(counts.dependent_calls.get(), compare_calls(2));
}

#[test]
fn the_sweeping_owners_whole_entry_matches_its_programs_one_at_a_time() {
    let counts = Rc::new(Counts::default());
    let block = spanned_block(
        vec![split_pair(0, 1, true), split_pair(2, 3, true)],
        "split_sweep_jvp.mo",
    );
    let owner = SweepingBackend(Rc::clone(&counts))
        .compile_jacobian_expression(&block)
        .unwrap();
    let seed = [1.0, 2.0, 3.0, 5.0];
    let mut whole = [0.0; 4];
    owner.call(&Y, &P, 0.0, &seed, &[], &mut whole).unwrap();
    let inputs = solve_eval::JacobianEvalInputs {
        y: &Y,
        p: &P,
        t: 0.0,
        seed: &seed,
    };
    let mut programs = Vec::new();
    let mut out = Vec::new();
    for program in 0..2 {
        assert!(
            owner
                .call_program_outputs(program, inputs, &[], &mut out)
                .unwrap()
        );
        programs.extend_from_slice(&out);
    }
    // v0 - v1, v0 + v1, v2 - v3, v2 + v3.
    assert_eq!(bits(&whole), bits(&[-1.0, 3.0, -2.0, 8.0]));
    assert_eq!(bits(&whole), bits(&programs));
}
