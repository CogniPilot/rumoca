//! The fused residual entry of an algebraic projection block whose residual
//! rows come from two-output programs: one entry answers the unsplit
//! programs, the split program keeps its own evaluation, and every decline
//! leaves the program-by-program pass exactly as it was.

use super::*;
use std::cell::Cell;

/// Rows `2k, 2k + 1` are `y[a] - y[b]` and `y[a] + y[b]`.
fn pair(a: usize, b: usize, derivative: bool) -> Vec<solve::LinearOp> {
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
        L::StoreOutputRange {
            start: 2,
            count: 2,
            stride: 1,
        },
    ]
}

/// Row 4 is `y[4] - p[0]`; its parameter load is invariant, so a block
/// residual split divides it.
fn single(derivative: bool) -> Vec<solve::LinearOp> {
    use solve::{BinaryOp as B, LinearOp as L};
    if derivative {
        return vec![L::LoadSeed { dst: 0, index: 4 }, L::StoreOutput { src: 0 }];
    }
    vec![
        L::LoadY { dst: 0, index: 4 },
        L::LoadP { dst: 1, index: 0 },
        L::Binary {
            dst: 2,
            op: B::Sub,
            lhs: 0,
            rhs: 1,
        },
        L::StoreOutput { src: 2 },
    ]
}

/// Rows 2 and 3 as `|y[2] - y[3]|` through a select, and `y[2] + y[3]`:
/// every operation still reads a block unknown, and the select gives the
/// program a lazy row plan.
fn select_pair() -> Vec<solve::LinearOp> {
    use solve::{BinaryOp as B, CompareOp as C, LinearOp as L};
    vec![
        L::LoadY { dst: 0, index: 2 },
        L::LoadY { dst: 1, index: 3 },
        L::Compare {
            dst: 2,
            op: C::Lt,
            lhs: 0,
            rhs: 1,
        },
        L::Binary {
            dst: 3,
            op: B::Sub,
            lhs: 0,
            rhs: 1,
        },
        L::Binary {
            dst: 4,
            op: B::Sub,
            lhs: 1,
            rhs: 0,
        },
        L::Select {
            dst: 5,
            cond: 2,
            if_true: 4,
            if_false: 3,
        },
        L::Binary {
            dst: 6,
            op: B::Add,
            lhs: 0,
            rhs: 1,
        },
        L::StoreOutput { src: 5 },
        L::StoreOutput { src: 6 },
    ]
}

/// Rows 2 and 3 as `y[3] - y[2]` when `0 < p[0]`, else `y[2] - y[3]`,
/// through a select, and `y[2] + y[3]`. The select reads only a parameter
/// condition, so the block stays affine; every register the condition is
/// built in is written again later, so no operation can be hoisted and the
/// program has no split.
fn affine_select_pair() -> Vec<solve::LinearOp> {
    use solve::{BinaryOp as B, CompareOp as C, LinearOp as L};
    vec![
        L::LoadP { dst: 0, index: 0 },
        L::Const { dst: 1, value: 0.0 },
        L::Compare {
            dst: 2,
            op: C::Lt,
            lhs: 1,
            rhs: 0,
        },
        L::LoadY { dst: 0, index: 2 },
        L::LoadY { dst: 1, index: 3 },
        L::Binary {
            dst: 3,
            op: B::Sub,
            lhs: 0,
            rhs: 1,
        },
        L::Binary {
            dst: 4,
            op: B::Sub,
            lhs: 1,
            rhs: 0,
        },
        L::Select {
            dst: 5,
            cond: 2,
            if_true: 4,
            if_false: 3,
        },
        L::Binary {
            dst: 2,
            op: B::Add,
            lhs: 0,
            rhs: 1,
        },
        L::StoreOutput { src: 5 },
        L::StoreOutput { src: 2 },
    ]
}

fn rows(derivative: bool) -> Vec<Vec<solve::LinearOp>> {
    vec![
        pair(0, 1, derivative),
        pair(2, 3, derivative),
        single(derivative),
    ]
}

fn fused_model() -> solve::SolveModel {
    fused_model_with(rows(false))
}

fn fused_model_with(primal: Vec<Vec<solve::LinearOp>>) -> solve::SolveModel {
    let names = (0..5).map(|index| format!("y{index}")).collect();
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            layout: crate::fixture_layout::named_y_layout(
                rumoca_ir_solve::VarLayout::from_parts(Default::default(), 0, 1),
                names,
            ),
            solve_layout: solve::SolveLayout {
                algebraic_scalar_count: 5,
                parameter_count: 1,
                compiled_parameter_len: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    primal,
                    "fused_residual.mo",
                )),
                implicit_row_targets: (0..5)
                    .map(|row| Some(solve::scalar_slot_y(row)))
                    .collect::<Vec<_>>()
                    .into(),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: (vec![solve::AlgebraicProjectionBlock {
                        rows: (0..5).collect(),
                        y_indices: (0..5).collect(),
                        tearing: None,
                        alternate_charts: Vec::new(),
                    }])
                    .into(),
                },
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0; 5].into(),
        ..Default::default()
    };
    set_test_implicit_jvp(&mut model, rows(true), "fused_residual_jvp.mo");
    derive_test_structural_artifacts(&mut model);
    model
}

/// What a compiled block of the fixture is, by its shape.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Shape {
    Fused,
    Pair,
    Single,
    Other,
}

/// Counts and faults of the counting backend's compiled residual entries.
#[derive(Default)]
struct Counts {
    fused: Cell<usize>,
    pair: Cell<usize>,
    single: Cell<usize>,
    fail_fused: Cell<bool>,
    nan_fused: Cell<bool>,
    fail_pair: Cell<bool>,
    fail_single: Cell<bool>,
}

struct Counted {
    inner: Rc<dyn CompiledSolveExpression>,
    shape: Shape,
    counts: Rc<Counts>,
}

impl CompiledSolveExpression for Counted {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError> {
        let counts = &self.counts;
        let (count, fail) = match self.shape {
            Shape::Fused => (&counts.fused, counts.fail_fused.get()),
            Shape::Pair => (&counts.pair, counts.fail_pair.get()),
            Shape::Single => (&counts.single, counts.fail_single.get()),
            Shape::Other => return self.inner.call(y, p, t, tables, out),
        };
        count.set(count.get() + 1);
        if fail {
            return Err(crate::RuntimeSolveError::solve_ir(match self.shape {
                Shape::Fused => "fused entry failed",
                Shape::Pair => "pair program failed",
                _ => "single program failed",
            }));
        }
        self.inner.call(y, p, t, tables, out)?;
        if self.shape == Shape::Fused && counts.nan_fused.get() {
            out[0] = f64::NAN;
        }
        Ok(())
    }
}

/// The reference backend, with every compiled residual block wrapped in a
/// counter that tells the fixture's blocks apart by their shape.
struct CountingBackend(Rc<Counts>);

impl SolveExecutionBackend for CountingBackend {
    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, crate::CompileRefusal> {
        let outputs: Vec<_> = source
            .programs()
            .iter()
            .map(|ops| solve::ScalarProgramBlock::program_output_count(ops))
            .collect();
        let shape = match outputs.as_slice() {
            [2, 2] => Shape::Fused,
            [2] => Shape::Pair,
            [1] => Shape::Single,
            _ => Shape::Other,
        };
        Ok(Rc::new(Counted {
            inner: super::super::test_kernels::expression(source)?,
            shape,
            counts: Rc::clone(&self.0),
        }))
    }

    fn compile_jacobian_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, crate::CompileRefusal> {
        Ok(super::super::test_kernels::jacobian(source)?)
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

const Y: [f64; 5] = [1.0, 2.0, 3.0, 5.0, 7.0];
const P: [f64; 1] = [0.5];
/// `y0 - y1, y0 + y1, y2 - y3, y2 + y3, y4 - p0` at `Y`, `P`.
const RESIDUAL: [f64; 5] = [-1.0, 3.0, -2.0, 8.0, 6.5];

fn fused_runtime() -> (SolveRuntime, Rc<Counts>) {
    fused_runtime_with(&fused_model())
}

fn fused_runtime_with(model: &solve::SolveModel) -> (SolveRuntime, Rc<Counts>) {
    let counts = Rc::new(Counts::default());
    let runtime =
        SolveRuntime::new_fixture_with_backend(model, Rc::new(CountingBackend(Rc::clone(&counts))))
            .unwrap();
    (runtime, counts)
}

/// One residual pass of block 0 through its issued residual selection.
fn residual_pass(runtime: &SolveRuntime) -> Result<[f64; 5], crate::RuntimeSolveError> {
    let projection = super::native_projection_jvp::selected_projection(runtime, false);
    let selection = runtime
        .scalar_execution()
        .unwrap()
        .scalar
        .continuous_structural
        .algebraic_projection()[0]
        .residual_output_evaluation()
        .unwrap();
    let mut out = [0.0; 5];
    assert!(projection.eval_implicit_residual_outputs(0, selection, &Y, &P, 0.0, &mut out)?);
    Ok(out)
}

/// Whether the runtime's projection treats block 0 as affine, the only kind
/// of block whose residual runs the grouped pass.
fn block_is_affine(runtime: &SolveRuntime) -> bool {
    super::native_projection_jvp::selected_projection(runtime, false)
        .algebraic_projection_block_is_affine(0)
}

fn reset(counts: &Counts) {
    for count in [&counts.fused, &counts.pair, &counts.single] {
        count.set(0);
    }
}

fn calls(counts: &Counts) -> [usize; 3] {
    [counts.fused.get(), counts.pair.get(), counts.single.get()]
}

#[test]
fn the_fused_entry_answers_the_unsplit_programs_and_the_split_one_follows() {
    let (runtime, counts) = fused_runtime();
    assert!(block_is_affine(&runtime));
    let fused = runtime
        .scalar_execution()
        .unwrap()
        .scalar
        .continuous_structural
        .algebraic_projection()[0]
        .fused_residual_evaluation()
        .expect("the two unsplit programs are issued for one fused entry");
    assert_eq!(fused.bases(), [0, 2]);
    reset(&counts);
    let out = residual_pass(&runtime).unwrap();
    assert_eq!(out.map(f64::to_bits), RESIDUAL.map(f64::to_bits));
    // Debug builds re-run the whole ordered pass after an accepted one, to
    // compare its bits (two pair calls and one single call).
    let compare = if cfg!(debug_assertions) {
        [0, 2, 1]
    } else {
        [0; 3]
    };
    assert_eq!(
        calls(&counts),
        [1 + compare[0], compare[1], 1 + compare[2]],
        "one fused call, no unsplit program call, the split program once"
    );
}

#[test]
fn a_failed_or_non_finite_fused_entry_leaves_the_whole_ordered_pass() {
    let (runtime, counts) = fused_runtime();
    for fault in [&counts.fail_fused, &counts.nan_fused] {
        fault.set(true);
        reset(&counts);
        let out = residual_pass(&runtime).unwrap();
        assert_eq!(out.map(f64::to_bits), RESIDUAL.map(f64::to_bits));
        assert_eq!(calls(&counts), [1, 2, 1], "every program runs in order");
        fault.set(false);
    }
}

#[test]
fn errors_keep_their_program_order_identity_through_the_fused_entry() {
    let (runtime, counts) = fused_runtime();
    counts.fail_fused.set(true);
    counts.fail_pair.set(true);
    counts.fail_single.set(true);
    reset(&counts);
    let error = residual_pass(&runtime).unwrap_err();
    assert!(error.to_string().contains("pair program failed"), "{error}");
    assert_eq!(
        calls(&counts),
        [1, 1, 0],
        "the first program's error stops the pass"
    );
    counts.fail_fused.set(false);
    counts.fail_pair.set(false);
    reset(&counts);
    let error = residual_pass(&runtime).unwrap_err();
    assert!(
        error.to_string().contains("single program failed"),
        "{error}"
    );
    assert_eq!(calls(&counts), [1, 0, 1]);
}

#[test]
fn a_selection_issued_over_other_programs_gets_no_fused_entry() {
    let (runtime, counts) = fused_runtime();
    let scalar = &runtime.scalar_execution().unwrap().scalar;
    let model = fused_model();
    let other = spanned_block(rows(false), "fused_residual.mo");
    let backend = CountingBackend(Rc::clone(&counts));
    let analysis = scalar.implicit_scalar_rhs.source();
    let structures = model.artifacts.continuous.structural.algebraic_projection();
    let canonical = structures[0]
        .fused_residual_evaluation()
        .unwrap()
        .primal_source()
        .clone();
    let issued = super::super::fused_residual::fused_residuals(
        |_| true,
        structures,
        (analysis, &canonical),
        Some(&backend),
    );
    assert!(issued[0].is_some(), "the issuing programs get the entry");
    let foreign = super::super::fused_residual::fused_residuals(
        |_| true,
        structures,
        (analysis, &other),
        Some(&backend),
    );
    assert!(
        foreign[0].is_none(),
        "equal but separately owned programs do not"
    );
    assert!(
        super::super::fused_residual::fused_residuals(
            |_| true,
            structures,
            (analysis, &canonical),
            None
        )[0]
        .is_none()
    );
    let ungrouped = super::super::fused_residual::fused_residuals(
        |_| false,
        structures,
        (analysis, &canonical),
        Some(&backend),
    );
    assert!(
        ungrouped[0].is_none(),
        "a block whose residual does not run program by program gets none"
    );
}

#[test]
fn a_non_affine_block_gets_no_fused_entry() {
    let model = fused_model_with(vec![pair(0, 1, false), select_pair(), single(false)]);
    let (runtime, counts) = fused_runtime_with(&model);
    assert!(
        !block_is_affine(&runtime),
        "the select on block unknowns makes the block non-affine"
    );
    let scalar = &runtime.scalar_execution().unwrap().scalar;
    assert!(
        scalar.continuous_structural.algebraic_projection()[0]
            .fused_residual_evaluation()
            .is_some(),
        "Solve IR still issues the fused selection"
    );
    assert!(
        scalar.fused_residuals[0].is_none(),
        "no entry is compiled for a block that never runs a grouped residual pass"
    );
    reset(&counts);
    let out = residual_pass(&runtime).unwrap();
    let expected = [-1.0, 3.0, 2.0, 8.0, 6.5];
    assert_eq!(out.map(f64::to_bits), expected.map(f64::to_bits));
    assert_eq!(counts.fused.get(), 0);
}

#[test]
fn an_affine_block_answers_a_lazily_planned_program_inside_the_fused_entry() {
    let primal = vec![pair(0, 1, false), affine_select_pair(), single(false)];
    let block = spanned_block(primal.clone(), "fused_residual.mo");
    assert!(
        solve::BlockResidualSplit::derive(block.program(1).unwrap(), &[0, 1, 2, 3, 4]).is_none(),
        "no operation of the select program can be hoisted"
    );
    let (runtime, counts) = fused_runtime_with(&fused_model_with(primal));
    assert!(
        block_is_affine(&runtime),
        "a parameter condition keeps the block affine"
    );
    let scalar = &runtime.scalar_execution().unwrap().scalar;
    assert!(scalar.implicit_scalar_rhs.source().has_lazy_row_plan(1));
    let programs: Vec<_> = scalar.continuous_structural.algebraic_projection()[0]
        .fused_residual_evaluation()
        .unwrap()
        .selection()
        .programs()
        .iter()
        .map(|p| p.program())
        .collect();
    assert_eq!(programs, [0, 1], "the lazily planned program is fused");
    assert!(scalar.fused_residuals[0].is_some());
    reset(&counts);
    let out = residual_pass(&runtime).unwrap();
    // 0 < 0.5 selects 5 - 3 for row 2.
    let expected = [-1.0, 3.0, 2.0, 8.0, 6.5];
    assert_eq!(out.map(f64::to_bits), expected.map(f64::to_bits));
    // Debug builds re-run the whole ordered pass to compare its bits (both
    // two-output programs and the single one).
    let compare = if cfg!(debug_assertions) {
        [0, 2, 1]
    } else {
        [0; 3]
    };
    assert_eq!(
        calls(&counts),
        [1 + compare[0], compare[1], 1 + compare[2]],
        "one fused call through the grouped pass, the split program once"
    );
}
