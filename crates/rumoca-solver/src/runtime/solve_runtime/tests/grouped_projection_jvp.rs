//! Shared tensor outputs execute once per prepared projection color.

use super::*;
use std::cell::Cell;

#[derive(Default)]
struct GroupedJacobian {
    single_calls: Cell<usize>,
    grouped_calls: Cell<usize>,
    fail: bool,
    /// Bind the Direct committing sweep, counting sweeps instead of calls.
    direct: bool,
    sweeps: Cell<usize>,
    /// Answer the split fixture's three programs instead of the coupled one.
    split: bool,
    /// The one program that fails, when not every program does.
    fail_program: Option<usize>,
}

impl GroupedJacobian {
    /// One program's outputs under `seed`, or its failure.
    fn outputs(
        &self,
        program: usize,
        p: &[f64],
        seed: &[f64],
    ) -> Result<Vec<f64>, crate::RuntimeSolveError> {
        if self.fail || self.fail_program == Some(program) {
            return Err("grouped native JVP failed".into());
        }
        if !self.split {
            assert_eq!(program, 0);
            return Ok(vec![seed[0] + p[0] * seed[1], p[0] * seed[0] - seed[1]]);
        }
        Ok(match program {
            0 => vec![seed[0], seed[1]],
            1 => vec![2.0 * seed[2]],
            2 => vec![seed[3]],
            other => panic!("the split fixture has no program {other}"),
        })
    }
}

impl CompiledSolveJacobianExpression for GroupedJacobian {
    fn programs_committing(
        self: std::rc::Rc<Self>,
    ) -> Option<std::rc::Rc<dyn crate::CompiledProgramsCommitting>> {
        if self.direct { Some(self) } else { None }
    }

    fn call(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _seed: &[f64],
        _tables: &[rumoca_core::ExternalTableData],
        _out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError> {
        panic!("a selected projection must not execute unrelated programs")
    }

    fn call_program_output(
        &self,
        (program, offset): (usize, usize),
        _y: &[f64],
        p: &[f64],
        _t: f64,
        seed: &[f64],
        _tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, crate::RuntimeSolveError> {
        assert_eq!(program, 0);
        self.single_calls.set(self.single_calls.get() + 1);
        Ok(Some(
            [seed[0] + p[0] * seed[1], p[0] * seed[0] - seed[1]][offset],
        ))
    }

    fn call_program_outputs(
        &self,
        program: usize,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        _tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, crate::RuntimeSolveError> {
        self.grouped_calls.set(self.grouped_calls.get() + 1);
        let solve_eval::JacobianEvalInputs { p, seed, .. } = inputs;
        let values = self.outputs(program, p, seed)?;
        out.clear();
        out.extend(values);
        Ok(true)
    }
}

impl crate::CompiledProgramsCommitting for GroupedJacobian {
    fn call_selected_programs_committing(
        &self,
        programs: &[usize],
        (_, p, _): (&[f64], &[f64], f64),
        seed: &mut [f64],
        _tables: &[rumoca_core::ExternalTableData],
        commit: &mut crate::ProgramOutputCommit<'_>,
    ) -> Result<(), crate::RuntimeSolveError> {
        self.sweeps.set(self.sweeps.get() + 1);
        for &program in programs {
            let values = self.outputs(program, p, seed)?;
            commit(program, &values, seed)?;
        }
        Ok(())
    }
}

fn coupled_program(derivative: bool) -> solve::ScalarProgramBlock {
    use solve::{BinaryOp as B, LinearOp as L};
    let load = |dst, index| {
        if derivative {
            L::LoadSeed { dst, index }
        } else {
            L::LoadY { dst, index }
        }
    };
    let ops = vec![
        load(0, 0),
        load(1, 1),
        L::LoadP { dst: 2, index: 0 },
        L::Binary {
            dst: 3,
            op: B::Mul,
            lhs: 2,
            rhs: 1,
        },
        L::Binary {
            dst: 4,
            op: B::Add,
            lhs: 0,
            rhs: 3,
        },
        L::Binary {
            dst: 5,
            op: B::Mul,
            lhs: 2,
            rhs: 0,
        },
        L::Binary {
            dst: 6,
            op: B::Sub,
            lhs: 5,
            rhs: 1,
        },
        L::Const {
            dst: 7,
            value: if derivative { 0.0 } else { 2.0 },
        },
        L::Const {
            dst: 8,
            value: if derivative { 0.0 } else { 1.0 },
        },
        L::Binary {
            dst: 9,
            op: B::Sub,
            lhs: 4,
            rhs: 7,
        },
        L::Binary {
            dst: 10,
            op: B::Sub,
            lhs: 6,
            rhs: 8,
        },
        L::StoreOutputRange {
            start: 9,
            count: 2,
            stride: 1,
        },
    ];
    solve::ScalarProgramBlock::with_output_indices(
        vec![ops],
        vec![test_span("grouped_projection.mo")],
        vec![1, 0],
    )
    .unwrap()
}

pub(super) fn coupled_model() -> solve::SolveModel {
    let mut model = mode_dependent_repivot_model();
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(coupled_program(false));
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(1)), Some(solve::scalar_slot_y(0))].into();
    let mut blocks = model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .scalar_blocks()
        .to_vec();
    blocks[0].rows = vec![1, 0];
    model.problem.continuous.algebraic_projection_plan.blocks = blocks.into();
    model.artifacts.continuous.implicit_jacobian_v_scalar = coupled_program(true);
    mirror_scalar_implicit_jvp(&mut model);
    derive_test_structural_artifacts(&mut model);
    model
}

#[derive(Default)]
struct GroupedResidual {
    single_calls: Cell<usize>,
    grouped_calls: Cell<usize>,
    fail: bool,
}

impl CompiledSolveExpression for GroupedResidual {
    fn call(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _tables: &[rumoca_core::ExternalTableData],
        _out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError> {
        panic!("selected residuals must not execute unrelated programs")
    }

    fn call_program_output(
        &self,
        (program, offset): (usize, usize),
        y: &[f64],
        p: &[f64],
        _t: f64,
        _tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, crate::RuntimeSolveError> {
        assert_eq!(program, 0);
        self.single_calls.set(self.single_calls.get() + 1);
        Ok(Some(
            [y[0] + p[0] * y[1] - 2.0, p[0] * y[0] - y[1] - 1.0][offset],
        ))
    }

    fn call_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        _t: f64,
        _tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, crate::RuntimeSolveError> {
        assert_eq!(program, 0);
        self.grouped_calls.set(self.grouped_calls.get() + 1);
        if self.fail {
            return Err("grouped native residual failed".into());
        }
        out.clear();
        out.extend([y[0] + p[0] * y[1] - 2.0, p[0] * y[0] - y[1] - 1.0]);
        Ok(true)
    }
}

#[test]
fn grouped_projection_residual_evaluates_shared_outputs_once_at_each_point() {
    let mut runtime = SolveRuntime::new_fixture(&coupled_model()).unwrap();
    let native = Rc::new(GroupedResidual::default());
    install_mock_primal(&mut runtime, native.clone());
    let projection = super::native_projection_jvp::selected_projection(&runtime, false);
    let selection = runtime
        .scalar_execution()
        .unwrap()
        .scalar
        .continuous_structural
        .algebraic_projection()[0]
        .residual_output_evaluation()
        .unwrap();
    let mut selected = [0.0; 2];
    assert!(
        projection
            .eval_implicit_residual_outputs(selection, &[8.0, -5.0], &[2.0], 1.0, &mut selected)
            .unwrap()
    );
    assert_eq!(selected, [-4.0, 20.0]);
    for k in [2.0, -3.0, 0.0, 2.0] {
        let before = native.grouped_calls.get();
        let mut y = vec![8.0, -5.0];
        runtime
            .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[k], 1e-10, 4)
            .unwrap();
        assert!((y[0] - (2.0 + k) / (1.0 + k * k)).abs() < 1e-9);
        assert!((y[1] - (2.0 * k - 1.0) / (1.0 + k * k)).abs() < 1e-9);
        assert!(
            native.grouped_calls.get() > before,
            "changed coordinates need fresh residuals"
        );
        assert_eq!(
            native.single_calls.get(),
            0,
            "tensor residual outputs must share one invocation"
        );
    }
}

#[test]
fn grouped_projection_residual_failure_propagates_without_replay_or_partial_commit() {
    let mut runtime = SolveRuntime::new_fixture(&coupled_model()).unwrap();
    let native = Rc::new(GroupedResidual {
        fail: true,
        ..Default::default()
    });
    install_mock_primal(&mut runtime, native.clone());
    let mut y = vec![8.0, -5.0];
    let error = runtime
        .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[2.0], 1e-10, 4)
        .unwrap_err();
    assert!(error.to_string().contains("grouped native residual failed"));
    assert_eq!(native.grouped_calls.get(), 1);
    assert_eq!(native.single_calls.get(), 0);
    assert_eq!(y, [8.0, -5.0]);
}

#[test]
fn grouped_projection_jvp_executes_once_per_color_at_changing_coefficients() {
    let model = coupled_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let native = Rc::new(GroupedJacobian::default());
    install_mock_jacobian(&mut runtime, false, native.clone());
    for (iteration, k) in [2.0, -3.0, 0.0, 2.0].into_iter().enumerate() {
        let mut y = vec![8.0, -5.0];
        runtime
            .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[k], 1e-10, 4)
            .unwrap();
        assert!((y[0] - (2.0 + k) / (1.0 + k * k)).abs() < 1e-9);
        assert!((y[1] - (2.0 * k - 1.0) / (1.0 + k * k)).abs() < 1e-9);
        assert_eq!(native.grouped_calls.get(), 2 * (iteration + 1));
        assert_eq!(
            native.single_calls.get(),
            0,
            "shared outputs must not replay the program"
        );
    }
}

#[test]
fn grouped_projection_jvp_error_propagates_without_single_row_retry() {
    let model = coupled_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let native = Rc::new(GroupedJacobian {
        fail: true,
        ..Default::default()
    });
    install_mock_jacobian(&mut runtime, false, native.clone());
    let mut y = vec![8.0, -5.0];
    let error = runtime
        .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[2.0], 1e-10, 4)
        .unwrap_err();
    assert!(error.to_string().contains("grouped native JVP failed"));
    assert_eq!(native.grouped_calls.get(), 1);
    assert_eq!(native.single_calls.get(), 0);
    assert_eq!(y, [8.0, -5.0]);
}

#[test]
fn grouped_projection_jvp_interpreter_uses_the_same_output_projection() {
    let runtime = SolveRuntime::new_fixture(&coupled_model()).unwrap();
    let mut y = vec![8.0, -5.0];
    runtime
        .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[2.0], 1e-10, 4)
        .unwrap();
    assert!((y[0] - 0.8).abs() < 1e-12);
    assert!((y[1] - 0.6).abs() < 1e-12);
}

#[test]
fn grouped_projection_jvp_preserves_reverse_completed_rows_and_full_seed_space() {
    let mut runtime = SolveRuntime::new_fixture(&coupled_model()).unwrap();
    let native = Rc::new(GroupedJacobian::default());
    install_mock_jacobian(&mut runtime, true, native.clone());
    let projection = super::native_projection_jvp::selected_projection(&runtime, true);
    let selection = runtime
        .scalar_execution()
        .unwrap()
        .scalar
        .continuous_structural
        .algebraic_projection()[0]
        .output_evaluation(0)
        .unwrap();
    let mut out = [-99.0, -99.0];
    let point: (&[f64], &[f64], f64) = (&[8.0, -5.0], &[2.0], 1.0);
    let mut seed = [1.0, 3.0, 0.0];
    assert!(
        projection
            .eval_implicit_jacobian_v_outputs(selection, point, &mut seed, &[true, false], &mut out)
            .unwrap()
    );
    assert_eq!(
        seed,
        [1.0, 3.0, 0.0],
        "a grouped evaluation leaves its seed unchanged"
    );
    assert_eq!(out, [7.0, -99.0]);
    assert_eq!(native.grouped_calls.get(), 1);
    assert!(
        projection
            .eval_implicit_jacobian_v_outputs(
                selection,
                point,
                &mut seed,
                &[false, false],
                &mut out
            )
            .unwrap()
    );
    assert_eq!(
        native.grouped_calls.get(),
        1,
        "settled rows need no invocation"
    );
}

fn swept_projection_outputs(
    native: Rc<GroupedJacobian>,
    enabled: &[bool],
) -> (Result<bool, crate::RuntimeSolveError>, [f64; 2], [f64; 3]) {
    let mut runtime = SolveRuntime::new_fixture(&coupled_model()).unwrap();
    install_mock_jacobian(&mut runtime, true, native);
    let projection = super::native_projection_jvp::selected_projection(&runtime, true);
    let selection = runtime
        .scalar_execution()
        .unwrap()
        .scalar
        .continuous_structural
        .algebraic_projection()[0]
        .output_evaluation(0)
        .unwrap();
    let mut out = [-99.0, -99.0];
    let mut seed = [1.0, 3.0, 0.0];
    let point: (&[f64], &[f64], f64) = (&[8.0, -5.0], &[2.0], 1.0);
    let result =
        projection.eval_implicit_jacobian_v_outputs(selection, point, &mut seed, enabled, &mut out);
    (result, out, seed)
}

#[test]
fn grouped_projection_jvp_direct_sweep_matches_the_program_path_bit_for_bit() {
    let native = Rc::new(GroupedJacobian {
        direct: true,
        ..Default::default()
    });
    let (result, out, seed) = swept_projection_outputs(native.clone(), &[true, false]);
    assert!(result.unwrap());
    // The prepared program path of the same fixture writes 7.0 (see
    // `grouped_projection_jvp_preserves_reverse_completed_rows_and_full_seed_space`).
    assert_eq!(out[0].to_bits(), 7.0_f64.to_bits());
    assert_eq!(
        out[1].to_bits(),
        (-99.0_f64).to_bits(),
        "a disabled row stays untouched"
    );
    assert_eq!(
        seed,
        [1.0, 3.0, 0.0],
        "a swept evaluation leaves its seed unchanged"
    );
    assert_eq!(native.sweeps.get(), 1);
    assert_eq!(
        (native.grouped_calls.get(), native.single_calls.get()),
        (0, 0),
        "a Direct-bound owner is reached only through its sweep"
    );
}

#[test]
fn grouped_projection_jvp_direct_sweep_skips_a_fully_settled_color() {
    let native = Rc::new(GroupedJacobian {
        direct: true,
        ..Default::default()
    });
    let (result, out, seed) = swept_projection_outputs(native.clone(), &[false, false]);
    assert!(result.unwrap());
    assert_eq!(out, [-99.0, -99.0]);
    assert_eq!(seed, [1.0, 3.0, 0.0]);
    assert_eq!(native.sweeps.get(), 0, "settled rows need no sweep");
}

#[test]
fn grouped_projection_jvp_direct_sweep_error_keeps_seed_and_prior_output() {
    let native = Rc::new(GroupedJacobian {
        direct: true,
        fail: true,
        ..Default::default()
    });
    let (result, out, seed) = swept_projection_outputs(native.clone(), &[true, true]);
    let error = result.unwrap_err();
    assert!(error.to_string().contains("grouped native JVP failed"));
    assert_eq!(out, [-99.0, -99.0], "a failing program commits nothing");
    assert_eq!(
        seed,
        [1.0, 3.0, 0.0],
        "an error exit leaves the seed unchanged"
    );
    assert_eq!(
        native.sweeps.get(),
        1,
        "no program-by-program retry after a swept error"
    );
}

/// Four decoupled rows in one color, owned by three programs: program 0
/// answers rows 0 and 1, program 1 row 2, program 2 row 3.
fn split_program(derivative: bool) -> solve::ScalarProgramBlock {
    use solve::{BinaryOp as B, LinearOp as L};
    let row = |index: usize, offset: f64, dst: u32| {
        if derivative {
            vec![L::LoadSeed { dst, index }]
        } else {
            vec![
                L::LoadY { dst, index },
                L::Const {
                    dst: dst + 1,
                    value: offset,
                },
                L::Binary {
                    dst,
                    op: B::Sub,
                    lhs: dst,
                    rhs: dst + 1,
                },
            ]
        }
    };
    let mut first = row(0, 2.0, 0);
    first.extend(row(1, 3.0, 2));
    first.push(L::StoreOutput { src: 0 });
    first.push(L::StoreOutput { src: 2 });
    let single = |index: usize, offset: f64| {
        let mut ops = row(index, offset, 0);
        ops.push(L::StoreOutput { src: 0 });
        ops
    };
    solve::ScalarProgramBlock::with_output_indices(
        vec![first, single(2, 4.0), single(3, 5.0)],
        vec![test_span("grouped_split.mo"); 3],
        vec![0, 1, 2, 3],
    )
    .unwrap()
}

fn split_model() -> solve::SolveModel {
    let mut model = mode_dependent_repivot_model();
    model.problem.layout = crate::fixture_layout::named_y_layout(
        rumoca_ir_solve::VarLayout::from_parts(Default::default(), 0, 3),
        ["a", "b", "c", "d"].map(String::from).to_vec(),
    );
    model.problem.solve_layout.algebraic_scalar_count = 4;
    model.initial_y = vec![0.0; 4].into();
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(split_program(false));
    model.problem.continuous.implicit_row_targets = (0..4)
        .map(|row| Some(solve::scalar_slot_y(row)))
        .collect::<Vec<_>>()
        .into();
    let mut blocks = model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .scalar_blocks()
        .to_vec();
    blocks[0].rows = vec![0, 1, 2, 3];
    blocks[0].y_indices = vec![0, 1, 2, 3];
    model.problem.continuous.algebraic_projection_plan.blocks = blocks.into();
    model.artifacts.continuous.implicit_jacobian_v_scalar = split_program(true);
    mirror_scalar_implicit_jvp(&mut model);
    derive_test_structural_artifacts(&mut model);
    model
}

/// A later program's failure in a two-or-more-program color keeps the earlier
/// program's enabled placement, leaves the failing, later and disabled rows
/// and the seed bit-unchanged, and is not retried, on both seed axes and both
/// sweep routes.
#[test]
fn grouped_projection_jvp_later_program_failure_keeps_earlier_placement() {
    for (full, direct) in [(false, false), (false, true), (true, false), (true, true)] {
        later_program_failure_keeps_earlier_placement(full, direct);
    }
}

/// One seed axis and sweep route of
/// [`grouped_projection_jvp_later_program_failure_keeps_earlier_placement`].
fn later_program_failure_keeps_earlier_placement(full: bool, direct: bool) {
    let native = Rc::new(GroupedJacobian {
        direct,
        split: true,
        fail_program: Some(1),
        ..Default::default()
    });
    let mut runtime = SolveRuntime::new_fixture(&split_model()).unwrap();
    install_mock_jacobian(&mut runtime, full, native.clone());
    let projection = super::native_projection_jvp::selected_projection(&runtime, full);
    let (selection, programs) = single_color_selection(&runtime, full);
    assert_eq!(
        programs,
        [0, 1, 2],
        "one color issues all three programs in order"
    );
    let scratch_capacity = || {
        runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .grouped_program_scratch
            .borrow()
            .capacity()
    };
    let reserved = scratch_capacity();
    assert!(
        reserved >= programs.len(),
        "construction reserves the largest issued selection"
    );
    let sentinel = -99.0_f64;
    let mut out = [sentinel; 4];
    let mut seed = vec![0.5, 0.25, 0.125, 0.0625];
    if full {
        seed.push(0.0);
    }
    let before = seed.clone();
    let point: (&[f64], &[f64], f64) = (&[1.0, 2.0, 3.0, 4.0], &[2.0], 1.0);
    let error = projection
        .eval_implicit_jacobian_v_outputs(
            selection,
            point,
            &mut seed,
            &[true, false, true, true],
            &mut out,
        )
        .unwrap_err();
    assert!(error.to_string().contains("grouped native JVP failed"));
    assert_eq!(
        out[0].to_bits(),
        0.5_f64.to_bits(),
        "the earlier program placed row 0"
    );
    for row in [1, 2, 3] {
        assert_eq!(
            out[row].to_bits(),
            sentinel.to_bits(),
            "row {row}: disabled, failing or later output stays untouched"
        );
    }
    assert_eq!(
        bits(&seed),
        bits(&before),
        "every seed cell, including signed zeros, is bit-unchanged after the failure"
    );
    assert_eq!(
        scratch_capacity(),
        reserved,
        "collecting the enabled programs never grows the list"
    );
    assert_no_retry(&native, direct);
}

/// The single color's issued selection, and its program order on one seed axis.
fn single_color_selection(
    runtime: &SolveRuntime,
    full: bool,
) -> (&solve::ProjectionJacobianOutputs, Vec<usize>) {
    let structure = &runtime
        .scalar_execution()
        .unwrap()
        .scalar
        .continuous_structural
        .algebraic_projection()[0];
    assert_eq!(
        structure.coloring().groups().len(),
        1,
        "every column shares one color"
    );
    let selection = structure.output_evaluation(0).unwrap();
    let axis = if full {
        selection.solver_y_and_parameters()
    } else {
        selection.solver_y()
    };
    let programs = axis
        .unwrap()
        .programs()
        .iter()
        .map(|program| program.program())
        .collect();
    (selection, programs)
}

/// The failing evaluation ran once on its route: one Direct sweep, or the
/// earlier and failing programs once each with the later one never run.
fn assert_no_retry(native: &GroupedJacobian, direct: bool) {
    if direct {
        assert_eq!(native.sweeps.get(), 1, "one sweep, never retried");
        assert_eq!(native.grouped_calls.get(), 0);
    } else {
        assert_eq!(
            native.grouped_calls.get(),
            2,
            "programs 0 and 1 run once each; program 2 never runs"
        );
        assert_eq!(native.sweeps.get(), 0);
    }
    assert_eq!(native.single_calls.get(), 0);
}

fn bits(values: &[f64]) -> Vec<u64> {
    values.iter().map(|value| value.to_bits()).collect()
}
