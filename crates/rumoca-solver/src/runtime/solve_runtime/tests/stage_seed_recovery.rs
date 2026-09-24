//! Optional seeds cannot replace the issued coupled residual block.

use super::*;
use std::cell::Cell;

fn coupled_rows(tangent: bool) -> Vec<Vec<solve::LinearOp>> {
    use solve::{BinaryOp as B, LinearOp as L};
    let load = |dst, index| {
        if tangent {
            L::LoadSeed { dst, index }
        } else {
            L::LoadY { dst, index }
        }
    };
    let binary = |dst, op, lhs, rhs| L::Binary { dst, op, lhs, rhs };
    vec![
        vec![
            load(0, 0),
            L::Const {
                dst: 1,
                value: if tangent { 0.0 } else { 2.0 },
            },
            binary(2, B::Sub, 0, 1),
            L::StoreOutput { src: 2 },
        ],
        vec![
            load(0, 1),
            load(1, 2),
            L::LoadP { dst: 2, index: 0 },
            binary(3, B::Mul, 0, 2),
            binary(4, B::Add, 3, 1),
            L::StoreOutput { src: 4 },
        ],
        vec![
            load(0, 1),
            load(1, 2),
            L::LoadY { dst: 2, index: 2 },
            L::Const {
                dst: 3,
                value: if tangent { 2.0 } else { 1.0 },
            },
            binary(4, B::Mul, 1, 2),
            binary(5, B::Mul, 4, 3),
            binary(6, B::Add, 0, 5),
            load(7, 0),
            binary(8, B::Sub, 6, 7),
            L::StoreOutput { src: 8 },
        ],
        vec![
            load(0, 3),
            load(1, 1),
            load(2, 2),
            binary(3, B::Sub, 0, 1),
            binary(4, B::Sub, 3, 2),
            L::StoreOutput { src: 4 },
        ],
    ]
}

fn model() -> solve::SolveModel {
    // model CoupledSeed
    //   parameter Real p=0; Real u,x,y,v;
    // equation u=2; p*x+y=0; x+y*y=u; v=x+y; end CoupledSeed;
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: ["u", "x", "y", "v"].map(str::to_owned).to_vec(),
                    ..Default::default()
                },
                algebraic_scalar_count: 4,
                parameter_count: 1,
                compiled_parameter_len: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    coupled_rows(false),
                    "CoupledSeed.mo",
                )),
                implicit_row_targets: (0..4).map(|i| Some(solve::scalar_slot_y(i))).collect(),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: [vec![0], vec![1, 2], vec![3]]
                        .into_iter()
                        .map(|rows| solve::AlgebraicProjectionBlock {
                            y_indices: rows.clone(),
                            rows,
                            tearing: None,
                            guarded_tearing: None,
                            alternate_charts: Vec::new(),
                        })
                        .collect(),
                },
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![9.0, 1.0, 0.25, -3.0],
        ..Default::default()
    };
    set_test_implicit_jvp(&mut model, coupled_rows(true), "CoupledSeed.mo");
    derive_test_structural_artifacts(&mut model);
    model
}

#[derive(Clone, Copy, PartialEq)]
enum Failure {
    None,
    NonFinite,
    NonConvergence,
    Native,
}

struct ResidualObserver {
    calls: RefCell<Vec<(usize, Vec<f64>)>>,
    failure: Cell<Failure>,
    global_entries: RefCell<Vec<Vec<f64>>>,
    calls_before_global: Cell<Option<usize>>,
    prepared: PreparedScalarProgramBlock,
}

impl CompiledSolveExpression for ResidualObserver {
    fn call(
        &self,
        _: &[f64],
        _: &[f64],
        _: f64,
        _: &[rumoca_core::ExternalTableData],
        _: &mut [f64],
    ) -> Result<(), String> {
        panic!("projection must retain selected-row ownership")
    }

    fn call_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, String> {
        let value = self
            .call_program_output((program, 0), y, p, t, tables)?
            .unwrap();
        out.clear();
        out.push(value);
        Ok(true)
    }

    fn call_program_output(
        &self,
        (program, offset): (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        _: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, String> {
        assert_eq!(offset, 0);
        self.calls.borrow_mut().push((program, y.to_vec()));
        if program == 1 && self.failure.get() == Failure::NonConvergence {
            return Ok(Some(1.0));
        }
        if program == 1 {
            match self.failure.replace(Failure::None) {
                Failure::NonFinite => return Ok(Some(f64::NAN)),
                Failure::Native => return Err("stage residual native failure".to_owned()),
                Failure::None => {}
                Failure::NonConvergence => unreachable!(),
            }
        }
        let mut out = Vec::new();
        self.prepared
            .eval_row_outputs_unchecked_with_context(
                program,
                y,
                p,
                t,
                RowEvalContext::default(),
                &mut out,
            )
            .map_err(|e| e.to_string())?;
        Ok(Some(out[0]))
    }
}

fn runtime(failure: Failure) -> (SolveRuntime, Rc<ResidualObserver>) {
    let model = model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    assert!(runtime.value_stage_schedule_is_certified(&runtime.algebraic_refresh));
    let stages = &runtime.algebraic_refresh.value_stages;
    let (block, rows) = stages
        .iter()
        .find_map(|stage| match stage {
            solve::RefreshStage::ProjectionBlock {
                plan, seed_rows, ..
            } => Some((&plan.blocks[0], seed_rows)),
            _ => None,
        })
        .unwrap();
    assert_eq!(block.y_indices, [1, 2]);
    assert_eq!(block.rows, [1, 2]);
    assert!(
        !rows.is_empty(),
        "nonlinear coupled owner retains optional seeds"
    );
    let observer = Rc::new(ResidualObserver {
        calls: RefCell::new(Vec::new()),
        failure: Cell::new(failure),
        global_entries: RefCell::new(Vec::new()),
        calls_before_global: Cell::new(None),
        prepared: runtime.implicit_scalar_rhs.clone(),
    });
    runtime.compiled_implicit_rhs = Some(observer.clone());
    let upstream = runtime
        .implicit_scalar_rhs
        .exact_target_assignment_output_program(0, 0, 0)
        .unwrap();
    runtime.execution_backend = Some(Rc::new(ObservingBackend {
        upstream,
        observer: observer.clone(),
    }));
    (runtime, observer)
}

fn run(runtime: &SolveRuntime, y: &mut [f64], certified: bool) -> Result<(), RuntimeSolveError> {
    runtime.refresh_slots_with_plan(
        &runtime.algebraic_refresh,
        RefreshSlotArgs {
            t: 0.0,
            solver_y: y,
            params: &[0.0],
            tol: 1e-10,
            max_iters: 8,
            certify_coordinates: certified,
        },
    )
}

#[test]
fn stage_seed_failure_projects_only_its_issued_block() {
    for certified in [false, true] {
        let (runtime, observer) = runtime(Failure::None);
        let mut y = model().initial_y;
        run(&runtime, &mut y, certified).unwrap();
        assert_eq!(y, [2.0, 2.0, 0.0, 2.0]);
        let calls = observer.calls.borrow();
        assert!(
            observer.global_entries.borrow().is_empty(),
            "seed failure replayed an upstream block"
        );
        assert_eq!(
            calls[0],
            (1, vec![2.0, 1.0, 0.25, -3.0]),
            "local projection must start at the complete stage-entry coordinate"
        );
    }
}

#[test]
fn stage_seed_projection_keeps_internal_nonfinite_recovery() {
    let (runtime, observer) = runtime(Failure::NonFinite);
    let mut y = model().initial_y;
    run(&runtime, &mut y, true).unwrap();
    assert_eq!(y, [2.0, 2.0, 0.0, 2.0]);
    let calls = observer.calls.borrow();
    assert_eq!(calls[0].0, 1, "try the existing block first");
    assert!(
        observer.global_entries.borrow().is_empty(),
        "the block already recovered its transient nonfinite residual"
    );
}

#[test]
fn stage_seed_local_native_failure_is_not_replayed() {
    let (runtime, observer) = runtime(Failure::Native);
    let mut y = model().initial_y;
    let incoming = y.clone();
    let error = run(&runtime, &mut y, true).unwrap_err();
    assert!(error.to_string().contains("stage residual native failure"));
    assert_eq!(y, incoming);
    assert!(observer.global_entries.borrow().is_empty());
    assert_eq!(
        observer.calls.borrow().len(),
        1,
        "native errors must not trigger complete-plan or interpreter replay"
    );
}

#[test]
fn stage_seed_zero_coefficient_is_an_actual_seed_error() {
    let (runtime, _) = runtime(Failure::None);
    let plan = &runtime.algebraic_refresh;
    let (sequence, rows) = plan
        .value_stages
        .iter()
        .find_map(|stage| match stage {
            solve::RefreshStage::ProjectionBlock {
                seed_sequence,
                seed_rows,
                ..
            } => Some((*seed_sequence, plan.selected_rows(seed_rows))),
            _ => None,
        })
        .unwrap();
    let mut y = [2.0, 1.0, 0.25, -3.0];
    let error = runtime
        .refresh_slots_once(rows, sequence, 0.0, &mut y, &[0.0])
        .unwrap_err();
    assert!(
        matches!(
            error,
            RuntimeSolveError::RefreshTargetSingular {
                target_y_index: 1,
                coefficient: 0.0,
                ..
            }
        ),
        "unexpected seed error: {error:?}"
    );
}

#[test]
fn stage_seed_local_nonconvergence_restores_original_global_entry() {
    for certified in [false, true] {
        let (runtime, observer) = runtime(Failure::NonConvergence);
        let mut y = model().initial_y;
        let incoming = y.clone();
        run(&runtime, &mut y, certified).unwrap();
        assert_eq!(y, [2.0, 2.0, 0.0, 2.0]);
        let calls = observer.calls.borrow();
        assert_eq!(
            calls[0].0, 1,
            "local nonconvergence must precede global projection"
        );
        assert!(
            calls.len() > 2,
            "the local numerical solve must actually attempt progress"
        );
        assert!(
            observer.calls_before_global.get().unwrap() > 1,
            "local numerical progress must precede any global replay"
        );
        assert_eq!(observer.global_entries.borrow()[0], incoming);
    }
}

struct SeedWrites {
    calls: Cell<usize>,
    values: [f64; 2],
    fail: bool,
}

impl CompiledSolveAssignmentSchedule for SeedWrites {
    fn call(
        &self,
        y: &mut [f64],
        _: &[f64],
        _: f64,
        _: &[rumoca_core::ExternalTableData],
    ) -> Result<(), String> {
        self.calls.set(self.calls.get() + 1);
        y[1..3].copy_from_slice(&self.values);
        if self.fail {
            Err("native seed failed after partial writes".to_owned())
        } else {
            Ok(())
        }
    }
}

struct ObservingBackend {
    upstream: Vec<solve::LinearOp>,
    observer: Rc<ResidualObserver>,
}
impl SolveExecutionBackend for ObservingBackend {
    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        if block.programs() != [self.upstream.clone()] {
            return Err("fixture declines unrelated isolators".into());
        }
        Ok(Rc::new(UpstreamAssignment {
            prepared: PreparedScalarProgramBlock::new(block.clone()).unwrap(),
            observer: self.observer.clone(),
        }))
    }
    fn compile_jacobian_expression(
        &self,
        _: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        Err("fixture declines".into())
    }
    fn compile_assignment_schedule(
        &self,
        _: &solve::ComputeBlock,
        _: &solve::ContinuousRefreshOwners,
        _: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        Err("fixture declines".into())
    }
    fn compile_event_transaction(
        &self,
        _: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        Err("fixture declines".into())
    }
}

fn install_seed(runtime: &mut SolveRuntime, values: [f64; 2], fail: bool) -> Rc<SeedWrites> {
    let sequence = runtime
        .algebraic_refresh
        .value_stages
        .iter()
        .find_map(|stage| match stage {
            solve::RefreshStage::ProjectionBlock { seed_sequence, .. } => Some(*seed_sequence),
            _ => None,
        })
        .unwrap();
    let seed = Rc::new(SeedWrites {
        calls: Cell::new(0),
        values,
        fail,
    });
    runtime
        .compiled_assignment_schedules
        .borrow_mut()
        .insert(sequence, Some(seed.clone()));
    seed
}

#[test]
fn stage_seed_recovery_restores_every_write_and_keeps_upstream() {
    for second in [f64::NAN, -101.0] {
        let (mut runtime, observer) = runtime(Failure::None);
        let seed = install_seed(&mut runtime, [f64::NAN, second], false);
        let mut y = model().initial_y;
        run(&runtime, &mut y, true).unwrap();
        assert_eq!(seed.calls.get(), 1);
        let calls = observer.calls.borrow();
        assert_eq!(calls[0], (1, vec![2.0, 1.0, 0.25, -3.0]));
        assert!(observer.global_entries.borrow().is_empty());
        assert_eq!(y, [2.0, 2.0, 0.0, 2.0]);
    }
}

#[test]
fn stage_seed_native_error_restores_call_entry_without_projection() {
    let (mut runtime, observer) = runtime(Failure::None);
    let seed = install_seed(&mut runtime, [f64::NAN; 2], true);
    let mut y = model().initial_y;
    let incoming = y.clone();
    let error = run(&runtime, &mut y, true).unwrap_err();
    assert!(matches!(error, RuntimeSolveError::SolveIr { .. }));
    assert!(
        error
            .to_string()
            .contains("native seed failed after partial writes")
    );
    assert_eq!(seed.calls.get(), 1);
    assert!(observer.calls.borrow().is_empty());
    assert_eq!(y, incoming);
}

// The complete projection re-executes this issued upstream isolator. The stage
// schedule executes its separate exact-assignment owner, which this adapter
// declines. Thus this call observes global replay even if its residual is not
// evaluated (the singleton can certify the exact assignment directly).
struct UpstreamAssignment {
    prepared: PreparedScalarProgramBlock,
    observer: Rc<ResidualObserver>,
}
impl CompiledSolveExpression for UpstreamAssignment {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        _: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String> {
        if self.observer.calls_before_global.get().is_none() {
            self.observer
                .calls_before_global
                .set(Some(self.observer.calls.borrow().len()));
        }
        self.observer.global_entries.borrow_mut().push(y.to_vec());
        if self.observer.failure.get() == Failure::NonConvergence {
            self.observer.failure.set(Failure::None);
        }
        self.prepared
            .eval_with_context(y, p, t, RowEvalContext::default(), out)
            .map_err(|e| e.to_string())
    }
}

#[test]
fn stage_seed_success_does_not_authorize_nonconvergence_replay() {
    let (mut runtime, observer) = runtime(Failure::NonConvergence);
    let seed = install_seed(&mut runtime, [2.0, 0.0], false);
    let mut y = model().initial_y;
    let incoming = y.clone();
    let error = run(&runtime, &mut y, true).unwrap_err();
    assert!(matches!(
        error,
        RuntimeSolveError::ProjectionNonConvergence { .. }
    ));
    assert_eq!(seed.calls.get(), 1);
    assert!(observer.global_entries.borrow().is_empty());
    assert_eq!(y, incoming);
}

#[test]
fn stage_seed_local_nonconvergence_has_typed_producer_and_preserved_diagnostic() {
    for certified in [false, true] {
        let (runtime, _) = runtime(Failure::NonConvergence);
        let (block_index, plan) = runtime
            .algebraic_refresh
            .value_stages
            .iter()
            .find_map(|stage| match stage {
                solve::RefreshStage::ProjectionBlock {
                    block_index, plan, ..
                } => Some((*block_index, plan)),
                _ => None,
            })
            .unwrap();
        let projection = RefreshProjectionModel {
            runtime: &runtime,
            seed_linearizations: None,
            plan,
            block_indices: std::slice::from_ref(&block_index),
            plan_validated: true,
            jacobian_v: ProjectionJacobian::SolverY {
                block: &runtime.implicit_projection_jacobian_v,
                scalar: &runtime.implicit_projection_scalar_jacobian_v,
            },
        };
        let incoming = [2.0, 1.0, 0.25, -3.0];
        let mut y = incoming;
        let args = crate::runtime::projection::AlgebraicProjectionArgs {
            parameters: &[0.0],
            time: 0.0,
            state_count: 0,
            tolerance: 1e-10,
        };
        let error = if certified {
            project_algebraics_with_plan_certified(&projection, plan, &mut y, args, 8)
        } else {
            project_algebraics_with_plan(&projection, plan, &mut y, args, 8)
        }
        .unwrap_err();
        let RuntimeSolveError::ProjectionNonConvergence { message } = &error else {
            panic!("numerical convergence failure lost its type: {error:?}")
        };
        assert!(message.contains("worst scaled residual row="));
        assert!(message.contains("row_scale="));
        assert_eq!(
            error.to_string(),
            format!("solve-IR evaluation failed: {message}")
        );
        let message = message.clone();
        assert!(
            matches!(crate::fmi_me::MeError::from(error), crate::fmi_me::MeError::Evaluation { message: actual } if actual == message)
        );
        assert_eq!(y, incoming);
    }
}
