use super::*;
use std::cell::Cell;

#[test]
fn prepared_projection_rejects_a_replaced_canonical_jvp_source() {
    let mut model = grouped_projection_jvp::coupled_model();
    assert!(SolveRuntime::new_fixture(&model.clone()).is_ok());
    let [solve::ComputeNode::ScalarPrograms(source)] = model
        .artifacts
        .continuous
        .implicit_jacobian_v
        .nodes
        .as_slice()
    else {
        panic!("fixture must retain one scalar-program owner");
    };
    let mut programs = source.programs().to_vec();
    let solve::LinearOp::LoadSeed { dst, .. } = programs[0][0] else {
        panic!("fixture begins with a seed read");
    };
    programs[0][0] = solve::LinearOp::Const { dst, value: 0.0 };
    let replacement = solve::ScalarProgramBlock::with_output_indices(
        programs,
        source.program_spans().to_vec(),
        source.output_indices().to_vec(),
    )
    .unwrap();
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(replacement);
    assert!(
        matches!(
            SolveRuntime::new_fixture(&model),
            Err(EvalSolveError::InvalidRow { message, .. }) if message.contains("canonical Jacobian")
        ),
        "stale derived metadata cannot substitute its old program for the canonical JVP"
    );
}

#[test]
fn interpreted_projection_rejects_missing_structural_owner() {
    let mut model = grouped_projection_jvp::coupled_model();
    assert!(SolveRuntime::new_fixture(&model).is_ok());
    model.artifacts.continuous.structural = Default::default();
    assert!(
        matches!(SolveRuntime::new_fixture(&model),
        Err(EvalSolveError::InvalidRow { message, .. })
            if message == "projection demand is outside its continuous structural owner"),
        "interpreter preparation cannot silently replace missing canonical structure"
    );
}

#[derive(Default)]
struct PreparedNative {
    prepares: Cell<usize>,
    calls: Cell<usize>,
    fail: bool,
    preparation_fail: bool,
}

struct Compiler(Rc<PreparedNative>);

impl CompiledSolveJacobianExpression for Compiler {
    fn prepare_projections(
        &self,
        applications: &[&solve::ProjectionJacobianApplication],
    ) -> Result<Vec<Option<Rc<dyn CompiledSolveProjectionJacobian>>>, String> {
        self.0
            .prepares
            .set(self.0.prepares.get() + applications.len());
        if self.0.preparation_fail {
            return Err("required projection preparation failed".into());
        }
        Ok(applications
            .iter()
            .map(|_| Some(self.0.clone() as Rc<dyn CompiledSolveProjectionJacobian>))
            .collect())
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
        panic!("prepared projection must not replay the whole Jacobian")
    }
}

impl CompiledSolveProjectionJacobian for PreparedNative {
    fn call(
        &self,
        _y: &[f64],
        p: &[f64],
        _t: f64,
        _tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        if self.fail {
            return Err("prepared projection failed".into());
        }
        out.copy_from_slice(&[1.0, p[0], p[0], -1.0]);
        Ok(())
    }
}

fn prepare(runtime: &mut SolveRuntime, native: Rc<PreparedNative>) {
    let prepared = {
        let view = runtime.scalar_execution().unwrap();
        refresh_projection::prepare_projection_jacobians(
            &view.scalar.continuous_structural,
            &view.scalar.implicit_scalar_rhs,
            view.scalar.implicit_projection_scalar_jacobian_v.block(),
            Some(&Compiler(native)),
            &refresh_projection::projection_jacobian_demand(
                &view.model,
                view.scalar.settled_read_cones.as_deref(),
            )
            .unwrap(),
        )
        .unwrap()
    };
    runtime.scalar_fixture_mut().compiled_algebraic_jacobians = prepared;
}

#[test]
fn complete_projection_prepares_once_and_uses_fresh_parameters() {
    let mut runtime = SolveRuntime::new_fixture(&grouped_projection_jvp::coupled_model()).unwrap();
    let native = Rc::new(PreparedNative::default());
    prepare(&mut runtime, native.clone());
    assert_eq!(native.prepares.get(), 1);
    for (iteration, k) in [2.0, -3.0, 0.0].into_iter().enumerate() {
        let mut y = [8.0, -5.0];
        runtime
            .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[k], 1e-10, 4)
            .unwrap();
        assert!((y[0] - (2.0 + k) / (1.0 + k * k)).abs() < 1e-9);
        assert!((y[1] - (2.0 * k - 1.0) / (1.0 + k * k)).abs() < 1e-9);
        assert_eq!(native.calls.get(), iteration + 1);
    }
    assert_eq!(native.prepares.get(), 1);
}

#[test]
fn complete_projection_failure_propagates_without_committing_y() {
    let mut runtime = SolveRuntime::new_fixture(&grouped_projection_jvp::coupled_model()).unwrap();
    let native = Rc::new(PreparedNative {
        fail: true,
        ..Default::default()
    });
    prepare(&mut runtime, native.clone());
    let mut y = [8.0, -5.0];
    let error = runtime
        .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[2.0], 1e-10, 4)
        .unwrap_err();
    assert!(error.to_string().contains("prepared projection failed"));
    assert_eq!(native.calls.get(), 1);
    assert_eq!(y, [8.0, -5.0]);
}

#[test]
fn complete_projection_declines_mixed_reverse_rows_and_full_parameter_seed_space() {
    use crate::runtime::projection::ImplicitProjectionModel;
    let mut runtime = SolveRuntime::new_fixture(&grouped_projection_jvp::coupled_model()).unwrap();
    let native = Rc::new(PreparedNative::default());
    prepare(&mut runtime, native.clone());
    let structure = &runtime
        .scalar_execution()
        .unwrap()
        .scalar
        .continuous_structural
        .algebraic_projection()[0];
    let application = structure.jacobian_application().unwrap();
    let projection = native_projection_jvp::selected_projection(&runtime, true);
    let mut out = [99.0; 4];
    assert!(
        !projection
            .eval_prepared_implicit_jacobian(
                structure,
                (application.rows(), application.y_indices()),
                &[8.0, -5.0],
                &[2.0],
                1.0,
                &mut out,
            )
            .unwrap()
    );
    assert_eq!(out, [99.0; 4]);
    assert_eq!(native.calls.get(), 0);

    let mut programs = runtime
        .scalar_execution()
        .unwrap()
        .scalar
        .implicit_scalar_rhs
        .block()
        .programs()
        .to_vec();
    programs.push(vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]);
    let mixed = PreparedScalarProgramBlock::new(
        solve::ScalarProgramBlock::with_output_indices(
            programs,
            vec![test_span("mixed_projection.mo"); 2],
            vec![0, 2, 1],
        )
        .unwrap(),
    )
    .unwrap();
    assert!(!mixed.reverse_row_y_gradient_supported(0));
    assert!(mixed.reverse_row_y_gradient_supported(1));
    let prepared = refresh_projection::prepare_projection_jacobians(
        &runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .continuous_structural,
        &mixed,
        runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .implicit_projection_scalar_jacobian_v
            .block(),
        Some(&Compiler(native.clone())),
        &refresh_projection::projection_jacobian_demand(
            &runtime.model,
            runtime
                .scalar_execution()
                .unwrap()
                .scalar
                .settled_read_cones
                .as_deref(),
        )
        .unwrap(),
    )
    .unwrap();
    assert!(prepared.iter().all(Option::is_none));
    assert_eq!(native.prepares.get(), 1);
}

#[test]
fn complete_projection_declines_a_separately_materialized_source_view() {
    let runtime = SolveRuntime::new_fixture(&grouped_projection_jvp::coupled_model()).unwrap();
    let original = runtime
        .scalar_execution()
        .unwrap()
        .scalar
        .implicit_projection_scalar_jacobian_v
        .block();
    let materialized = solve::ScalarProgramBlock::with_output_indices(
        original.programs().to_vec(),
        original.program_spans().to_vec(),
        original.output_indices().to_vec(),
    )
    .unwrap();
    let native = Rc::new(PreparedNative::default());
    let prepared = refresh_projection::prepare_projection_jacobians(
        &runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .continuous_structural,
        &runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .implicit_scalar_rhs,
        &materialized,
        Some(&Compiler(native.clone())),
        &refresh_projection::projection_jacobian_demand(
            &runtime.model,
            runtime
                .scalar_execution()
                .unwrap()
                .scalar
                .settled_read_cones
                .as_deref(),
        )
        .unwrap(),
    )
    .unwrap();
    assert!(prepared.iter().all(Option::is_none));
    assert_eq!(native.prepares.get(), 0);
}

#[test]
fn prepared_projection_rejects_a_replaced_canonical_primal_source() {
    let mut model = grouped_projection_jvp::coupled_model();
    let primal = to_scalar_program_block(&model.problem.continuous.implicit_rhs).unwrap();
    let structure = &mut model.artifacts.continuous.structural;
    let application = structure.algebraic_projection()[0]
        .jacobian_application()
        .unwrap();
    let domain = solve::ProjectionJacobianSeedDomain::derive(application, &primal).unwrap();
    let derivative = application.source().clone();
    let specialized = domain.with_lowered_derivative(derivative).unwrap();
    structure
        .bind_algebraic_jacobian_application(specialized)
        .unwrap();
    assert!(SolveRuntime::new_fixture(&model).is_ok());
    let replaced = solve::ScalarProgramBlock::with_output_indices(
        primal.programs().to_vec(),
        primal.program_spans().to_vec(),
        primal.output_indices().to_vec(),
    )
    .unwrap();
    model.problem.continuous.implicit_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(replaced)],
    };
    let result = SolveRuntime::new_fixture(&model);
    assert!(
        matches!(result, Err(EvalSolveError::InvalidRow { message, .. })
        if message.contains("different canonical primal source"))
    );
}

// The constructor receives a backend whose projection preparation is observable
// and can fail. Other optional compiled entries explicitly decline.
impl SolveExecutionBackend for Compiler {
    fn compile_expression(
        &self,
        _block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        Err("demand fixture uses interpreted values".into())
    }

    fn compile_jacobian_expression(
        &self,
        _block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        Ok(Rc::new(Compiler(self.0.clone())))
    }

    fn compile_assignment_schedule(
        &self,
        _source: &solve::ComputeBlock,
        _owners: &solve::ContinuousRefreshOwners,
        _schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        Err("demand fixture uses interpreted assignments".into())
    }

    fn compile_event_transaction(
        &self,
        _program: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        Err("demand fixture uses interpreted event transactions".into())
    }
}

#[test]
fn declined_transaction_compilation_retains_atomic_interpreted_execution() {
    let model = super::super::event_transactions::tests::transaction_model();
    let native = Rc::new(PreparedNative::default());
    let runtime = runtime_with_preparation_backend(model, native.clone())
        .expect("optional transaction compilation may decline");
    assert_eq!(
        runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .compiled_event_transactions
            .len(),
        1
    );
    assert!(
        runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .compiled_event_transactions[0]
            .is_none()
    );
    let mut y = [];
    let mut p = [4.5, 0.0, 9.0, 9.0];
    let before = p;
    let failed = runtime
        .execute_event_transaction(0, &mut y, &mut p, 0.0)
        .unwrap();
    assert_eq!(failed.failed_assertion, Some(0));
    assert!(!failed.changed);
    assert_eq!(
        p, before,
        "a declined backend cannot bypass atomic assertion failure"
    );

    p[1] = 1.0;
    let committed = runtime
        .execute_event_transaction(0, &mut y, &mut p, 0.0)
        .unwrap();
    assert_eq!(committed.failed_assertion, None);
    assert!(committed.changed);
    assert_eq!(p, [4.5, 1.0, 4.5, 9.0]);
    assert_eq!(
        native.prepares.get(),
        0,
        "this transaction needs no projection JVP"
    );
}

fn causal_grouped_model() -> solve::SolveModel {
    use solve::{BinaryOp as B, LinearOp as L};
    let mut model = grouped_projection_jvp::coupled_model();
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![vec![
                L::LoadY { dst: 0, index: 0 },
                L::LoadY { dst: 1, index: 1 },
                L::Const { dst: 2, value: 2.0 },
                L::Const { dst: 3, value: 1.0 },
                L::Binary {
                    dst: 4,
                    op: B::Sub,
                    lhs: 0,
                    rhs: 2,
                },
                L::Binary {
                    dst: 5,
                    op: B::Sub,
                    lhs: 1,
                    rhs: 3,
                },
                L::StoreOutputRange {
                    start: 4,
                    count: 2,
                    stride: 1,
                },
            ]],
            "causal_grouped.mo",
        ));
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))].into();
    set_test_implicit_jvp(
        &mut model,
        vec![vec![
            L::LoadSeed { dst: 0, index: 0 },
            L::LoadSeed { dst: 1, index: 1 },
            L::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ]],
        "causal_grouped_jvp.mo",
    );
    set_causal_test_projection_plan(&mut model);
    derive_test_structural_artifacts(&mut model);
    model
}

fn runtime_with_preparation_backend(
    mut model: solve::SolveModel,
    native: Rc<PreparedNative>,
) -> Result<SolveRuntime, EvalSolveError> {
    model.problem.continuous.refresh_owners =
        solve_eval::refresh_plan::build_continuous_refresh_owners(&mut model.problem)?;
    SolveRuntime::new_with_execution_backend(&model, Some(Rc::new(Compiler(native))))
}

#[test]
fn projection_demand_causal_certificate_omits_native_applications() {
    let native = Rc::new(PreparedNative {
        preparation_fail: true,
        ..Default::default()
    });
    let runtime = runtime_with_preparation_backend(causal_grouped_model(), native.clone())
        .expect("a certified value schedule never invokes projection preparation");
    assert!(
        runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .algebraic_refresh
            .causal_solution_certified
    );
    let structures = runtime
        .scalar_execution()
        .unwrap()
        .scalar
        .continuous_structural
        .algebraic_projection();
    assert_eq!(structures.len(), 2);
    assert!(
        structures
            .iter()
            .all(|structure| structure.jacobian_application().is_some())
    );
    assert!(
        !runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .implicit_scalar_rhs
            .reverse_row_y_gradient_supported(0)
    );
    assert_eq!(native.prepares.get(), 0);
    assert!(
        runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .compiled_algebraic_jacobians
            .iter()
            .all(Option::is_none)
    );
    let mut y = [8.0, -5.0];
    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut y, &[2.0], 1e-10, 4)
        .unwrap();
    assert_eq!(y, [2.0, 1.0]);
}

#[test]
fn projection_demand_uncertified_preparation_failure_remains_an_error() {
    let native = Rc::new(PreparedNative {
        preparation_fail: true,
        ..Default::default()
    });
    let error =
        runtime_with_preparation_backend(grouped_projection_jvp::coupled_model(), native.clone())
            .err()
            .expect("a required native application failure must propagate");
    assert!(
        error
            .to_string()
            .contains("required projection preparation failed")
    );
    assert_eq!(native.prepares.get(), 1);
}

#[test]
fn projection_demand_preserves_initialization_settled_read_cones() {
    let mut model = causal_grouped_model();
    model.problem.initialization =
        solve::InitializationSolveSystem::construct(solve::InitializationSystemInput {
            residual: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                vec![shifted_variable_residual_row(0, 2.0)],
                "settled_read_demand.mo",
            )),
            row_roles: vec![solve::InitializationRowRole::SurplusAlgebraicCheck],
            ..Default::default()
        })
        .unwrap();
    model.artifacts.initialization.residual_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            "settled_read_demand_jvp.mo",
        ));
    derive_test_structural_artifacts(&mut model);
    let native = Rc::new(PreparedNative::default());
    let runtime = runtime_with_preparation_backend(model, native.clone()).unwrap();
    assert!(
        runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .algebraic_refresh
            .causal_solution_certified
    );
    assert_eq!(
        runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .settled_read_cones
            .as_deref()
            .unwrap(),
        [vec![0].into_boxed_slice()]
    );
    assert_eq!(native.prepares.get(), 1);
    assert!(
        runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .compiled_algebraic_jacobians[0]
            .is_some()
    );
    assert!(
        runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .compiled_algebraic_jacobians[1]
            .is_none()
    );
}

#[test]
fn projection_demand_refuses_foreign_initialization_cone_positions() {
    let runtime = SolveRuntime::new_fixture(&causal_grouped_model()).unwrap();
    let cones = [vec![usize::MAX].into_boxed_slice()];
    let error =
        refresh_projection::projection_jacobian_demand(&runtime.model, Some(&cones)).unwrap_err();
    assert!(matches!(error, EvalSolveError::InvalidRow { message, .. }
        if message.contains("initialization read cone is outside")));
}
