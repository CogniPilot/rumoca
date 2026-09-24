use super::*;
use crate::runtime::projection::JacobianMatrix;
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

#[derive(Default)]
pub(super) struct PreparedNative {
    prepares: Cell<usize>,
    pub(super) calls: Cell<usize>,
    incoming: Cell<[u64; 4]>,
    fail: bool,
    corrupt: bool,
}

struct Compiler(Rc<PreparedNative>);

impl PreparedNative {
    pub(super) fn failing() -> Self {
        Self {
            fail: true,
            ..Self::default()
        }
    }
}

impl CompiledSolveJacobianExpression for Compiler {
    fn prepare_projection(
        &self,
        _application: &solve::ProjectionJacobianApplication,
    ) -> Result<Option<Rc<dyn CompiledSolveProjectionJacobian>>, String> {
        self.0.prepares.set(self.0.prepares.get() + 1);
        Ok(Some(self.0.clone()))
    }

    fn call(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _seed: &[f64],
        _tables: &[rumoca_core::ExternalTableData],
        _out: &mut [f64],
    ) -> Result<(), String> {
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
    ) -> Result<(), String> {
        self.calls.set(self.calls.get() + 1);
        self.incoming
            .set(std::array::from_fn(|index| out[index].to_bits()));
        if self.fail {
            out.fill(f64::NAN);
            return Err("prepared projection failed".into());
        }
        out.copy_from_slice(&[1.0, p[0], p[0], -1.0]);
        if self.corrupt {
            for value in out {
                *value *= 1e8;
            }
        }
        Ok(())
    }
}

pub(super) fn prepare(runtime: &mut SolveRuntime, native: Rc<PreparedNative>) {
    runtime.compiled_algebraic_jacobians = refresh_projection::prepare_projection_jacobians(
        &runtime.continuous_structural,
        runtime.implicit_projection_scalar_jacobian_v.block(),
        Some(&Compiler(native)),
    )
    .unwrap();
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
fn affine_refresh_reuses_matrix_allocation_but_evaluates_each_point() {
    use crate::runtime::projection::jacobian_allocation_count;
    let mut runtime = SolveRuntime::new_fixture(&grouped_projection_jvp::coupled_model()).unwrap();
    let native = Rc::new(PreparedNative::default());
    prepare(&mut runtime, native.clone());
    let mut warmed_count = 0;
    for (iteration, k) in [2.0, -3.0, 0.0, 4.0].into_iter().enumerate() {
        let mut incoming = [0.0; 4];
        if let Some(matrix) = runtime.affine_jacobian_storage[0].borrow_mut().as_mut() {
            incoming = [f64::NAN, 99.0, -37.0, f64::NAN];
            matrix.as_mut_slice().copy_from_slice(&incoming);
        }
        let mut y = [8.0, -5.0];
        runtime
            .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[k], 1e-10, 4)
            .unwrap();
        assert!((y[0] + k * y[1] - 2.0).abs() < 1e-10);
        assert!((k * y[0] - y[1] - 1.0).abs() < 1e-10);
        assert_eq!(native.calls.get(), iteration + 1);
        assert_eq!(
            native.incoming.get(),
            incoming.map(f64::to_bits),
            "complete provider receives reused storage without an outer preclear"
        );
        assert_eq!(
            runtime.affine_jacobian_storage[0]
                .borrow()
                .as_ref()
                .unwrap()
                .as_slice(),
            &[1.0, k, k, -1.0],
            "successful provider overwrites every entry, including newly zero derivatives"
        );
        if iteration == 0 {
            warmed_count = jacobian_allocation_count();
        } else {
            assert_eq!(jacobian_allocation_count(), warmed_count);
        }
    }
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
    let recovered = Rc::new(PreparedNative::default());
    prepare(&mut runtime, recovered.clone());
    runtime
        .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[0.0], 1e-10, 4)
        .unwrap();
    assert_eq!(recovered.incoming.get(), [f64::NAN.to_bits(); 4]);
    assert_eq!(recovered.calls.get(), 1);
    assert_eq!(y, [2.0, -1.0]);
}

#[test]
fn complete_projection_declines_missing_application_or_full_parameter_seed_space() {
    use crate::runtime::projection::ImplicitProjectionModel;
    let mut runtime = SolveRuntime::new_fixture(&grouped_projection_jvp::coupled_model()).unwrap();
    let native = Rc::new(PreparedNative::default());
    prepare(&mut runtime, native.clone());
    let structure = &runtime.continuous_structural.algebraic_projection()[0];
    let application = structure.jacobian_application().unwrap();
    let without_application = solve::ContinuousStructuralArtifacts::derived(
        None,
        vec![structure.pattern().clone()],
        vec![false],
        None,
        vec![],
        None,
    );
    let bare = &without_application.algebraic_projection()[0];
    assert!(bare.jacobian_application().is_none());
    for (candidate, full_seed) in [(structure, true), (bare, false)] {
        let projection = native_projection_jvp::selected_projection(&runtime, full_seed);
        let mut out = [99.0; 4];
        assert!(
            !projection
                .eval_prepared_implicit_jacobian(
                    candidate,
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
    }
}

#[test]
fn complete_projection_declines_a_separately_materialized_source_view() {
    let runtime = SolveRuntime::new_fixture(&grouped_projection_jvp::coupled_model()).unwrap();
    let original = runtime.implicit_projection_scalar_jacobian_v.block();
    let materialized = solve::ScalarProgramBlock::with_output_indices(
        original.programs().to_vec(),
        original.program_spans().to_vec(),
        original.output_indices().to_vec(),
    )
    .unwrap();
    let native = Rc::new(PreparedNative::default());
    let prepared = refresh_projection::prepare_projection_jacobians(
        &runtime.continuous_structural,
        &materialized,
        Some(&Compiler(native.clone())),
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

pub(super) fn mixed_coupled_model() -> solve::SolveModel {
    let mut model = grouped_projection_jvp::coupled_model();
    let original = to_scalar_program_block(&model.problem.continuous.implicit_rhs).unwrap();
    let mut forward = original.programs()[0].clone();
    *forward.last_mut().unwrap() = solve::LinearOp::StoreOutputRange {
        start: 9,
        count: 1,
        stride: 1,
    };
    let mut reverse = original.programs()[0].clone();
    *reverse.last_mut().unwrap() = solve::LinearOp::StoreOutput { src: 10 };
    let primal = solve::ScalarProgramBlock::with_output_indices(
        vec![forward, reverse],
        vec![test_span("mixed_projection.mo"); 2],
        vec![1, 0],
    )
    .unwrap();
    let prepared = PreparedScalarProgramBlock::new(primal.clone()).unwrap();
    assert!(!prepared.reverse_row_y_gradient_supported(0));
    assert!(prepared.reverse_row_y_gradient_supported(1));
    model.problem.continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(primal);
    derive_test_structural_artifacts(&mut model);
    model
}

#[test]
fn complete_mixed_projection_uses_compiled_jvp_and_preserves_certified_solution() {
    let model = mixed_coupled_model();
    let interpreted = SolveRuntime::new_fixture(&model).unwrap();
    let mut compiled = SolveRuntime::new_fixture(&model).unwrap();
    let native = Rc::new(PreparedNative::default());
    prepare(&mut compiled, native.clone());
    for (iteration, k) in [2.0, -3.0, 0.0, 2.0].into_iter().enumerate() {
        let mut expected = [8.0, -5.0];
        let mut actual = expected;
        interpreted
            .refresh_algebraic_and_output_slots_certified(1.0, &mut expected, &[k], 1e-10, 4)
            .unwrap();
        compiled
            .refresh_algebraic_and_output_slots_certified(1.0, &mut actual, &[k], 1e-10, 4)
            .unwrap();
        assert_eq!(actual.map(f64::to_bits), expected.map(f64::to_bits));
        assert!((actual[0] + k * actual[1] - 2.0).abs() < 1e-10);
        assert!((k * actual[0] - actual[1] - 1.0).abs() < 1e-10);
        assert_eq!(native.calls.get(), iteration + 1);
    }
    assert_eq!(native.prepares.get(), 1);
}

#[test]
fn complete_mixed_projection_failure_preserves_coordinates() {
    let mut runtime = SolveRuntime::new_fixture(&mixed_coupled_model()).unwrap();
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
fn complete_mixed_projection_still_requires_the_original_residual_certificate() {
    let mut runtime = SolveRuntime::new_fixture(&mixed_coupled_model()).unwrap();
    let native = Rc::new(PreparedNative {
        corrupt: true,
        ..Default::default()
    });
    prepare(&mut runtime, native.clone());
    let mut y = [8.0, -5.0];
    let result =
        runtime.refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[2.0], 1e-10, 4);
    assert!(
        result.is_err(),
        "an inaccurate native matrix cannot certify source residuals"
    );
    assert!(native.calls.get() > 0);
    assert_eq!(y, [8.0, -5.0]);
    assert!(runtime.affine_jacobian_storage[0].try_borrow_mut().is_ok());
}
