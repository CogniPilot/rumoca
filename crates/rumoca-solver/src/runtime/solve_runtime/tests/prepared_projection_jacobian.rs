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

#[derive(Default)]
struct PreparedNative {
    prepares: Cell<usize>,
    calls: Cell<usize>,
    fail: bool,
}

struct Compiler(Rc<PreparedNative>);

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
        if self.fail {
            return Err("prepared projection failed".into());
        }
        out.copy_from_slice(&[1.0, p[0], p[0], -1.0]);
        Ok(())
    }
}

fn prepare(runtime: &mut SolveRuntime, native: Rc<PreparedNative>) {
    runtime.compiled_algebraic_jacobians = refresh_projection::prepare_projection_jacobians(
        &runtime.continuous_structural,
        &runtime.implicit_scalar_rhs,
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
    let structure = &runtime.continuous_structural.algebraic_projection()[0];
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

    let mut programs = runtime.implicit_scalar_rhs.block().programs().to_vec();
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
        &runtime.continuous_structural,
        &mixed,
        runtime.implicit_projection_scalar_jacobian_v.block(),
        Some(&Compiler(native.clone())),
    )
    .unwrap();
    assert!(prepared.iter().all(Option::is_none));
    assert_eq!(native.prepares.get(), 1);
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
        &runtime.implicit_scalar_rhs,
        &materialized,
        Some(&Compiler(native.clone())),
    )
    .unwrap();
    assert!(prepared.iter().all(Option::is_none));
    assert_eq!(native.prepares.get(), 0);
}
