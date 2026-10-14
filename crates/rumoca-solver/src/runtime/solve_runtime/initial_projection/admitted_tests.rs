use super::super::tests::warm_start_test_model;
use super::*;
use crate::runtime::solve_runtime::tests::{
    install_mock_initial_jacobian, install_mock_initial_primal, install_mock_jacobian,
    install_mock_primal,
};
use std::cell::Cell;

struct Fault(Rc<Cell<usize>>);
impl CompiledSolveExpression for Fault {
    fn call(
        &self,
        _: &[f64],
        _: &[f64],
        _: f64,
        _: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.0.set(self.0.get() + 1);
        out[0] = 777.0;
        Err("first initial hook fault".into())
    }
}
impl CompiledSolveJacobianExpression for Fault {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        _: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        CompiledSolveExpression::call(self, y, p, t, tables, out)
    }
}

fn initial_fault_model() -> solve::SolveModel {
    let mut model = warm_start_test_model();
    let block = |name: &'static str, seed: bool| {
        let span =
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(name), 1, 2);
        let rows = (0..2)
            .map(|index| {
                vec![
                    if seed {
                        solve::LinearOp::LoadSeed { dst: 0, index }
                    } else {
                        solve::LinearOp::LoadY { dst: 0, index }
                    },
                    solve::LinearOp::StoreOutput { src: 0 },
                ]
            })
            .collect();
        solve::ComputeBlock::from_scalar_program_block(
            solve::ScalarProgramBlock::with_source_span(
                rows,
                span.require_provenance("initial fault fixture").unwrap(),
            )
            .unwrap(),
        )
    };
    model.problem.initialization =
        solve::InitializationSolveSystem::construct(solve::InitializationSystemInput {
            residual: block("initial_fault_residual.mo", false),
            row_roles: vec![solve::InitializationRowRole::Solved; 2],
            projection_plan: solve::InitializationProjectionPlan {
                blocks: vec![solve::InitializationProjectionBlock {
                    rows: vec![0, 1],
                    unknowns: vec![solve::scalar_slot_y(0), solve::scalar_slot_y(1)],
                    scales: vec![solve::InitializationUnknownScale::Solver; 2],
                }],
                iterates_discretes: false,
            },
            ..Default::default()
        })
        .unwrap();
    model.artifacts.initialization.residual_jacobian_v = block("initial_fault_jvp.mo", true);
    crate::test_fixtures::derive_structural_artifacts(&mut model);
    model
}

#[test]
fn admitted_initial_full_and_partial_faults_preserve_output_bits() {
    let model = initial_fault_model();
    assert_eq!(model.problem.initialization.residual().len().unwrap(), 2);
    assert_eq!(
        model
            .artifacts
            .initialization
            .residual_jacobian_v
            .len()
            .unwrap(),
        2
    );
    for owner in 0..4 {
        let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
        let calls = Rc::new(Cell::new(0));
        let compiled = Rc::new(Fault(calls.clone()));
        match owner {
            0 => install_mock_primal(&mut runtime, compiled),
            1 => install_mock_jacobian(&mut runtime, false, compiled),
            2 => install_mock_initial_primal(&mut runtime, compiled),
            _ => install_mock_initial_jacobian(&mut runtime, compiled),
        }
        let projection = InitialProjectionModel {
            runtime: runtime.scalar_execution().unwrap(),
            tol: 1e-10,
            max_iters: 16,
            refreshes_algebraic_reads: false,
        };
        let mut out = [-0.0_f64, f64::from_bits(0x7ff8_0000_0000_0123)];
        let before = out.map(f64::to_bits);
        let error = match owner {
            0 => projection.eval_residual(&[1.0, 2.0], &[], 0.0, &mut out),
            1 => projection.eval_jacobian_v(&[1.0, 2.0], &[], 0.0, &[1.0, 1.0], &mut out),
            2 => projection.eval_initial_residual(&[1.0, 2.0], &[], 0.0, None, &mut out),
            _ => projection.eval_partial_initial_jacobian_v(
                &[1.0, 2.0],
                &[],
                0.0,
                &[1.0, 1.0],
                &mut out,
            ),
        }
        .expect_err("an admitted initial hook fault must propagate");
        assert!(
            matches!(&error, RuntimeSolveError::CompiledExecution { .. }),
            "owner {owner}: {error:?}, calls {}",
            calls.get()
        );
        assert!(error.to_string().contains("first initial hook fault"));
        assert_eq!(calls.get(), 1);
        assert_eq!(out.map(f64::to_bits), before);
    }
}
