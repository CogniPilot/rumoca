//! A source-derived coupled Jacobian uses the admitted native application.

use rumoca::Compiler;
use rumoca_core::ExternalTableData;
use rumoca_exec_cranelift as native;
use rumoca_ir_solve as solve;
use rumoca_solver as runtime;
use std::{cell::Cell, rc::Rc};

#[derive(Default)]
struct Backend {
    calls: Rc<Cell<usize>>,
    batches: Rc<Cell<usize>>,
    programs: Rc<Cell<usize>>,
    pure_calls: Option<native::CompiledPureCallTable>,
}

struct Jacobian(native::CompiledJacobianV, Rc<Cell<usize>>);
struct Projection(native::CompiledProjectionJacobian, Rc<Cell<usize>>);

impl runtime::CompiledSolveJacobianExpression for Jacobian {
    fn prepare_projection(
        &self,
        application: &solve::ProjectionJacobianApplication,
    ) -> Result<Option<Rc<dyn runtime::CompiledSolveProjectionJacobian>>, String> {
        self.0
            .prepare_projection(application)
            .map(|value| Some(Rc::new(Projection(value, self.1.clone())) as Rc<_>))
            .map_err(|error| error.to_string())
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String> {
        self.0
            .call_with_external_tables(y, p, t, seed, tables, out)
            .map_err(|error| error.to_string())
    }
}

impl runtime::CompiledSolveProjectionJacobian for Projection {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String> {
        self.1.set(self.1.get() + 1);
        self.0
            .call(y, p, t, tables, out)
            .map_err(|error| error.to_string())
    }
}

impl runtime::SolveExecutionBackend for Backend {
    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn runtime::CompiledSolveExpression>, String> {
        native::compile_selectable_expression_scalar_program_block(block, self.pure_calls.as_ref())
            .map(|compiled| {
                Rc::new(Residual(
                    compiled,
                    self.batches.clone(),
                    self.programs.clone(),
                )) as Rc<_>
            })
            .map_err(|error| error.to_string())
    }

    fn compile_jacobian_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn runtime::CompiledSolveJacobianExpression>, String> {
        native::compile_jacobian_scalar_program_block(block)
            .map(|value| Rc::new(Jacobian(value, self.calls.clone())) as Rc<_>)
            .map_err(|error| error.to_string())
    }

    fn compile_assignment_schedule(
        &self,
        _source: &solve::ComputeBlock,
        _owners: &solve::ContinuousRefreshOwners,
        _schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn runtime::CompiledSolveAssignmentSchedule>, String> {
        Err("fixture interprets assignments".into())
    }

    fn compile_event_transaction(
        &self,
        _program: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn runtime::CompiledSolveEventTransaction>, String> {
        Err("fixture interprets events".into())
    }
}

#[test]
fn source_coupled_projection_uses_native_jacobian_with_reverse_capable_rows() {
    let source = r#"
model CoupledProjection
  Real x(start=1, fixed=true);
  Real a;
  Real b;
equation
  der(x) = -x;
  a = x + b;
  b = 0.25*a;
end CoupledProjection;
"#;
    let compiled = Compiler::new()
        .model("CoupledProjection")
        .compile_str(source, "coupled_projection.mo")
        .unwrap();
    let model =
        rumoca_sim::lower_dae_for_simulation(&compiled.dae, &rumoca_sim::SimOptions::default())
            .unwrap();
    let primal = rumoca_eval_solve::PreparedScalarProgramBlock::new(
        rumoca_eval_solve::to_scalar_program_block(&model.problem.continuous.implicit_rhs).unwrap(),
    )
    .unwrap();
    assert!(
        model
            .problem
            .continuous
            .algebraic_projection_plan
            .blocks
            .iter()
            .any(|block| {
                block.rows.len() > 1
                    && block.rows.iter().all(|&row| {
                        primal.row_output_position(row).is_some_and(|(program, _)| {
                            primal.reverse_row_y_gradient_supported(program)
                        })
                    })
            })
    );
    assert!(
        model
            .artifacts
            .continuous
            .structural
            .algebraic_projection()
            .iter()
            .any(|s| {
                s.residual_output_evaluation().is_some_and(|selection| {
                    selection.programs().len() > 1
                        && selection
                            .programs()
                            .iter()
                            .all(|p| p.output_count() == 1 && p.placements().len() == 1)
                })
            })
    );
    let backend = Rc::new(Backend {
        pure_calls: Some(native::compile_pure_call_table(&model.pure_calls).unwrap()),
        ..Default::default()
    });
    let prepared =
        runtime::SolveRuntime::new_with_execution_backend(&model, Some(backend.clone())).unwrap();
    let interpreted = runtime::SolveRuntime::new(&model).unwrap();
    let names = &model.problem.solve_layout.solver_maps.names;
    let index = |name: &str| names.iter().position(|value| value == name).unwrap();
    for (t, x) in [(0.0, 1.0), (0.5, -3.0), (1.0, 0.0)] {
        let mut expected = vec![8.0; names.len()];
        expected[index("x")] = x;
        let mut actual = expected.clone();
        let before = backend.calls.get();
        let before_batches = backend.batches.get();
        interpreted
            .refresh_algebraic_and_output_slots_certified(
                t,
                &mut expected,
                &model.parameters,
                1e-10,
                4,
            )
            .unwrap();
        prepared
            .refresh_algebraic_and_output_slots_certified(
                t,
                &mut actual,
                &model.parameters,
                1e-10,
                4,
            )
            .unwrap();
        assert_eq!(actual, expected);
        assert!(
            backend.batches.get() > before_batches,
            "distinct scalar residuals use the existing native batch"
        );
        assert!((actual[index("a")] - 4.0 * x / 3.0).abs() < 1e-12);
        assert!((actual[index("b")] - x / 3.0).abs() < 1e-12);
        assert!(
            backend.calls.get() > before,
            "source Jacobian must use its admitted native application"
        );
    }
}

struct Residual(
    native::CompiledExpressionRows,
    Rc<Cell<usize>>,
    Rc<Cell<usize>>,
);

impl runtime::CompiledSolveExpression for Residual {
    fn call_projection_outputs(
        &self,
        selection: &solve::ProjectionOutputSelection,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<bool, String> {
        let admitted = self
            .0
            .call_projection_outputs(selection, y, p, t, tables, out)
            .map_err(|e| e.to_string())?;
        self.1.set(self.1.get() + usize::from(admitted));
        Ok(admitted)
    }
    fn call_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, String> {
        let admitted = self
            .0
            .call_program_outputs(program, y, p, t, tables, out)
            .map_err(|e| e.to_string())?;
        self.2.set(self.2.get() + usize::from(admitted));
        Ok(admitted)
    }
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String> {
        self.0
            .call_with_external_tables(y, p, t, tables, out)
            .map_err(|e| e.to_string())
    }
}

#[test]
fn source_tensor_projection_batches_existing_residual_programs() {
    let source = r#"
package BatchProjection
  function transform
    input Real R[2,2];
    input Real v[2];
    output Real w[2];
  algorithm
    w := transpose(R)*v;
    annotation(Inline=false);
  end transform;
  model Probe
    Real x(start=1, fixed=true);
    Real a[2];
    Real b[2];
  equation
    der(x) = -x;
    a = transform([x,0.25;0.5,2],b) + {x,1};
    b = {0.25*a[1],0.125*a[2]};
  end Probe;
end BatchProjection;
"#;
    let compiled = Compiler::new()
        .model("BatchProjection.Probe")
        .compile_str(source, "batch_projection.mo")
        .unwrap();
    let model = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &Default::default()).unwrap();
    assert!(
        model
            .artifacts
            .continuous
            .structural
            .algebraic_projection()
            .iter()
            .any(|block| {
                block.residual_output_evaluation().is_some_and(|selection| {
                    selection.programs().len() > 1
                        && selection.programs().iter().any(|p| p.output_count() > 1)
                })
            })
    );
    let backend = Rc::new(Backend {
        pure_calls: Some(native::compile_pure_call_table(&model.pure_calls).unwrap()),
        ..Default::default()
    });
    let prepared =
        runtime::SolveRuntime::new_with_execution_backend(&model, Some(backend.clone())).unwrap();
    let interpreted = runtime::SolveRuntime::new(&model).unwrap();
    let names = &model.problem.solve_layout.solver_maps.names;
    let index = |name: &str| names.iter().position(|n| n == name).unwrap();
    for (t, x) in [(0.0, 1.0), (0.5, -3.0), (1.0, 0.0)] {
        let mut expected = vec![8.0; names.len()];
        expected[index("x")] = x;
        let mut actual = expected.clone();
        let before = backend.batches.get();
        interpreted
            .refresh_algebraic_and_output_slots_certified(
                t,
                &mut expected,
                &model.parameters,
                1e-10,
                4,
            )
            .unwrap();
        prepared
            .refresh_algebraic_and_output_slots_certified(
                t,
                &mut actual,
                &model.parameters,
                1e-10,
                4,
            )
            .unwrap();
        assert_eq!(actual, expected);
        let (a1, a2, b1, b2) = (
            actual[index("a[1]")],
            actual[index("a[2]")],
            actual[index("b[1]")],
            actual[index("b[2]")],
        );
        assert!((a1 - x * b1 - 0.5 * b2 - x).abs() < 1e-12);
        assert!((a2 - 0.25 * b1 - 2.0 * b2 - 1.0).abs() < 1e-12);
        assert!((b1 - 0.25 * a1).abs() < 1e-12);
        assert!((b2 - 0.125 * a2).abs() < 1e-12);
        assert!(
            backend.batches.get() > before,
            "source residual selection must enter the batch adapter; admitted individual calls={}",
            backend.programs.get()
        );
    }
    assert_eq!(
        backend.programs.get(),
        0,
        "selected programs must share one adapter call"
    );
}
