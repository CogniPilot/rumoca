//! Admission and dispatch witnesses through the production owner preparation.
//! Handwritten callbacks test routing, source coordinates, counters and faults;
//! native code generation is covered by the independent backend suites.
use super::*;

#[derive(Clone)]
struct FixtureExpression {
    source: solve::ScalarProgramBlock,
    whole: Rc<dyn CompiledSolveExpression>,
    program: Option<usize>,
    reject_selected: bool,
}

fn fixture_decline(span: Option<rumoca_core::Span>) -> RuntimeSolveError {
    RuntimeSolveError::CompiledExecution {
        context: "complete fixture kernel",
        message: "accepted fixture kernel declined numeric execution".into(),
        source_spans: span.into_iter().collect(),
    }
}

impl CompiledSolveExpression for FixtureExpression {
    fn bind_program(
        &self,
        source: &solve::ScalarProgramBlock,
        program: usize,
    ) -> Result<Option<Rc<dyn CompiledSolveExpression>>, String> {
        if !self.source.shares_program_owner(source) || source.program(program).is_none() {
            return Err("fixture selected source owner differs".into());
        }
        if self.reject_selected {
            return Ok(None);
        }
        Ok(Some(Rc::new(Self {
            program: Some(program),
            ..self.clone()
        })))
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let Some(program) = self.program else {
            return self.whole.call(y, p, t, tables, out);
        };
        let mut values = Vec::new();
        if self
            .whole
            .call_program_outputs(program, y, p, t, tables, &mut values)?
        {
            if values.len() != out.len() {
                return Err(fixture_decline(self.source.program_span(program)));
            }
            out.copy_from_slice(&values);
            return Ok(());
        }
        for (offset, value) in out.iter_mut().enumerate() {
            *value = self
                .whole
                .call_program_output((program, offset), y, p, t, tables)?
                .ok_or_else(|| fixture_decline(self.source.program_span(program)))?;
        }
        Ok(())
    }

    fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.whole.call_program_output(coordinate, y, p, t, tables)
    }

    fn call_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, RuntimeSolveError> {
        self.whole
            .call_program_outputs(program, y, p, t, tables, out)
    }
}

#[derive(Clone)]
struct FixtureJacobian {
    source: solve::ScalarProgramBlock,
    whole: Rc<dyn CompiledSolveJacobianExpression>,
    program: Option<usize>,
}

impl CompiledSolveJacobianExpression for FixtureJacobian {
    fn bind_program(
        &self,
        source: &solve::ScalarProgramBlock,
        program: usize,
    ) -> Result<Option<Rc<dyn CompiledSolveJacobianExpression>>, String> {
        if !self.source.shares_program_owner(source) || source.program(program).is_none() {
            return Err("fixture selected source owner differs".into());
        }
        Ok(Some(Rc::new(Self {
            program: Some(program),
            ..self.clone()
        })))
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let Some(program) = self.program else {
            return self.whole.call(y, p, t, seed, tables, out);
        };
        let mut values = Vec::new();
        if self.whole.call_program_outputs(
            program,
            solve_eval::JacobianEvalInputs { y, p, t, seed },
            tables,
            &mut values,
        )? {
            if values.len() != out.len() {
                return Err(fixture_decline(self.source.program_span(program)));
            }
            out.copy_from_slice(&values);
            return Ok(());
        }
        for (offset, value) in out.iter_mut().enumerate() {
            *value = self
                .whole
                .call_program_output((program, offset), y, p, t, seed, tables)?
                .ok_or_else(|| fixture_decline(self.source.program_span(program)))?;
        }
        Ok(())
    }

    fn prepare_projections(
        &self,
        applications: &[&solve::ProjectionJacobianApplication],
    ) -> Result<Vec<Option<Rc<dyn CompiledSolveProjectionJacobian>>>, String> {
        self.whole.prepare_projections(applications)
    }

    fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.whole
            .call_program_output(coordinate, y, p, t, seed, tables)
    }

    fn call_program_outputs(
        &self,
        program: usize,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, RuntimeSolveError> {
        self.whole
            .call_program_outputs(program, inputs, tables, out)
    }
}

enum FixtureKernel {
    Expression(FixtureExpression),
    Jacobian(FixtureJacobian),
}
struct FixtureBackend(FixtureKernel);

impl SolveExecutionBackend for FixtureBackend {
    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        match &self.0 {
            FixtureKernel::Expression(kernel) if kernel.source.shares_program_owner(source) => {
                Ok(Rc::new(kernel.clone()))
            }
            _ => Err("fixture refused a required independently lowered selected program".into()),
        }
    }
    fn compile_jacobian_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        match &self.0 {
            FixtureKernel::Jacobian(kernel) if kernel.source.shares_program_owner(source) => {
                Ok(Rc::new(kernel.clone()))
            }
            _ => Err("fixture refused a required independently lowered selected JVP".into()),
        }
    }
}

pub(in crate::runtime::solve_runtime) fn prepare_mock_primal(
    source: solve::ScalarProgramBlock,
    whole: Rc<dyn CompiledSolveExpression>,
    reject_selected: bool,
) -> Result<PrimalProgram, EvalSolveError> {
    let backend: Rc<dyn SolveExecutionBackend> = Rc::new(FixtureBackend(
        FixtureKernel::Expression(FixtureExpression {
            source: source.clone(),
            whole,
            program: None,
            reject_selected,
        }),
    ));
    PrimalProgram::prepare("mock implicit primal", source, Some(&backend), None)
}

pub(in crate::runtime::solve_runtime) fn install_mock_primal(
    runtime: &mut SolveRuntime,
    whole: Rc<dyn CompiledSolveExpression>,
) {
    let source = {
        let view = runtime.scalar_execution().unwrap();
        to_scalar_program_block(&view.model.problem.continuous.implicit_rhs).unwrap()
    };
    runtime.scalar_fixture_mut().implicit_scalar_rhs =
        prepare_mock_primal(source, whole, false).unwrap();
}

fn jacobian_source(runtime: &SolveRuntime, full: bool) -> solve::ScalarProgramBlock {
    let view = runtime.scalar_execution().unwrap();
    if full {
        view.model
            .artifacts
            .continuous
            .implicit_jacobian_v_scalar
            .clone()
    } else {
        refresh_projection::projection_jacobian_source(
            &view.model.artifacts.continuous.implicit_jacobian_v,
            &view.scalar.continuous_structural,
        )
        .unwrap()
    }
}

pub(in crate::runtime::solve_runtime) fn prepare_mock_jacobian(
    source: solve::ScalarProgramBlock,
    whole: Rc<dyn CompiledSolveJacobianExpression>,
) -> DirectionalProgram {
    let backend: Rc<dyn SolveExecutionBackend> =
        Rc::new(FixtureBackend(FixtureKernel::Jacobian(FixtureJacobian {
            source: source.clone(),
            whole,
            program: None,
        })));
    DirectionalProgram::prepare("mock implicit JVP", source, Some(&backend), None).unwrap()
}

pub(in crate::runtime::solve_runtime) fn install_mock_jacobian(
    runtime: &mut SolveRuntime,
    full: bool,
    whole: Rc<dyn CompiledSolveJacobianExpression>,
) {
    let owner = prepare_mock_jacobian(jacobian_source(runtime, full), whole);
    if full {
        runtime.scalar_fixture_mut().implicit_jacobian_v = owner;
    } else {
        runtime
            .scalar_fixture_mut()
            .implicit_projection_scalar_jacobian_v = owner;
    }
}

pub(in crate::runtime::solve_runtime) fn reset_mock_jacobian(
    runtime: &mut SolveRuntime,
    full: bool,
) {
    let owner = DirectionalProgram::prepare(
        "reference implicit JVP",
        jacobian_source(runtime, full),
        None,
        None,
    )
    .unwrap();
    if full {
        runtime.scalar_fixture_mut().implicit_jacobian_v = owner;
    } else {
        runtime
            .scalar_fixture_mut()
            .implicit_projection_scalar_jacobian_v = owner;
    }
}

pub(in crate::runtime::solve_runtime) fn install_mock_initial_primal(
    runtime: &mut SolveRuntime,
    whole: Rc<dyn CompiledSolveExpression>,
) {
    let source = to_scalar_program_block(
        runtime
            .scalar_execution()
            .unwrap()
            .model
            .problem
            .initialization
            .residual(),
    )
    .unwrap();
    runtime.scalar_fixture_mut().initial_scalar_residual =
        prepare_mock_primal(source, whole, false).unwrap();
}

pub(in crate::runtime::solve_runtime) fn install_mock_initial_jacobian(
    runtime: &mut SolveRuntime,
    whole: Rc<dyn CompiledSolveJacobianExpression>,
) {
    let source = to_scalar_program_block(
        &runtime
            .scalar_execution()
            .unwrap()
            .model
            .artifacts
            .initialization
            .residual_jacobian_v,
    )
    .unwrap();
    let backend: Rc<dyn SolveExecutionBackend> =
        Rc::new(FixtureBackend(FixtureKernel::Jacobian(FixtureJacobian {
            source: source.clone(),
            whole,
            program: None,
        })));
    runtime.scalar_fixture_mut().initial_residual_jacobian_v =
        EvaluationDirectional::prepare("mock initial JVP", source, None, Some(&backend), None)
            .unwrap();
}
