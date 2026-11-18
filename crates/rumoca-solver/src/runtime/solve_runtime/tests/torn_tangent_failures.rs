//! An admitted torn JVP failure must not become reference source execution.
use super::*;
use std::cell::Cell;

#[derive(Default)]
struct SourceProbe {
    calls: Cell<usize>,
}

impl solve_eval::PureCallExecution for SourceProbe {
    fn executes(&self, _: solve_eval::PureCallInvocation<'_>) -> bool {
        true
    }

    fn call(
        &self,
        _: solve_eval::PureCallInvocation<'_>,
        input: &[f64],
        output: &mut [f64],
    ) -> Result<(), EvalSolveError> {
        self.calls.set(self.calls.get() + 1);
        output.copy_from_slice(input);
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Default)]
enum Failure {
    #[default]
    None,
    Source,
    Backend,
}

#[derive(Default)]
struct Backend {
    fail: Rc<Cell<Failure>>,
    native_calls: Rc<Cell<usize>>,
    source: SourceProbe,
}

/// The fixture backend's compiled JVP owner, which also binds the issued rows
/// of a torn tangent, reading the call table `table`.
struct IssuingBackend {
    native_calls: Rc<Cell<usize>>,
    schedule_calls: Rc<Cell<usize>>,
    table: solve::SolvePureCallTable,
}

/// Issued rows run as a compiled schedule runs them: in order over the
/// slots, each reading its seed from the issued seed slots and committing its
/// outputs to their targets before the next row reads them.
struct IssuedRows {
    rows: PreparedScalarProgramBlock,
    count: usize,
    targets: Vec<usize>,
    seed: Option<std::ops::Range<usize>>,
    table: solve::SolvePureCallTable,
    calls: Rc<Cell<usize>>,
}

impl CompiledSolveAssignmentSchedule for IssuedRows {
    fn call(
        &self,
        slots: &mut [f64],
        p: &[f64],
        t: f64,
        _: &[rumoca_core::ExternalTableData],
    ) -> Result<(), RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        let mut targets = self.targets.iter();
        let mut out = Vec::new();
        for row in 0..self.count {
            let seed = self.seed.clone().map(|seed| slots[seed].to_vec());
            let context = RowEvalContext {
                seed: seed.as_deref(),
                pure_calls: Some(&self.table),
                ..Default::default()
            };
            self.rows
                .eval_row_outputs_unchecked_with_context(row, slots, p, t, context, &mut out)?;
            for (value, &target) in out.iter().zip(targets.by_ref()) {
                slots[target] = *value;
            }
        }
        Ok(())
    }
}

struct Kernel {
    indices: Vec<usize>,
    spans: Vec<rumoca_core::Span>,
    fail: Rc<Cell<Failure>>,
    calls: Rc<Cell<usize>>,
}

impl Kernel {
    fn execute(
        &self,
        programs: &[usize],
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        match self.fail.get() {
            Failure::Source => {
                return Err(RuntimeSolveError::SourceFault {
                    message: "admitted torn native source fault".into(),
                    span: Some(self.spans[programs[0]]),
                });
            }
            Failure::Backend => {
                return Err(RuntimeSolveError::CompiledExecution {
                    context: "torn native kernel",
                    message: "admitted torn backend fault".into(),
                    source_spans: vec![self.spans[programs[0]]].into_boxed_slice(),
                });
            }
            Failure::None => {}
        }
        for (&program, value) in programs.iter().zip(out) {
            *value = seed[self.indices[program]];
        }
        Ok(())
    }
}

impl Kernel {
    fn over(
        source: &solve::ScalarProgramBlock,
        fail: &Rc<Cell<Failure>>,
        calls: &Rc<Cell<usize>>,
    ) -> Self {
        let indices = source
            .programs()
            .iter()
            .map(|program| {
                let solve::LinearOp::LoadSeed { index, .. } = program[0] else {
                    panic!("fixture JVP begins with its exact coordinate seed")
                };
                index
            })
            .collect();
        Self {
            indices,
            spans: source.program_spans().to_vec(),
            fail: fail.clone(),
            calls: calls.clone(),
        }
    }
}

impl CompiledSolveJacobianExpression for Kernel {
    fn programs_committing(
        self: std::rc::Rc<Self>,
    ) -> Option<std::rc::Rc<dyn crate::CompiledProgramsCommitting>> {
        None
    }

    fn call_program_outputs(
        &self,
        program: usize,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        _: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, RuntimeSolveError> {
        // A private partial result must not be published on failure.
        out.resize(1, 971.0);
        out.fill(971.0);
        self.execute(&[program], inputs.seed, out)?;
        Ok(true)
    }

    fn call(
        &self,
        _: &[f64],
        _: &[f64],
        _: f64,
        seed: &[f64],
        _: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.execute(&(0..self.indices.len()).collect::<Vec<_>>(), seed, out)
    }
}

impl SolveExecutionBackend for Backend {
    fn pure_call_execution(&self) -> Option<&dyn solve_eval::PureCallExecution> {
        Some(&self.source)
    }

    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, crate::CompileRefusal> {
        super::super::test_kernels::expression(source).map_err(Into::into)
    }

    fn compile_jacobian_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, crate::CompileRefusal> {
        Ok(Rc::new(Kernel::over(
            source,
            &self.fail,
            &self.native_calls,
        )))
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

impl SolveExecutionBackend for IssuingBackend {
    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, crate::CompileRefusal> {
        super::super::test_kernels::expression(source).map_err(Into::into)
    }

    fn compile_jacobian_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, crate::CompileRefusal> {
        Ok(Rc::new(Kernel::over(
            source,
            &Rc::default(),
            &self.native_calls,
        )))
    }

    fn compile_assignment_schedule(
        &self,
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        super::super::test_kernels::assignment(source, owners, schedule)
    }

    fn compile_issued_assignment_rows(
        &self,
        rows: &solve::IssuedAssignmentRows,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        Ok(Rc::new(IssuedRows {
            rows: PreparedScalarProgramBlock::new(rows.rows().clone())
                .map_err(|error| error.to_string())?,
            count: rows.rows().programs().len(),
            targets: rows.targets().to_vec(),
            seed: rows.seed(),
            table: self.table.clone(),
            calls: Rc::clone(&self.schedule_calls),
        }))
    }
}

fn identity_table() -> solve::SolvePureCallTable {
    let profile = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile));
    solve::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![real.clone()],
            vec![solve::SolvePureCallOutput::result(real)],
            test_span("torn_identity.mo"),
            |body, input, output| {
                let value = body.load(input[0], test_span("torn_identity.mo"))?;
                body.store(output[0], value, test_span("torn_identity.mo"))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn model() -> solve::SolveModel {
    let mut model = warm_start_test_model();
    model.problem.solve_layout.state_scalar_count = 0;
    model.problem.solve_layout.algebraic_scalar_count = 2;
    model.problem.continuous.derivative_rhs = Default::default();
    model.artifacts.continuous.full_jacobian_v = Default::default();
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![
                shifted_variable_residual_row(0, 0.0),
                shifted_variable_residual_row(1, 0.0),
            ],
            "torn_identity_residual.mo",
        ));
    model.pure_calls = identity_table();
    let site = model.pure_calls.owners()[0].call_site();
    set_test_implicit_jvp(
        &mut model,
        (0..2)
            .map(|index| {
                vec![
                    solve::LinearOp::LoadSeed { dst: 0, index },
                    solve::LinearOp::PureCall {
                        dst_start: 1,
                        input_starts: vec![0].into(),
                        site: site.clone(),
                    },
                    solve::LinearOp::StoreOutput { src: 1 },
                ]
            })
            .collect(),
        "torn_identity_jvp.mo",
    );
    model.problem.continuous.algebraic_projection_plan.blocks =
        vec![solve::AlgebraicProjectionBlock {
            rows: vec![0, 1],
            y_indices: vec![0, 1],
            tearing: Some(solve::BlockTearing {
                tear_y_indices: vec![0],
                residual_rows: vec![0],
                causal_steps: vec![solve::CausalStep {
                    row: 1,
                    y_index: 1,
                    coefficient: solve::CausalCoefficient::Unit,
                }],
            }),
            alternate_charts: Vec::new(),
        }]
        .into();
    derive_test_structural_artifacts(&mut model);
    model
}

fn tearing(model: &solve::SolveModel) -> &solve::BlockTearing {
    model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .scalar_blocks()[0]
        .tearing
        .as_ref()
        .unwrap()
}

#[test]
fn torn_native_source_fault_preserves_error_without_reference_retry_or_input_mutation() {
    check_native_fault(Failure::Source);
}

#[test]
fn torn_native_backend_fault_preserves_error_without_reference_retry_or_input_mutation() {
    check_native_fault(Failure::Backend);
}

fn check_native_fault(failure: Failure) {
    let model = model();
    let backend = Rc::new(Backend::default());
    backend.fail.set(failure);
    let runtime = SolveRuntime::new_fixture_with_backend(&model, backend.clone()).unwrap();
    assert_eq!(backend.native_calls.get(), 0);
    assert_eq!(backend.source.calls.get(), 0);
    let y = [1.0, 2.0];
    let p = [];
    let result =
        runtime
            .scalar_execution()
            .unwrap()
            .torn_tangent_jacobian(0, tearing(&model), &y, &p, 0.0);
    assert_eq!(y, [1.0, 2.0]);
    let correct_fault = match (failure, &result) {
        (
            Failure::Source,
            Err(RuntimeSolveError::SourceFault {
                span: Some(span), ..
            }),
        ) => *span == test_span("torn_identity_jvp.mo"),
        (Failure::Backend, Err(RuntimeSolveError::CompiledExecution { source_spans, .. })) => {
            source_spans.as_ref() == [test_span("torn_identity_jvp.mo")]
        }
        _ => false,
    };
    assert!(
        correct_fault,
        "native={}, source={}, result={result:?}",
        backend.native_calls.get(),
        backend.source.calls.get()
    );
    assert_eq!(backend.native_calls.get(), 1);
    assert_eq!(
        backend.source.calls.get(),
        0,
        "admitted native errors cannot execute retained source"
    );
    backend.fail.set(Failure::None);
    let recovered = runtime
        .scalar_execution()
        .unwrap()
        .torn_tangent_jacobian(0, tearing(&model), &y, &p, 0.0)
        .unwrap()
        .unwrap()
        .unwrap();
    assert_eq!(recovered.residual, [1.0]);
    assert_eq!(recovered.recovered, [0.0]);
    assert_eq!(
        backend.source.calls.get(),
        0,
        "recovery must still use native execution"
    );
}

#[test]
fn torn_explicit_reference_control_evaluates_the_same_source() {
    let model = model();
    let runtime = SolveRuntime::new_fixture(&model).unwrap();
    let jacobian = runtime
        .scalar_execution()
        .unwrap()
        .torn_tangent_jacobian(0, tearing(&model), &[1.0, 2.0], &[], 0.0)
        .unwrap()
        .unwrap()
        .unwrap();
    assert_eq!(jacobian.residual, [1.0]);
    assert_eq!(jacobian.recovered, [0.0]);
    // Establish that the source-execution witness used by the failure tests
    // really observes this exact original call relation when it is executed.
    let probe = SourceProbe::default();
    let reference = DirectionalProgram::prepare(
        "explicit reference witness",
        model
            .artifacts
            .continuous
            .implicit_jacobian_v_scalar
            .clone(),
        None,
        None,
    )
    .unwrap();
    let mut out = [79.0; 2];
    reference
        .eval(
            solve_eval::JacobianEvalInputs {
                y: &[1.0, 2.0],
                p: &[],
                t: 0.0,
                seed: &[3.0, 5.0],
            },
            RowEvalContext {
                pure_calls: Some(&model.pure_calls),
                pure_call_execution: Some(&probe),
                ..Default::default()
            },
            &mut out,
        )
        .unwrap();
    assert_eq!(out, [3.0, 5.0]);
    assert_eq!(probe.calls.get(), 2);
}

#[test]
fn whole_torn_directional_owner_preserves_faults_and_recovers_without_source_retry() {
    let model = model();
    let backend = Rc::new(Backend::default());
    let runtime = SolveRuntime::new_fixture_with_backend(&model, backend.clone()).unwrap();
    let scalar = runtime.scalar_execution().unwrap();
    let mut output = [17.0, -0.0];
    let inputs = solve_eval::JacobianEvalInputs {
        y: &[1.0, 2.0],
        p: &[],
        t: 0.0,
        seed: &[3.0, 5.0],
    };
    backend.fail.set(Failure::Source);
    assert!(matches!(
        scalar
            .scalar
            .implicit_jacobian_v
            .eval(inputs, runtime.row_eval_context(), &mut output),
        Err(RuntimeSolveError::SourceFault { .. })
    ));
    assert_eq!(output.map(f64::to_bits), [17.0, -0.0].map(f64::to_bits));
    backend.fail.set(Failure::None);
    scalar
        .scalar
        .implicit_jacobian_v
        .eval(inputs, runtime.row_eval_context(), &mut output)
        .unwrap();
    assert_eq!(output, [3.0, 5.0]);
    assert_eq!(backend.native_calls.get(), 2);
    assert_eq!(backend.source.calls.get(), 0);
}

/// A backend that binds the issued tangent rows answers the tear Jacobian
/// with one call of that schedule per point: the per-row values, and never a
/// call of the compiled JVP owner the per-row tangent would sweep.
#[test]
fn the_tear_jacobian_is_read_from_one_issued_schedule_call_per_point() {
    let model = model();
    let backend = Rc::new(IssuingBackend {
        native_calls: Rc::default(),
        schedule_calls: Rc::default(),
        table: model.pure_calls.clone(),
    });
    let runtime = SolveRuntime::new_fixture_with_backend(&model, backend.clone()).unwrap();
    for (point, y) in [[1.0, 2.0], [-3.0, 0.5]].iter().enumerate() {
        let jacobian = runtime
            .scalar_execution()
            .unwrap()
            .torn_tangent_jacobian(0, tearing(&model), y, &[], 0.0)
            .unwrap()
            .unwrap()
            .unwrap();
        assert_eq!(jacobian.residual, [1.0]);
        assert_eq!(jacobian.recovered, [0.0]);
        assert_eq!(backend.schedule_calls.get(), point + 1);
    }
    assert_eq!(backend.native_calls.get(), 0);
}
