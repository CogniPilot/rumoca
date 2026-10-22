//! Required code for lifecycle paths, prepared before runtime admission.

use super::*;

#[cfg(test)]
mod tests;

#[derive(Clone)]
pub(super) struct CompiledPrograms {
    programs: Box<[CompiledProgram]>,
    /// The whole block as one compiled entry, which evaluates every program
    /// into its output placement in one call.
    whole: Option<Rc<dyn CompiledSolveExpression>>,
    scratch: RefCell<Vec<f64>>,
}

#[derive(Clone)]
struct CompiledProgram {
    expression: ProgramExecution,
    output_count: usize,
    span: rumoca_core::Span,
}

#[derive(Clone)]
enum ProgramExecution {
    Numeric(Rc<dyn CompiledSolveExpression>),
    Observed(Rc<dyn CompiledSolveEventObservation>),
}

impl ProgramExecution {
    fn call(
        &self,
        point: RowEvalPoint<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        match self {
            Self::Numeric(code) => code.call(point.y, point.p, point.t, tables, out),
            Self::Observed(code) => code.call(point.y, point.p, point.t, tables, out),
        }
    }
    fn observed_message(&self, action: &solve::SolveEventAction) -> Option<String> {
        match self {
            Self::Numeric(_) => None,
            Self::Observed(code) => code.observed_message(action),
        }
    }
}

impl CompiledPrograms {
    fn new(
        label: &str,
        source: &solve::ScalarProgramBlock,
        backend: &dyn SolveExecutionBackend,
    ) -> Result<Self, EvalSolveError> {
        let whole = (source.row_count() > 1)
            .then(|| optional_compiled(label, backend.compile_selectable_expression(source)))
            .flatten();
        let programs = source
            .programs()
            .iter()
            .enumerate()
            .map(|(index, ops)| {
                Self::prepare_program(label, source, index, ops, whole.as_ref(), backend)
            })
            .collect::<Result<Box<[_]>, EvalSolveError>>()?;
        Ok(Self {
            programs,
            whole,
            scratch: RefCell::default(),
        })
    }

    fn prepare_program(
        label: &str,
        source: &solve::ScalarProgramBlock,
        index: usize,
        ops: &[solve::LinearOp],
        whole: Option<&Rc<dyn CompiledSolveExpression>>,
        backend: &dyn SolveExecutionBackend,
    ) -> Result<CompiledProgram, EvalSolveError> {
        let span = source
            .program_span(index)
            .ok_or_else(|| EvalSolveError::InvalidRow {
                message: format!("{label} program {index} has no source span"),
                span: None,
            })?;
        let bound = whole.map(|code| code.bind_program(source, index)).transpose()
            .map_err(|message| EvalSolveError::CompiledExecution {
                message: format!("required compiled program `{label}[{index}]` could not bind its source: {message}"),
                span: Some(span),
            })?.flatten();
        let expression = if let Some(bound) = bound {
            bound
        } else {
            let block =
                solve::ScalarProgramBlock::with_program_spans(vec![ops.to_vec()], vec![span])
                    .map_err(|error| EvalSolveError::InvalidRow {
                        message: error.to_string(),
                        span: Some(span),
                    })?;
            backend.compile_expression(&block).map_err(|message| EvalSolveError::CompiledExecution {
                message: format!("required compiled program `{label}[{index}]` could not be lowered: {message}"),
                span: Some(span),
            })?
        };
        Ok(CompiledProgram {
            expression: ProgramExecution::Numeric(expression),
            output_count: solve::ScalarProgramBlock::program_output_count(ops),
            span,
        })
    }

    fn event(
        source: &solve::SolveExecutionSource,
        which: solve::RetainedAssertionSource,
        backend: &dyn SolveExecutionBackend,
    ) -> Result<Self, EvalSolveError> {
        let block = match which {
            solve::RetainedAssertionSource::RootConditions => {
                &source.model().problem.events.root_conditions
            }
            solve::RetainedAssertionSource::ActionConditions => {
                &source.model().problem.events.action_conditions
            }
        };
        let mut programs = Vec::new();
        for index in 0..block.row_count() {
            let program =
                source
                    .event_program(which, index)
                    .ok_or_else(|| EvalSolveError::InvalidRow {
                        message: "event program has no checked source/action binding".into(),
                        span: block.program_span(index),
                    })?;
            let span = program.span();
            let compiled_block = solve::ScalarProgramBlock::with_program_spans(
                vec![program.program().to_vec()],
                vec![span],
            )
            .map_err(|error| EvalSolveError::InvalidRow {
                message: error.to_string(),
                span: Some(span),
            })?;
            let output_count = compiled_block.output_count();
            let expression = if program.has_observations() {
                backend
                    .compile_event_observation(program)
                    .map(ProgramExecution::Observed)
            } else {
                backend
                    .compile_expression(&compiled_block)
                    .map(ProgramExecution::Numeric)
            }
            .map_err(|message| EvalSolveError::CompiledExecution {
                message: format!(
                    "required compiled event program {index} could not be lowered: {message}"
                ),
                span: Some(span),
            })?;
            programs.push(CompiledProgram {
                expression,
                output_count,
                span,
            });
        }
        Ok(Self {
            programs: programs.into_boxed_slice(),
            whole: None,
            scratch: RefCell::default(),
        })
    }

    fn observed_message(&self, action: &solve::SolveEventAction) -> Option<String> {
        self.programs
            .iter()
            .find_map(|program| program.expression.observed_message(action))
    }

    pub(super) fn call(
        &self,
        program: usize,
        point: RowEvalPoint<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        let code = self.programs.get(program).ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "required lifecycle program {program} is outside its prepared source"
            ))
        })?;
        let mut values = self.scratch.borrow_mut();
        resize_runtime_values(
            &mut values,
            code.output_count,
            0.0,
            "compiled lifecycle outputs",
        )?;
        code.expression
            .call(point, tables, &mut values)
            .map_err(|error| admitted_compiled_error("lifecycle program", &[code.span], error))?;
        copy_runtime_values_into(out, &values, "compiled lifecycle output commit")
    }

    /// Every program into its output placement in `out` through the whole
    /// entry, when the block has one; a failure names every program's span.
    pub(super) fn call_whole(
        &self,
        point: RowEvalPoint<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Option<Result<(), RuntimeSolveError>> {
        let whole = self.whole.as_ref()?;
        Some(
            whole
                .call(point.y, point.p, point.t, tables, out)
                .map_err(|error| {
                    let spans = self
                        .programs
                        .iter()
                        .map(|program| program.span)
                        .collect::<Vec<_>>();
                    admitted_compiled_error("lifecycle program", &spans, error)
                }),
        )
    }
}

#[derive(Clone)]
pub(super) struct CompiledLifecycle {
    discrete: CompiledPrograms,
    clock_intermediates: CompiledPrograms,
    visible: CompiledPrograms,
    event_conditions: CompiledPrograms,
    roots: Rc<CompiledPrograms>,
    root_expression: Rc<dyn CompiledSolveExpression>,
    structured: CompiledPrograms,
    initialization_updates: CompiledPrograms,
    dynamic_deadlines: CompiledPrograms,
    runtime_assignments: CompiledPrograms,
    post_commit_assignments: CompiledPrograms,
    pub(super) guarded: Box<[Rc<dyn CompiledSolveExpression>]>,
}

impl CompiledLifecycle {
    pub(super) fn root_expression(&self) -> Rc<dyn CompiledSolveExpression> {
        Rc::clone(&self.root_expression)
    }
    pub(super) fn new(
        source: &solve::SolveExecutionSource,
        visible: &solve::ScalarProgramBlock,
        structured: &solve::ScalarProgramBlock,
        guarded: &[PreparedGuardedAssignmentProgram],
        backend: &dyn SolveExecutionBackend,
    ) -> Result<Self, EvalSolveError> {
        let model = source.model();
        let discrete = &model.problem.discrete;
        let compile = |label, block| CompiledPrograms::new(label, block, backend);
        let guarded = guarded
            .iter()
            .enumerate()
            .map(|(index, prepared)| {
                let block = solve::ScalarProgramBlock::with_program_spans(
                    vec![prepared.program().to_vec()],
                    vec![prepared.span()],
                )
                .map_err(|error| EvalSolveError::InvalidRow {
                    message: error.to_string(),
                    span: Some(prepared.span()),
                })?;
                backend.compile_expression(&block).map_err(|message| {
                    EvalSolveError::CompiledExecution {
                        message: format!(
                            "required guarded assignment {index} could not be lowered: {message}"
                        ),
                        span: Some(prepared.span()),
                    }
                })
            })
            .collect::<Result<Box<[_]>, _>>()?;
        let roots = Rc::new(CompiledPrograms::event(
            source,
            solve::RetainedAssertionSource::RootConditions,
            backend,
        )?);
        let root_expression = Rc::new(CompiledRootBlock {
            programs: Rc::clone(&roots),
            source: model.problem.events.root_conditions.clone(),
            values: RefCell::default(),
            staged: RefCell::default(),
        });
        Ok(Self {
            discrete: compile("discrete_rhs", &discrete.rhs)?,
            clock_intermediates: compile(
                "clock_intermediates",
                &discrete.clock_partition_intermediates,
            )?,
            visible: compile("visible_values", visible)?,
            event_conditions: CompiledPrograms::event(
                source,
                solve::RetainedAssertionSource::ActionConditions,
                backend,
            )?,
            roots,
            root_expression,
            structured: compile("structured_discrete_rhs", structured)?,
            initialization_updates: compile(
                "initialization_updates",
                model.problem.initialization.update_rhs(),
            )?,
            dynamic_deadlines: compile(
                "dynamic_deadlines",
                &model.problem.events.dynamic_time_event_rhs,
            )?,
            runtime_assignments: compile("runtime_assignments", &discrete.runtime_assignment_rhs)?,
            post_commit_assignments: compile(
                "post_commit_assignments",
                &discrete.post_commit_assignment_rhs,
            )?,
            guarded,
        })
    }
}

impl ScalarExecutionInventory {
    pub(super) fn lifecycle(&self) -> Option<&CompiledLifecycle> {
        match &self.expressions {
            PreparedExpressions::Compiled(code) => Some(&code.lifecycle),
            PreparedExpressions::Reference => None,
        }
    }

    pub(super) fn lifecycle_programs(
        &self,
        block: &PreparedScalarProgramBlock,
    ) -> Result<Option<&CompiledPrograms>, RuntimeSolveError> {
        let Some(code) = self.lifecycle() else {
            return Ok(None);
        };
        let sources = [
            (&self.discrete_rhs, &code.discrete),
            (
                &self.clock_partition_intermediates,
                &code.clock_intermediates,
            ),
            (&self.visible_value_rows, &code.visible),
            (&self.event_action_conditions, &code.event_conditions),
            (&self.root_condition_rows, code.roots.as_ref()),
            (&self.structured_discrete_rows.rhs, &code.structured),
            (
                &self.initialization_update_rhs,
                &code.initialization_updates,
            ),
            (&self.dynamic_time_event_rhs, &code.dynamic_deadlines),
            (&self.runtime_assignment_rhs, &code.runtime_assignments),
            (
                &self.post_commit_assignment_rhs,
                &code.post_commit_assignments,
            ),
        ];
        sources
            .into_iter()
            .find(|(source, _)| std::ptr::eq(*source, block))
            .map(|(_, programs)| Some(programs))
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(
                    "lifecycle evaluation requested a foreign prepared source",
                )
            })
    }
}

impl ScalarExecutionView<'_> {
    pub(super) fn event_action_message(
        &self,
        action: &solve::SolveEventAction,
        point: RowEvalPoint<'_>,
        context: RowEvalContext<'_>,
    ) -> Result<String, RuntimeSolveError> {
        let Some(code) = self.scalar.lifecycle() else {
            return solve_eval::eval_event_action_message(
                action, point.y, point.p, point.t, context,
            )
            .map_err(Into::into);
        };
        if action.assertion_projection.is_some() {
            return code
                .event_conditions
                .observed_message(action)
                .ok_or_else(|| {
                    RuntimeSolveError::solve_ir(
                        "failed compiled action has no exact invocation observation",
                    )
                });
        }
        solve_eval::eval_event_action_message(action, point.y, point.p, point.t, context)
            .map_err(Into::into)
    }

    pub(super) fn eval_lifecycle_program_outputs(
        &self,
        block: &PreparedScalarProgramBlock,
        program: usize,
        point: RowEvalPoint<'_>,
        out: &mut Vec<f64>,
    ) -> Result<bool, RuntimeSolveError> {
        let Some(code) = self.scalar.lifecycle_programs(block)? else {
            return Ok(false);
        };
        code.call(program, point, self.model.external_tables.as_slice(), out)?;
        Ok(true)
    }

    pub(super) fn eval_lifecycle_row_output(
        &self,
        block: &PreparedScalarProgramBlock,
        program: usize,
        output: usize,
        point: RowEvalPoint<'_>,
    ) -> Result<f64, RuntimeSolveError> {
        let mut values = Vec::new();
        if self.eval_lifecycle_program_outputs(block, program, point, &mut values)? {
            return values.get(output).copied().ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "compiled lifecycle output {output} is outside program {program}"
                ))
            });
        }
        block
            .eval_row_output_unchecked_with_context(
                program,
                output,
                point.y,
                point.p,
                point.t,
                self.row_eval_context(),
            )
            .map_err(Into::into)
    }

    pub(super) fn eval_lifecycle_row(
        &self,
        block: &PreparedScalarProgramBlock,
        program: usize,
        point: RowEvalPoint<'_>,
    ) -> Result<f64, RuntimeSolveError> {
        let mut out = Vec::new();
        if self.eval_lifecycle_program_outputs(block, program, point, &mut out)? {
            return match out.as_slice() {
                [value] => Ok(*value),
                _ => Err(RuntimeSolveError::solve_ir(
                    "lifecycle scalar row has multiple outputs",
                )),
            };
        }
        block
            .eval_row_unchecked_with_context(
                program,
                point.y,
                point.p,
                point.t,
                self.row_eval_context(),
            )
            .map_err(Into::into)
    }

    pub(super) fn eval_lifecycle_block(
        &self,
        block: &PreparedScalarProgramBlock,
        point: RowEvalPoint<'_>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let Some(code) = self.scalar.lifecycle_programs(block)? else {
            return block
                .eval_with_context(point.y, point.p, point.t, self.row_eval_context(), out)
                .map_err(Into::into);
        };
        validate_runtime_output_len("lifecycle block", block.block().output_count(), out.len())?;
        let mut staged = zero_runtime_values(out.len(), "lifecycle block output transaction")?;
        if let Some(called) =
            code.call_whole(point, self.model.external_tables.as_slice(), &mut staged)
        {
            called?;
            out.copy_from_slice(&staged);
            return Ok(());
        }
        let mut values = Vec::new();
        let mut ordinal = 0;
        for program in 0..block.block().row_count() {
            code.call(
                program,
                point,
                self.model.external_tables.as_slice(),
                &mut values,
            )?;
            for value in &values {
                staged[block.block().output_indices()[ordinal]] = *value;
                ordinal += 1;
            }
        }
        out.copy_from_slice(&staged);
        Ok(())
    }
}

/// The whole-root entry shares the same issued per-program compiled products as selection.
struct CompiledRootBlock {
    programs: Rc<CompiledPrograms>,
    source: solve::ScalarProgramBlock,
    values: RefCell<Vec<f64>>,
    staged: RefCell<Vec<f64>>,
}

impl CompiledSolveExpression for CompiledRootBlock {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        validate_runtime_output_len("compiled root block", self.source.output_count(), out.len())?;
        let mut values = self.values.borrow_mut();
        let mut staged = self.staged.borrow_mut();
        resize_runtime_values(
            &mut staged,
            out.len(),
            0.0,
            "compiled root block transaction",
        )?;
        let mut ordinal = 0;
        for program in 0..self.source.row_count() {
            self.programs
                .call(program, RowEvalPoint { y, p, t }, tables, &mut values)?;
            for &value in values.iter() {
                staged[self.source.output_indices()[ordinal]] = value;
                ordinal += 1;
            }
        }
        out.copy_from_slice(&staged);
        Ok(())
    }
}
