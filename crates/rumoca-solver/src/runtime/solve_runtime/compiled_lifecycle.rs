//! Required code for lifecycle paths, prepared before runtime admission.

use super::*;

#[derive(Clone)]
pub(super) struct CompiledPrograms {
    programs: Box<[CompiledProgram]>,
    scratch: RefCell<Vec<f64>>,
}

#[derive(Clone)]
struct CompiledProgram {
    expression: Rc<dyn CompiledSolveExpression>,
    output_count: usize,
    span: rumoca_core::Span,
}

impl CompiledPrograms {
    fn new(
        label: &str,
        source: &solve::ScalarProgramBlock,
        backend: &dyn SolveExecutionBackend,
    ) -> Result<Self, EvalSolveError> {
        let programs = source.programs().iter().enumerate().map(|(index, ops)| {
            let span = source.program_span(index).ok_or_else(|| EvalSolveError::InvalidRow {
                message: format!("{label} program {index} has no source span"),
                span: None,
            })?;
            let block = solve::ScalarProgramBlock::with_program_spans(vec![ops.clone()], vec![span])
                .map_err(|error| EvalSolveError::InvalidRow { message: error.to_string(), span: Some(span) })?;
            let expression = backend.compile_expression(&block).map_err(|message| EvalSolveError::CompiledExecution {
                message: format!("required compiled program `{label}[{index}]` could not be lowered: {message}"),
                span: Some(span),
            })?;
            Ok(CompiledProgram { expression, output_count: block.output_count(), span })
        }).collect::<Result<Box<[_]>, EvalSolveError>>()?;
        Ok(Self {
            programs,
            scratch: RefCell::default(),
        })
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
            .call(point.y, point.p, point.t, tables, &mut values)
            .map_err(|error| admitted_compiled_error("lifecycle program", &[code.span], error))?;
        copy_runtime_values_into(out, &values, "compiled lifecycle output commit")
    }
}

#[derive(Clone)]
pub(super) struct CompiledLifecycle {
    discrete: CompiledPrograms,
    clock_intermediates: CompiledPrograms,
    visible: CompiledPrograms,
    event_conditions: CompiledPrograms,
    roots: CompiledPrograms,
    structured: CompiledPrograms,
    runtime_assignments: CompiledPrograms,
    post_commit_assignments: CompiledPrograms,
    pub(super) guarded: Box<[Rc<dyn CompiledSolveExpression>]>,
}

impl CompiledLifecycle {
    pub(super) fn new(
        model: &solve::SolveModel,
        visible: &solve::ScalarProgramBlock,
        structured: &solve::ScalarProgramBlock,
        guarded: &[PreparedGuardedAssignmentProgram],
        backend: &dyn SolveExecutionBackend,
    ) -> Result<Self, EvalSolveError> {
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
        Ok(Self {
            discrete: compile("discrete_rhs", &discrete.rhs)?,
            clock_intermediates: compile(
                "clock_intermediates",
                &discrete.clock_partition_intermediates,
            )?,
            visible: compile("visible_values", visible)?,
            event_conditions: compile("event_conditions", &model.problem.events.action_conditions)?,
            roots: compile("root_conditions", &model.problem.events.root_conditions)?,
            structured: compile("structured_discrete_rhs", structured)?,
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
            #[cfg(test)]
            PreparedExpressions::Fixture(_) => None,
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
            (&self.root_condition_rows, &code.roots),
            (&self.structured_discrete_rows.rhs, &code.structured),
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
        let mut values = Vec::new();
        let mut staged = zero_runtime_values(out.len(), "lifecycle block output transaction")?;
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
