//! Optional source-bound isolator and private target-tuple execution.

use super::*;

type CompiledAssignment = Option<Rc<dyn CompiledSolveExpression>>;
type CompiledTargetBinding = Option<(Rc<dyn CompiledSolveTargetValues>, usize)>;
type CompiledTargetFamily = Option<Rc<dyn CompiledSolveTargetValues>>;

#[derive(Clone, Default)]
pub(super) struct NativeProjectionAssignments {
    entries: RefCell<FxHashMap<(usize, usize), CompiledAssignment>>,
    target_bindings: RefCell<FxHashMap<(usize, usize, usize), CompiledTargetBinding>>,
    target_families: RefCell<FxHashMap<(usize, usize), CompiledTargetFamily>>,
}

impl SolveRuntime {
    pub(super) fn compiled_projection_target_values(
        &self,
        program: usize,
        output: usize,
        target: usize,
    ) -> Result<CompiledTargetBinding, RuntimeSolveError> {
        // Runtime construction may evaluate rows before installing its backend.
        if self.execution_backend.is_none() {
            return Ok(None);
        }
        let key = (program, output, target);
        if let Some(cached) = self
            .native_projection_assignments
            .target_bindings
            .borrow()
            .get(&key)
        {
            return Ok(cached.clone());
        }
        let compiled = self.build_projection_target_values(key)?;
        self.native_projection_assignments
            .target_bindings
            .borrow_mut()
            .insert(key, compiled.clone());
        Ok(compiled)
    }

    fn build_projection_target_values(
        &self,
        (program, output, target): (usize, usize, usize),
    ) -> Result<CompiledTargetBinding, RuntimeSolveError> {
        let Some(backend) = self.execution_backend.as_ref() else {
            return Ok(None);
        };
        let Some(plan) = self
            .implicit_scalar_rhs
            .portable_target_value_plan(program, output, target)?
        else {
            return Ok(None);
        };
        let family = (program, plan.canonical_prefix_len());
        let cached = self
            .native_projection_assignments
            .target_families
            .borrow()
            .get(&family)
            .cloned();
        let compiled = if let Some(cached) = cached {
            cached
        } else {
            let compiled = optional_compiled(
                "prepared_target_values",
                backend.compile_target_values(&plan, self.row_eval_context()),
            );
            self.native_projection_assignments
                .target_families
                .borrow_mut()
                .insert(family, compiled.clone());
            compiled
        };
        Ok(compiled.map(|compiled| (compiled, plan.private_result_offset())))
    }

    pub(super) fn compiled_projection_assignment(
        &self,
        program_index: usize,
        target_y_index: usize,
    ) -> Result<Option<Rc<dyn CompiledSolveExpression>>, RuntimeSolveError> {
        let Some(backend) = self.execution_backend.as_ref() else {
            return Ok(None);
        };
        // A tensor program keeps its shared aggregate owner. Do not manufacture
        // one compiled copy of its source prefix per scalar output projection.
        if self.implicit_scalar_rhs.row_output_count(program_index) != Some(1) {
            return Ok(None);
        }
        let key = (program_index, target_y_index);
        if let Some(cached) = self
            .native_projection_assignments
            .entries
            .borrow()
            .get(&key)
        {
            return Ok(cached.clone());
        }
        let compiled = self.build_projection_assignment(backend.as_ref(), key)?;
        self.native_projection_assignments
            .entries
            .borrow_mut()
            .insert(key, compiled.clone());
        Ok(compiled)
    }

    fn build_projection_assignment(
        &self,
        backend: &dyn SolveExecutionBackend,
        (program_index, target_y_index): (usize, usize),
    ) -> Result<Option<Rc<dyn CompiledSolveExpression>>, RuntimeSolveError> {
        let source = &self.implicit_scalar_rhs;
        let Some(program) =
            source.exact_target_assignment_output_program(program_index, 0, target_y_index)
        else {
            return Ok(None);
        };
        let span = source.block().program_span(program_index).ok_or_else(|| {
            RuntimeSolveError::solve_ir("projection assignment source span is missing")
        })?;
        let block = solve::ScalarProgramBlock::with_program_spans(vec![program], vec![span])
            .map_err(|error| {
                RuntimeSolveError::solve_ir_with_span(error.to_string(), Some(span))
            })?;
        Ok(optional_compiled(
            "singleton_projection_assignment",
            backend.compile_expression(&block),
        ))
    }
}
