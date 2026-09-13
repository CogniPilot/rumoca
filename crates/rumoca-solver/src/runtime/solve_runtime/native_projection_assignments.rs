//! Optional native execution of source-bound, single-output exact isolators.

use super::*;

type CompiledAssignment = Option<Rc<dyn CompiledSolveExpression>>;

#[derive(Clone, Default)]
pub(super) struct NativeProjectionAssignments {
    entries: RefCell<FxHashMap<(usize, usize), CompiledAssignment>>,
}

impl SolveRuntime {
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
