//! Execution-aware policy for the existing structural register walk.
//!
//! Value mode is the original sparsity derivation. Execution mode also retains
//! obligations even when no output uses the operation. Nested walks share the
//! accumulator; guarded regions conservatively include their control/captures.

use super::*;
use std::cell::RefCell;

mod prefix;
use prefix::ExecutionPrefixBuilder;
pub use prefix::{ExecutionYReadPrefix, ScalarProgramExecutionYDependency};

#[derive(Clone, Copy)]
pub(super) enum DependencyPolicy<'a> {
    Value,
    Execution(&'a RefCell<DependencyState>),
    ExecutionPrefixes(&'a RefCell<ExecutionPrefixBuilder>),
}

impl DependencyPolicy<'_> {
    pub(super) fn is_execution(self) -> bool {
        !matches!(self, Self::Value)
    }

    pub(super) fn include(self, state: DependencyState) {
        match self {
            Self::Value => {}
            Self::Execution(reads) => {
                let mut reads = reads.borrow_mut();
                *reads = reads.clone().union(state);
            }
            Self::ExecutionPrefixes(reads) => reads.borrow_mut().include(state),
        }
    }
}

impl StructuralPattern {
    /// Whole-program execution reads, including results and checked obligations.
    /// Value/Jacobian dependency APIs deliberately keep their value-only policy.
    pub fn derive_execution_y_dependency_union(
        program: &[LinearOp],
        span: Option<Span>,
    ) -> Result<BTreeSet<usize>, StructuralPatternError> {
        execution_union(program, span, DependencySource::SolverY).map(DependencyState::into_set)
    }

    /// Parameter reads of the complete execution, including guard-only inputs.
    pub fn derive_execution_p_dependency_union(
        program: &[LinearOp],
        span: Option<Span>,
    ) -> Result<BTreeSet<usize>, StructuralPatternError> {
        execution_union(program, span, DependencySource::SolverP).map(DependencyState::into_set)
    }

    /// Compact solver reads of the complete execution.
    pub fn derive_execution_y_dependency_ranges(
        program: &[LinearOp],
        span: Option<Span>,
    ) -> Result<Arc<IndexIntervals>, StructuralPatternError> {
        execution_union(program, span, DependencySource::SolverY).map(DependencyState::into_shared)
    }

    /// Compact parameter reads of the complete execution.
    pub fn derive_execution_p_dependency_ranges(
        program: &[LinearOp],
        span: Option<Span>,
    ) -> Result<Arc<IndexIntervals>, StructuralPatternError> {
        execution_union(program, span, DependencySource::SolverP).map(DependencyState::into_shared)
    }

    /// Guard/effect reads alone, for union with an assignment's precise value
    /// footprint. Other outputs of the same program do not become value reads.
    pub fn derive_execution_obligation_y_dependency_ranges(
        program: &[LinearOp],
        span: Option<Span>,
    ) -> Result<Arc<IndexIntervals>, StructuralPatternError> {
        let obligations = RefCell::new(DependencyState::empty());
        program_dependencies_with_policy(
            program,
            span,
            None,
            None,
            None,
            DependencySource::SolverY,
            DependencyPolicy::Execution(&obligations),
        )?;
        Ok(obligations.into_inner().into_shared())
    }

    /// Each result's value reads plus obligations executed by its program.
    pub fn derive_output_execution_y_dependency_ranges(
        program: &[LinearOp],
        span: Option<Span>,
    ) -> Result<Vec<Arc<IndexIntervals>>, StructuralPatternError> {
        output_execution_ranges(program, span, DependencySource::SolverY)
    }

    /// Each result's parameter reads plus the program's execution obligations.
    pub fn derive_output_execution_p_dependency_ranges(
        program: &[LinearOp],
        span: Option<Span>,
    ) -> Result<Vec<Arc<IndexIntervals>>, StructuralPatternError> {
        output_execution_ranges(program, span, DependencySource::SolverP)
    }

    /// Whether complete execution reads time, even in an unused guarded call.
    pub fn execution_reads_time(
        program: &[LinearOp],
        span: Option<Span>,
    ) -> Result<bool, StructuralPatternError> {
        execution_union(program, span, DependencySource::Time).map(|reads| !reads.is_empty())
    }

    /// Whether complete execution reads an AD seed.
    pub fn execution_reads_seed(
        program: &[LinearOp],
        span: Option<Span>,
    ) -> Result<bool, StructuralPatternError> {
        execution_union(program, span, DependencySource::Seed).map(|reads| !reads.is_empty())
    }

    /// Whether complete execution evaluates opaque table/random state.
    pub fn execution_has_effect(
        program: &[LinearOp],
        span: Option<Span>,
    ) -> Result<bool, StructuralPatternError> {
        execution_union(program, span, DependencySource::Effect).map(|reads| !reads.is_empty())
    }
}

fn output_execution_ranges(
    program: &[LinearOp],
    span: Option<Span>,
    source: DependencySource,
) -> Result<Vec<Arc<IndexIntervals>>, StructuralPatternError> {
    let obligations = RefCell::new(DependencyState::empty());
    let outputs = program_dependencies_with_policy(
        program,
        span,
        None,
        None,
        None,
        source,
        DependencyPolicy::Execution(&obligations),
    )?;
    let obligations = obligations.into_inner();
    Ok(outputs
        .into_iter()
        .map(|output| output.union(obligations.clone()).into_shared())
        .collect())
}

fn execution_union(
    program: &[LinearOp],
    span: Option<Span>,
    source: DependencySource,
) -> Result<DependencyState, StructuralPatternError> {
    let obligations = RefCell::new(DependencyState::empty());
    let outputs = program_dependencies_with_policy(
        program,
        span,
        None,
        None,
        None,
        source,
        DependencyPolicy::Execution(&obligations),
    )?;
    Ok(outputs
        .into_iter()
        .fold(obligations.into_inner(), DependencyState::union))
}

impl DependencyWalk<'_> {
    pub(super) fn observe_execution(&self, op: &LinearOp) -> Result<(), StructuralPatternError> {
        if matches!(self.policy, DependencyPolicy::Value) {
            return Ok(());
        }
        match op {
            LinearOp::PureCall {
                input_starts, site, ..
            } => self.include_call_reads(input_starts, site.inputs(), site.execution_reads()),
            LinearOp::PureCallDirectional {
                input_starts, site, ..
            } => self.include_call_reads(input_starts, site.inputs(), site.execution_reads()),
            LinearOp::PureCallObservation {
                input_starts, site, ..
            } => {
                let inputs = site.value_site().inputs();
                self.include_call_reads(
                    input_starts,
                    inputs,
                    &(0..inputs.len()).collect::<Vec<_>>(),
                )
            }
            LinearOp::FunctionConditional {
                capture_start,
                program,
                ..
            } if conditional_obligation(program) => {
                self.policy
                    .include(self.range(*capture_start, program.capture_count)?);
                Ok(())
            }
            LinearOp::FunctionFold {
                initial_start,
                capture_start,
                program,
                ..
            }
            | LinearOp::GuardedFunctionFold {
                initial_start,
                capture_start,
                program,
                ..
            } if fold_obligation(program) => {
                self.policy
                    .include(self.range(*initial_start, program.carried_count)?);
                self.policy
                    .include(self.range(*capture_start, program.capture_count)?);
                if let LinearOp::GuardedFunctionFold { activation, .. } = op {
                    self.policy.include(self.get(*activation)?);
                }
                Ok(())
            }
            // Compact nested-fold sources can name parent-carried cells; the
            // normal fold substitution carries their execution obligations.
            LinearOp::StoreOutputFunctionFold {
                initial,
                capture_start,
                program,
                condition,
                ..
            } if fold_obligation(program) => {
                let parent = self.fold_carried.ok_or_else(|| {
                    dependency_error("execution fold has no carried context", self.span)
                })?;
                let mut carried = Vec::new();
                for source in initial {
                    self.push_nested_fold_initial(&mut carried, parent, source)?;
                }
                for source in carried {
                    self.policy.include(source);
                }
                self.policy
                    .include(self.range(*capture_start, program.capture_count)?);
                if let Some(condition) = condition {
                    self.policy.include(self.get(*condition)?);
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn include_call_reads(
        &self,
        starts: &[Reg],
        inputs: &[crate::SolveValueType],
        reads: &[usize],
    ) -> Result<(), StructuralPatternError> {
        if starts.len() != inputs.len() {
            return Err(dependency_error("execution call input mismatch", self.span));
        }
        for &input in reads {
            let (Some(&start), Some(value_type)) = (starts.get(input), inputs.get(input)) else {
                return Err(dependency_error(
                    "execution call read outside interface",
                    self.span,
                ));
            };
            self.policy
                .include(self.range(start, value_type.scalar_count() as usize)?);
        }
        Ok(())
    }

    pub(super) fn observe_execution_result(
        &self,
        op: &LinearOp,
    ) -> Result<(), StructuralPatternError> {
        if matches!(self.policy, DependencyPolicy::Value) || !direct_obligation(op) {
            return Ok(());
        }
        // The canonical value walk already substituted these operations' exact
        // operands, including fold-capture indices and opaque effect seeds.
        if let Some(start) = op.dst_register() {
            self.policy
                .include(self.range(start, op.dst_register_count())?);
        }
        Ok(())
    }
}

pub(crate) fn conditional_obligation(program: &crate::FunctionConditionalProgram) -> bool {
    program
        .arms
        .iter()
        .any(|arm| program_obligation(&arm.condition) || program_obligation(&arm.result))
        || program_obligation(&program.fallback)
}

pub(crate) fn fold_obligation(program: &crate::FunctionFoldProgram) -> bool {
    program_obligation(&program.update)
        || program
            .continuation
            .as_ref()
            .is_some_and(|region| program_obligation(&region.ops))
}

fn program_obligation(program: &[LinearOp]) -> bool {
    program.iter().any(|op| match op {
        LinearOp::PureCall { site, .. } => site.has_execution_obligations(),
        LinearOp::PureCallDirectional { site, .. } => site.has_execution_obligations(),
        LinearOp::PureCallObservation { .. } => true,
        LinearOp::FunctionConditional { program, .. } => conditional_obligation(program),
        LinearOp::FunctionFold { program, .. }
        | LinearOp::GuardedFunctionFold { program, .. }
        | LinearOp::StoreOutputFunctionFold { program, .. } => fold_obligation(program),
        _ => direct_obligation(op),
    })
}

fn direct_obligation(op: &LinearOp) -> bool {
    match op {
        LinearOp::PureCall { .. }
        | LinearOp::PureCallDirectional { .. }
        | LinearOp::PureCallObservation { .. }
        | LinearOp::FunctionConditional { .. }
        | LinearOp::FunctionFold { .. }
        | LinearOp::GuardedFunctionFold { .. }
        | LinearOp::StoreOutputFunctionFold { .. }
        | LinearOp::StoreOutput { .. }
        | LinearOp::StoreOutputRange { .. }
        | LinearOp::StoreOutputFoldTensorUpdate { .. }
        | LinearOp::LoadFoldCarried { .. }
        | LinearOp::LoadFoldIndex { .. }
        | LinearOp::LoadFoldCapture { .. }
        | LinearOp::LoadFunctionConditionalCapture { .. }
        | LinearOp::LoadFunctionConditionalCaptureRange { .. } => false,
        _ => !op.fault_class().is_prunable(),
    }
}
