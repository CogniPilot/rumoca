use super::*;
use selected_programs::CompleteJacobian;

/// Required source expressions of a compiled scalar runtime. A missing kernel
/// has no representation in this inventory.
#[derive(Clone)]
pub(super) struct CompiledExpressions {
    pub(super) implicit_targets: compiled_targets::TargetQueries,
    pub(super) initial_targets: compiled_targets::TargetQueries,
    pub(super) initial_update_jacobian: InitialUpdateJacobian,
    pub(super) event_transactions: Box<[Rc<dyn CompiledSolveEventTransaction>]>,
    pub(super) lifecycle: CompiledLifecycle,
    pub(super) messages: compiled_messages::CompiledMessagePrograms,
    pub(super) root_conditions: Rc<dyn CompiledSolveExpression>,
}

#[derive(Clone)]
pub(super) enum InitialUpdateJacobian {
    Unavailable,
    Compiled(Rc<CompleteJacobian>),
}

impl InitialUpdateJacobian {
    pub(super) fn new(
        source: Option<&solve::ScalarProgramBlock>,
        backend: &dyn SolveExecutionBackend,
    ) -> Result<Self, EvalSolveError> {
        let Some(source) = source else {
            return Ok(Self::Unavailable);
        };
        let whole = required_compiled(
            "initial_update_jacobian",
            source.program_spans(),
            backend.compile_jacobian_expression(source),
        )?;
        selected_programs::jacobian_from_whole(backend, "initial_update_jacobian", source, whole)
            .map(Self::Compiled)
    }
}

#[derive(Clone)]
pub(super) enum PreparedExpressions {
    Reference,
    Compiled(Box<CompiledExpressions>),
}

pub(super) fn require_prepared<T>(label: &str, prepared: Option<T>) -> Result<T, EvalSolveError> {
    prepared.ok_or_else(|| EvalSolveError::CompiledExecution {
        message: format!("required compiled entry `{label}` was not prepared"),
        span: None,
    })
}

impl ScalarExecutionInventory {
    pub(super) fn compiled_targets(
        &self,
        initial: bool,
    ) -> Option<&compiled_targets::TargetQueries> {
        match &self.expressions {
            PreparedExpressions::Compiled(code) => Some(if initial {
                &code.initial_targets
            } else {
                &code.implicit_targets
            }),
            PreparedExpressions::Reference => None,
        }
    }
    pub(super) fn compiled_initial_update_jacobian(&self) -> Option<&CompleteJacobian> {
        match &self.expressions {
            PreparedExpressions::Compiled(code) => match &code.initial_update_jacobian {
                InitialUpdateJacobian::Unavailable => None,
                InitialUpdateJacobian::Compiled(code) => Some(code.as_ref()),
            },
            PreparedExpressions::Reference => None,
        }
    }
    pub(super) fn compiled_event_transaction(
        &self,
        index: usize,
    ) -> Result<Option<&Rc<dyn CompiledSolveEventTransaction>>, RuntimeSolveError> {
        match &self.expressions {
            PreparedExpressions::Reference => Ok(None),
            PreparedExpressions::Compiled(code) => {
                code.event_transactions.get(index).map(Some).ok_or_else(|| {
                    RuntimeSolveError::solve_ir(
                        "required event transaction is outside its prepared source",
                    )
                })
            }
        }
    }

    pub(super) fn compiled_root_conditions(&self) -> Option<&dyn CompiledSolveExpression> {
        match &self.expressions {
            PreparedExpressions::Reference => None,
            PreparedExpressions::Compiled(code) => Some(code.root_conditions.as_ref()),
        }
    }
}
