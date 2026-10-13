//! Retained model hooks for a runtime compiled independently of the model.

use std::rc::Rc;

use rumoca_ir_solve as solve;

use super::{
    CompiledSolveAssignmentSchedule, CompiledSolveEventTransaction, CompiledSolveExpression,
    CompiledSolveJacobianExpression, SolveExecutionBackend,
};

struct Hook<T: ?Sized> {
    source: solve::ScalarProgramBlock,
    callable: Rc<T>,
}

struct Hooks<T: ?Sized>(Vec<Hook<T>>);

impl<T: ?Sized> Default for Hooks<T> {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl<T: ?Sized> Hooks<T> {
    fn insert(
        &mut self,
        source: &solve::ScalarProgramBlock,
        callable: Rc<T>,
    ) -> Result<(), String> {
        if self.get(source).is_some() {
            return Err("a precompiled hook already retains this source owner".into());
        }
        self.0.push(Hook {
            source: source.clone(),
            callable,
        });
        Ok(())
    }

    fn get(&self, source: &solve::ScalarProgramBlock) -> Option<Rc<T>> {
        self.0
            .iter()
            .find(|hook| hook.source.shares_program_owner(source))
            .map(|hook| hook.callable.clone())
    }
}

/// Prepare retained model hooks before instantiating the common ME runtime.
///
/// This is an execution adapter, not a proof that arbitrary supplied machine
/// code implements its source. The emitting backend remains responsible for
/// the existing compiled-call contracts, layout and pure-call context. Hook
/// association retains exact immutable source owners, including provenance and
/// output placement; an independent reconstruction never selects a hook.
pub struct PrecompiledSolveBackendBuilder {
    layout: solve::VarLayout,
    calls: solve::SolvePureCallTable,
    expressions: Hooks<dyn CompiledSolveExpression>,
    jacobians: Hooks<dyn CompiledSolveJacobianExpression>,
    pure_call_execution: Option<Rc<dyn rumoca_eval_solve::PureCallExecution>>,
}

impl PrecompiledSolveBackendBuilder {
    /// Retain the exact model context used to emit these hooks. External table
    /// hooks are not admitted by this initial adapter.
    pub fn new(model: &solve::SolveModel) -> Result<Self, String> {
        model
            .problem
            .layout
            .validate_shape_contract()
            .map_err(|error| error.to_string())?;
        if !model.external_tables.is_empty() {
            return Err("precompiled external-table hooks are not admitted yet".into());
        }
        Ok(Self {
            layout: model.problem.layout.clone(),
            calls: model.pure_calls.clone(),
            expressions: Hooks::default(),
            jacobians: Hooks::default(),
            pure_call_execution: None,
        })
    }

    pub fn expression(
        &mut self,
        source: &solve::ScalarProgramBlock,
        callable: Rc<dyn CompiledSolveExpression>,
    ) -> Result<(), String> {
        self.expressions.insert(source, callable)
    }

    pub fn jacobian(
        &mut self,
        source: &solve::ScalarProgramBlock,
        callable: Rc<dyn CompiledSolveJacobianExpression>,
    ) -> Result<(), String> {
        self.jacobians.insert(source, callable)
    }

    /// Bind one executor to the exact complete canonical call table retained
    /// at construction. Binding does not execute any model call.
    pub fn pure_call_execution(
        &mut self,
        calls: &solve::SolvePureCallTable,
        callable: Rc<dyn rumoca_eval_solve::PureCallExecution>,
    ) -> Result<(), String> {
        if !self.calls.shares_table_owner(calls) {
            return Err("precompiled pure-call executor belongs to a different table owner".into());
        }
        if self.pure_call_execution.is_some() {
            return Err("a precompiled pure-call executor is already bound".into());
        }
        self.pure_call_execution = Some(callable);
        Ok(())
    }

    /// Freeze the hook association. Runtime compile requests only look up an
    /// issued owner; they never generate code or mutate this table.
    #[must_use]
    pub fn finish(self) -> PrecompiledSolveBackend {
        PrecompiledSolveBackend {
            layout: self.layout,
            calls: self.calls,
            expressions: self.expressions,
            jacobians: self.jacobians,
            pure_call_execution: self.pure_call_execution,
        }
    }
}

/// Immutable execution hooks for the existing Rust ME component.
///
/// This adapter admits scalar expression/JVP owners and an optional canonical
/// pure-call executor. Other
/// requests explicitly decline. The existing optional-backend runtime may
/// interpret a decline; this adapter therefore does not itself admit a direct
/// FMI-LS deployment or establish complete compiled execution.
pub struct PrecompiledSolveBackend {
    layout: solve::VarLayout,
    calls: solve::SolvePureCallTable,
    expressions: Hooks<dyn CompiledSolveExpression>,
    jacobians: Hooks<dyn CompiledSolveJacobianExpression>,
    pure_call_execution: Option<Rc<dyn rumoca_eval_solve::PureCallExecution>>,
}

impl SolveExecutionBackend for PrecompiledSolveBackend {
    fn pure_call_execution(&self) -> Option<&dyn rumoca_eval_solve::PureCallExecution> {
        self.pure_call_execution.as_deref()
    }

    fn validate_model_context(&self, model: &solve::SolveModel) -> Result<(), String> {
        if !self.layout.same_execution_layout(&model.problem.layout) {
            return Err("precompiled hook layout differs from the model execution layout".into());
        }
        if !self.calls.shares_table_owner(&model.pure_calls) {
            return Err("precompiled hooks belong to a different canonical call table".into());
        }
        if !model.external_tables.is_empty() {
            return Err("precompiled external-table hooks are not admitted yet".into());
        }
        Ok(())
    }

    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        self.expressions
            .get(source)
            .ok_or_else(|| "no precompiled expression hook for this source owner".into())
    }

    fn compile_jacobian_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        self.jacobians
            .get(source)
            .ok_or_else(|| "no precompiled Jacobian hook for this source owner".into())
    }

    fn compile_assignment_schedule(
        &self,
        _: &solve::ComputeBlock,
        _: &solve::ContinuousRefreshOwners,
        _: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        Err("precompiled assignment hooks are not admitted yet".into())
    }

    fn compile_event_transaction(
        &self,
        _: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        Err("precompiled event hooks are not admitted yet".into())
    }
}
