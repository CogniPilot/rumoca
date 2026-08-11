//! The one construction authority for checked GALEC methods.
//!
//! Every action proves its own obligations before it is committed: cells are
//! visible in the current lexical scope, reads are dominated by a definition,
//! writes reach writable storage, loops have finite non-empty domains, branches
//! own complete arms, calls match an issued interface, and effects name only
//! declared signals and ranged entities.

use std::marker::PhantomData;

use rumoca_core::{Span, StructuredIndexDomain};

use super::super::call::SolvePureCallInterface;
use super::super::effect::{SolveSignalSet, SolveValueRange};
use super::super::program::{
    ProgramSlot, SolveProgramConstructionError, SolveProgramRegion, SolveSlotAccess,
    SolveStorageClass, TypedProgram, TypedProgramBuilder, construct_region,
};
use super::super::types::{SolveArithmeticProfile, SolveScalarType, SolveValueType};
use super::action::{
    SolveAction, SolveActionBlock, SolveBranchCondition, SolveBranchConditionSpec,
    SolveLimitTarget, SolveLocalDeclaration, SolveSignalCheck, SolveSpannedAction,
    SolveValueProgram,
};
use super::escape::{EscapeContext, escape_set, require_named_signals};
use super::{
    SolveActionConstructionError, SolveCallAbiPlan, SolveCell, SolveCellId, SolveMethod,
    SolveMethodContext, SolveMethodId, SolveMethodInterface, SolveMethodKind, SolveScope,
    SolveScopeId, SolveSignalClosure, SolveSignalClosureId, require_provenance,
};

/// A generatively branded handle to one cell of one method under construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodCell<'method> {
    id: SolveCellId,
    marker: PhantomData<&'method mut &'method ()>,
}

impl MethodCell<'_> {
    #[must_use]
    pub const fn id(self) -> SolveCellId {
        self.id
    }
}

/// A generatively branded handle to one caught signal closure.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodClosure<'method> {
    id: SolveSignalClosureId,
    marker: PhantomData<&'method mut &'method ()>,
}

impl MethodClosure<'_> {
    #[must_use]
    pub const fn id(self) -> SolveSignalClosureId {
        self.id
    }
}

/// One limit target expressed over branded cells.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SolveLimitTargetSpec<'method> {
    RangedState,
    Cell(MethodCell<'method>),
}

/// The interface cells a method body may address from its root scope.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SolveMethodCells<'method> {
    parameters: Vec<MethodCell<'method>>,
    results: Vec<MethodCell<'method>>,
    bindings: Vec<MethodCell<'method>>,
}

impl<'method> SolveMethodCells<'method> {
    #[must_use]
    pub fn parameters(&self) -> &[MethodCell<'method>] {
        &self.parameters
    }

    #[must_use]
    pub fn results(&self) -> &[MethodCell<'method>] {
        &self.results
    }

    #[must_use]
    pub fn bindings(&self) -> &[MethodCell<'method>] {
        &self.bindings
    }
}

struct OpenBlock {
    scope: SolveScopeId,
    locals: Vec<SolveLocalDeclaration>,
    actions: Vec<SolveSpannedAction>,
    provenance: Span,
}

/// The checked builder of one method body.
pub struct SolveMethodBuilder<'method> {
    context: SolveMethodContext,
    kind: SolveMethodKind,
    scopes: Vec<SolveScope>,
    cells: Vec<SolveCell>,
    defined: Vec<bool>,
    closures: Vec<SolveSignalClosure>,
    blocks: Vec<OpenBlock>,
    marker: PhantomData<&'method mut &'method ()>,
}

pub(super) fn construct_method(
    id: SolveMethodId,
    kind: SolveMethodKind,
    interface: SolveMethodInterface,
    provenance: Span,
    context: SolveMethodContext,
    build: impl for<'method> FnOnce(
        &mut SolveMethodBuilder<'method>,
        &SolveMethodCells<'method>,
    ) -> Result<(), SolveActionConstructionError>,
) -> Result<SolveMethod, SolveActionConstructionError> {
    let mut builder = SolveMethodBuilder::open(context, kind, provenance)?;
    let cells = builder.declare_interface(&interface, provenance)?;
    build(&mut builder, &cells)?;
    builder.finish(id, interface, provenance)
}

impl<'method> SolveMethodBuilder<'method> {
    fn open(
        context: SolveMethodContext,
        kind: SolveMethodKind,
        provenance: Span,
    ) -> Result<Self, SolveActionConstructionError> {
        require_provenance(provenance)?;
        let root = SolveScopeId(0);
        Ok(Self {
            context,
            kind,
            scopes: vec![SolveScope {
                id: root,
                parent: None,
                provenance,
            }],
            cells: Vec::new(),
            defined: Vec::new(),
            closures: Vec::new(),
            blocks: vec![OpenBlock {
                scope: root,
                locals: Vec::new(),
                actions: Vec::new(),
                provenance,
            }],
            marker: PhantomData,
        })
    }

    fn declare_interface(
        &mut self,
        interface: &SolveMethodInterface,
        provenance: Span,
    ) -> Result<SolveMethodCells<'method>, SolveActionConstructionError> {
        let mut parameters = Vec::with_capacity(interface.parameters().len());
        for value_type in interface.parameters() {
            parameters.push(self.issue_cell(
                value_type.clone(),
                SolveStorageClass::Input,
                SolveSlotAccess::ReadOnly,
                None,
                provenance,
            )?);
        }
        let mut results = Vec::with_capacity(interface.results().len());
        for value_type in interface.results() {
            results.push(self.issue_cell(
                value_type.clone(),
                SolveStorageClass::Output,
                SolveSlotAccess::ReadWrite,
                None,
                provenance,
            )?);
        }
        let mut bindings = Vec::with_capacity(interface.bindings().len());
        for binding in interface.bindings() {
            bindings.push(self.issue_cell(
                binding.value_type().clone(),
                binding.storage(),
                binding.access(),
                binding.range().cloned(),
                provenance,
            )?);
        }
        Ok(SolveMethodCells {
            parameters,
            results,
            bindings,
        })
    }

    /// Seals one method body.
    ///
    /// A nested block that is still open means an action constructor failed and
    /// its error was swallowed by the build closure. That is a typed
    /// construction error, never an assertion: with assertions compiled out the
    /// abandoned arm would otherwise be popped and published as the body.
    fn finish(
        mut self,
        id: SolveMethodId,
        interface: SolveMethodInterface,
        provenance: Span,
    ) -> Result<SolveMethod, SolveActionConstructionError> {
        if self.blocks.len() != 1 {
            return Err(SolveActionConstructionError::UnclosedBlock { provenance });
        }
        let open = self
            .blocks
            .pop()
            .ok_or(SolveActionConstructionError::UnclosedBlock { provenance })?;
        let body = SolveActionBlock::issued(open.scope, open.locals, open.actions, open.provenance);
        let results =
            interface.parameters().len()..interface.parameters().len() + interface.results().len();
        if results.clone().any(|index| !self.defined[index]) {
            return Err(SolveActionConstructionError::UndefinedResult { provenance });
        }
        let callees = self
            .context
            .methods
            .iter()
            .map(|signature| signature.interface.escapes())
            .collect::<Vec<_>>();
        let computed = escape_set(
            &body,
            &EscapeContext {
                closures: &self.closures,
                callees: &callees,
                calls: &self.context.call_signals,
            },
        )?;
        if computed != interface.escapes() {
            return Err(SolveActionConstructionError::EscapeSetMismatch { provenance });
        }
        let abi = SolveCallAbiPlan::derive(&interface);
        Ok(SolveMethod {
            id,
            kind: self.kind,
            interface,
            abi,
            scopes: self.scopes.into_boxed_slice(),
            cells: self.cells.into_boxed_slice(),
            closures: self.closures.into_boxed_slice(),
            body,
            provenance,
        })
    }

    /// Declares one lexical local in the current scope.
    ///
    /// Locals are always method-local scratch: they can never become block
    /// storage, persistent state, a parameter, or a result.
    pub fn declare_local(
        &mut self,
        value_type: SolveValueType,
        range: Option<SolveValueRange>,
        provenance: Span,
    ) -> Result<MethodCell<'method>, SolveActionConstructionError> {
        require_provenance(provenance)?;
        if !self.current_block().actions.is_empty() {
            return Err(SolveActionConstructionError::DeclarationAfterAction { provenance });
        }
        let cell = self.issue_cell(
            value_type.clone(),
            SolveStorageClass::MethodLocal,
            SolveSlotAccess::ReadWrite,
            range.clone(),
            provenance,
        )?;
        self.current_block_mut()
            .locals
            .push(SolveLocalDeclaration::issued(
                cell.id, value_type, range, provenance,
            ));
        Ok(cell)
    }

    /// Evaluates one typed program and commits its results to exact cells.
    pub fn assign(
        &mut self,
        targets: &[MethodCell<'method>],
        reads: &[MethodCell<'method>],
        provenance: Span,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
            &[ProgramSlot<'program>],
            &[ProgramSlot<'program>],
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<(), SolveActionConstructionError> {
        require_provenance(provenance)?;
        let inputs = self.read_types(reads, provenance)?;
        let outputs = self.write_types(targets, provenance)?;
        if outputs.is_empty() {
            return Err(SolveActionConstructionError::InvalidProgramInterface { provenance });
        }
        let region = self.region(inputs, outputs, provenance, build)?;
        self.assign_from_region(targets, reads, region, provenance)
    }

    pub(in crate::typed_program) fn assign_from_region(
        &mut self,
        targets: &[MethodCell<'method>],
        reads: &[MethodCell<'method>],
        region: SolveProgramRegion,
        provenance: Span,
    ) -> Result<(), SolveActionConstructionError> {
        require_provenance(provenance)?;
        let inputs = self.read_types(reads, provenance)?;
        let outputs = self.write_types(targets, provenance)?;
        if outputs.is_empty()
            || region.inputs() != inputs.as_slice()
            || region.outputs() != outputs.as_slice()
            || region.provenance() != provenance
            || region.body().arithmetic() != self.context.arithmetic
        {
            return Err(SolveActionConstructionError::InvalidProgramInterface { provenance });
        }
        require_named_signals(&region, provenance)?;
        let program = SolveValueProgram::issued(cell_ids(reads), region);
        self.push_action(
            SolveAction::Assign {
                program,
                targets: cell_ids(targets).into_boxed_slice(),
            },
            provenance,
        );
        for target in targets {
            self.defined[target.id.index()] = true;
        }
        Ok(())
    }

    /// Builds one typed expression program over exact readable cells.
    pub fn value_program(
        &self,
        reads: &[MethodCell<'method>],
        outputs: Vec<SolveValueType>,
        provenance: Span,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
            &[ProgramSlot<'program>],
            &[ProgramSlot<'program>],
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<SolveValueProgram, SolveActionConstructionError> {
        require_provenance(provenance)?;
        let inputs = self.read_types(reads, provenance)?;
        let region = self.region(inputs, outputs, provenance, build)?;
        require_named_signals(&region, provenance)?;
        Ok(SolveValueProgram::issued(cell_ids(reads), region))
    }

    /// Builds one typed Boolean scalar test usable as a branch condition.
    pub fn boolean_program(
        &self,
        reads: &[MethodCell<'method>],
        provenance: Span,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
            &[ProgramSlot<'program>],
            &[ProgramSlot<'program>],
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<SolveValueProgram, SolveActionConstructionError> {
        self.value_program(reads, vec![boolean_type()], provenance, build)
    }

    pub(in crate::typed_program) fn value_program_from_region(
        &self,
        reads: &[MethodCell<'method>],
        region: SolveProgramRegion,
        provenance: Span,
    ) -> Result<SolveValueProgram, SolveActionConstructionError> {
        require_provenance(provenance)?;
        let inputs = self.read_types(reads, provenance)?;
        if region.inputs() != inputs.as_slice()
            || region.provenance() != provenance
            || region.body().arithmetic() != self.context.arithmetic
        {
            return Err(SolveActionConstructionError::InvalidProgramInterface { provenance });
        }
        require_named_signals(&region, provenance)?;
        Ok(SolveValueProgram::issued(cell_ids(reads), region))
    }

    /// Commits one complete two-armed branch; each arm owns a lexical scope.
    pub fn branch(
        &mut self,
        condition: SolveBranchConditionSpec,
        provenance: Span,
        build_true: impl FnOnce(
            &mut Self,
            Option<MethodClosure<'method>>,
        ) -> Result<(), SolveActionConstructionError>,
        build_false: impl FnOnce(&mut Self) -> Result<(), SolveActionConstructionError>,
    ) -> Result<(), SolveActionConstructionError> {
        require_provenance(provenance)?;
        self.require_condition(&condition, provenance)?;
        let entry = self.defined.clone();
        let (condition, if_true) = self.build_true_arm(condition, provenance, build_true)?;
        let after_true = self.defined.clone();
        self.defined[..entry.len()].copy_from_slice(&entry);
        let if_false = self.build_arm(provenance, build_false)?;
        for ((defined, entry), after_true) in self.defined.iter_mut().zip(&entry).zip(&after_true) {
            *defined = *entry || (*after_true && *defined);
        }
        if if_true.is_empty() && if_false.is_empty() {
            return Err(SolveActionConstructionError::EmptyBranch { provenance });
        }
        self.push_action(
            SolveAction::Branch {
                condition,
                if_true,
                if_false,
            },
            provenance,
        );
        Ok(())
    }

    /// Commits one statically bounded loop over a checked finite domain.
    pub fn iterate(
        &mut self,
        domain: StructuredIndexDomain,
        provenance: Span,
        build: impl FnOnce(
            &mut Self,
            &[MethodCell<'method>],
        ) -> Result<(), SolveActionConstructionError>,
    ) -> Result<(), SolveActionConstructionError> {
        require_provenance(provenance)?;
        require_bounded_domain(&domain, self.context.arithmetic, provenance)?;
        let scope = self.open_scope(provenance)?;
        let binder_type = SolveValueType::scalar(SolveScalarType::integer(self.context.arithmetic));
        let mut binders = Vec::with_capacity(domain.binders.len());
        for _ in &domain.binders {
            let binder = self.issue_cell(
                binder_type.clone(),
                SolveStorageClass::MethodLocal,
                SolveSlotAccess::ReadOnly,
                None,
                provenance,
            )?;
            self.defined[binder.id.index()] = true;
            binders.push(binder);
        }
        build(self, &binders)?;
        let body = self.close_block(scope, provenance)?;
        self.push_action(
            SolveAction::Loop {
                domain,
                binders: cell_ids(&binders).into_boxed_slice(),
                body,
            },
            provenance,
        );
        Ok(())
    }

    /// Commits one checked call of a previously issued method.
    pub fn invoke(
        &mut self,
        method: SolveMethodId,
        arguments: &[MethodCell<'method>],
        results: &[MethodCell<'method>],
        provenance: Span,
    ) -> Result<(), SolveActionConstructionError> {
        require_provenance(provenance)?;
        let signature = self
            .context
            .methods
            .get(method.index() as usize)
            .filter(|signature| signature.id == method)
            .ok_or(SolveActionConstructionError::UnknownMethod { provenance })?
            .clone();
        if self.kind == SolveMethodKind::Stateless && signature.kind == SolveMethodKind::Stateful {
            return Err(
                SolveActionConstructionError::StatefulEffectInStatelessMethod { provenance },
            );
        }
        let argument_types = self.read_types(arguments, provenance)?;
        let result_types = self.write_types(results, provenance)?;
        if argument_types.as_slice() != signature.interface.parameters()
            || result_types.as_slice() != signature.interface.results()
        {
            return Err(SolveActionConstructionError::InvalidCallInterface { provenance });
        }
        self.push_action(
            SolveAction::Invoke {
                method,
                arguments: cell_ids(arguments).into_boxed_slice(),
                results: cell_ids(results).into_boxed_slice(),
            },
            provenance,
        );
        for result in results {
            self.defined[result.id.index()] = true;
        }
        Ok(())
    }

    /// Commits one explicit saturation effect over ranged entities.
    pub fn limit(
        &mut self,
        targets: &[SolveLimitTargetSpec<'method>],
        provenance: Span,
    ) -> Result<(), SolveActionConstructionError> {
        require_provenance(provenance)?;
        if targets.is_empty() {
            return Err(SolveActionConstructionError::EmptyLimit { provenance });
        }
        let mut committed = Vec::with_capacity(targets.len());
        for target in targets {
            committed.push(self.limit_target(*target, provenance)?);
        }
        self.push_action(
            SolveAction::Limit {
                targets: committed.into_boxed_slice(),
            },
            provenance,
        );
        Ok(())
    }

    /// Commits one explicit error-signal set or closure re-raise.
    pub fn raise(
        &mut self,
        signals: SolveSignalSet,
        closures: &[MethodClosure<'method>],
        provenance: Span,
    ) -> Result<(), SolveActionConstructionError> {
        require_provenance(provenance)?;
        if !self.context.signals.contains_all(signals) {
            return Err(SolveActionConstructionError::UndeclaredSignal { provenance });
        }
        if signals.is_empty() && closures.is_empty() {
            return Err(SolveActionConstructionError::EmptySignalEffect { provenance });
        }
        let mut committed = Vec::with_capacity(closures.len());
        for closure in closures {
            committed.push(self.visible_closure(*closure, provenance)?);
        }
        self.push_action(
            SolveAction::Signal {
                signals,
                closures: committed.into_boxed_slice(),
            },
            provenance,
        );
        Ok(())
    }

    /// The pure-call owners a value program of this method may invoke.
    pub(in crate::typed_program) fn call_interfaces(&self) -> &[SolvePureCallInterface] {
        &self.context.calls
    }

    /// Recovers the branded handle of the cell this builder issued `ordinal`th.
    ///
    /// Wire replay addresses cells by wire-local ordinal and never carries an
    /// identity: the handle returned here always carries the identity this
    /// builder issued, so the bytes select an existing cell instead of naming
    /// one. Every action constructor still proves visibility, definition, and
    /// mutability before committing.
    pub(in crate::typed_program) fn cell_handle(
        &self,
        ordinal: u32,
        provenance: Span,
    ) -> Result<MethodCell<'method>, SolveActionConstructionError> {
        let cell = self
            .cells
            .get(ordinal as usize)
            .ok_or(SolveActionConstructionError::UnknownCell { provenance })?;
        Ok(MethodCell {
            id: cell.id,
            marker: PhantomData,
        })
    }

    /// Recovers the branded handle of the `ordinal`th issued signal closure.
    pub(in crate::typed_program) fn closure_handle(
        &self,
        ordinal: u32,
        provenance: Span,
    ) -> Result<MethodClosure<'method>, SolveActionConstructionError> {
        let closure = self
            .closures
            .get(ordinal as usize)
            .ok_or(SolveActionConstructionError::UnknownClosure { provenance })?;
        Ok(MethodClosure {
            id: closure.id,
            marker: PhantomData,
        })
    }

    /// Recovers the identity the table issued to its `ordinal`th method.
    ///
    /// Only a method issued before this one is addressable, so the acyclic
    /// call graph is proved by the resolution itself rather than by trusting a
    /// decoded identity.
    pub(in crate::typed_program) fn callee_identity(
        &self,
        ordinal: u32,
        provenance: Span,
    ) -> Result<SolveMethodId, SolveActionConstructionError> {
        self.context
            .methods
            .get(ordinal as usize)
            .map(|signature| signature.id)
            .ok_or(SolveActionConstructionError::UnknownMethod { provenance })
    }

    fn build_true_arm(
        &mut self,
        condition: SolveBranchConditionSpec,
        provenance: Span,
        build: impl FnOnce(
            &mut Self,
            Option<MethodClosure<'method>>,
        ) -> Result<(), SolveActionConstructionError>,
    ) -> Result<(SolveBranchCondition, SolveActionBlock), SolveActionConstructionError> {
        let scope = self.open_scope(provenance)?;
        let (committed, closure) = match condition {
            SolveBranchConditionSpec::Value(program) => {
                (SolveBranchCondition::Value(program), None)
            }
            SolveBranchConditionSpec::Signal {
                test,
                capture_closure,
                fallback,
            } => {
                let caught = test.map_or(self.context.signals, |test| {
                    test.caught(self.context.signals)
                });
                let closure = if capture_closure {
                    Some(self.issue_closure(scope, caught, provenance)?)
                } else {
                    None
                };
                let check = SolveSignalCheck::issued(
                    test,
                    caught,
                    closure.map(MethodClosure::id),
                    fallback,
                );
                (SolveBranchCondition::Signal(check), closure)
            }
        };
        build(self, closure)?;
        let arm = self.close_block(scope, provenance)?;
        Ok((committed, arm))
    }

    fn build_arm(
        &mut self,
        provenance: Span,
        build: impl FnOnce(&mut Self) -> Result<(), SolveActionConstructionError>,
    ) -> Result<SolveActionBlock, SolveActionConstructionError> {
        let scope = self.open_scope(provenance)?;
        build(self)?;
        self.close_block(scope, provenance)
    }

    fn require_condition(
        &self,
        condition: &SolveBranchConditionSpec,
        provenance: Span,
    ) -> Result<(), SolveActionConstructionError> {
        match condition {
            SolveBranchConditionSpec::Value(program) => {
                self.require_boolean_program(program, provenance)
            }
            SolveBranchConditionSpec::Signal { test, fallback, .. } => {
                if let Some(test) = test
                    && !self.context.signals.contains_all(test.signals())
                {
                    return Err(SolveActionConstructionError::UndeclaredSignal { provenance });
                }
                match fallback {
                    Some(program) => self.require_boolean_program(program, provenance),
                    None => Ok(()),
                }
            }
        }
    }

    fn require_boolean_program(
        &self,
        program: &SolveValueProgram,
        provenance: Span,
    ) -> Result<(), SolveActionConstructionError> {
        let outputs = program.region().outputs();
        if outputs.len() != 1 || outputs[0] != boolean_type() {
            return Err(SolveActionConstructionError::InvalidCondition { provenance });
        }
        if program.region().inputs().len() != program.reads().len() {
            return Err(SolveActionConstructionError::InvalidProgramInterface { provenance });
        }
        for (id, expected) in program.reads().iter().zip(program.region().inputs()) {
            let cell = self.readable_cell_by_id(*id, provenance)?;
            if cell.value_type != *expected {
                return Err(SolveActionConstructionError::InvalidProgramInterface { provenance });
            }
        }
        Ok(())
    }

    fn limit_target(
        &self,
        target: SolveLimitTargetSpec<'method>,
        provenance: Span,
    ) -> Result<SolveLimitTarget, SolveActionConstructionError> {
        match target {
            SolveLimitTargetSpec::RangedState => {
                if self.kind == SolveMethodKind::Stateless {
                    return Err(
                        SolveActionConstructionError::StatefulEffectInStatelessMethod {
                            provenance,
                        },
                    );
                }
                let ranged = self.cells.iter().any(|cell| {
                    cell.is_persistent()
                        && cell.range.is_some()
                        && cell.access == SolveSlotAccess::ReadWrite
                        && self.scope_is_visible(cell.scope)
                });
                if !ranged {
                    return Err(SolveActionConstructionError::UnrangedLimitTarget { provenance });
                }
                Ok(SolveLimitTarget::RangedState)
            }
            SolveLimitTargetSpec::Cell(cell) => {
                let owned = self.writable_cell(cell, provenance)?;
                if owned.range.is_none() {
                    return Err(SolveActionConstructionError::UnrangedLimitTarget { provenance });
                }
                if !self.defined[owned.id.index()] {
                    return Err(SolveActionConstructionError::UndefinedRead { provenance });
                }
                Ok(SolveLimitTarget::Cell(owned.id))
            }
        }
    }

    fn region(
        &self,
        inputs: Vec<SolveValueType>,
        outputs: Vec<SolveValueType>,
        provenance: Span,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
            &[ProgramSlot<'program>],
            &[ProgramSlot<'program>],
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<SolveProgramRegion, SolveActionConstructionError> {
        let body = TypedProgram::construct_with_calls(
            self.context.arithmetic,
            self.context.calls.clone(),
            |builder| {
                let mut input_slots = Vec::with_capacity(inputs.len());
                for value_type in &inputs {
                    input_slots.push(builder.declare_slot(
                        value_type.clone(),
                        SolveStorageClass::Input,
                        SolveSlotAccess::ReadOnly,
                        provenance,
                    )?);
                }
                let mut output_slots = Vec::with_capacity(outputs.len());
                for value_type in &outputs {
                    output_slots.push(builder.declare_slot(
                        value_type.clone(),
                        SolveStorageClass::Output,
                        SolveSlotAccess::ReadWrite,
                        provenance,
                    )?);
                }
                build(builder, &input_slots, &output_slots)
            },
        )?;
        Ok(construct_region(inputs, outputs, body, provenance)?)
    }

    fn read_types(
        &self,
        cells: &[MethodCell<'method>],
        provenance: Span,
    ) -> Result<Vec<SolveValueType>, SolveActionConstructionError> {
        cells
            .iter()
            .map(|cell| {
                let owned = self.readable_cell_by_id(cell.id, provenance)?;
                Ok(owned.value_type.clone())
            })
            .collect()
    }

    fn write_types(
        &self,
        cells: &[MethodCell<'method>],
        provenance: Span,
    ) -> Result<Vec<SolveValueType>, SolveActionConstructionError> {
        for (index, cell) in cells.iter().enumerate() {
            if cells[..index].contains(cell) {
                return Err(SolveActionConstructionError::DuplicateTarget { provenance });
            }
        }
        cells
            .iter()
            .map(|cell| {
                let owned = self.writable_cell(*cell, provenance)?;
                Ok(owned.value_type.clone())
            })
            .collect()
    }

    fn readable_cell_by_id(
        &self,
        id: SolveCellId,
        provenance: Span,
    ) -> Result<&SolveCell, SolveActionConstructionError> {
        let owned = self.visible_cell(id, provenance)?;
        if !self.defined[owned.id.index()] {
            return Err(SolveActionConstructionError::UndefinedRead { provenance });
        }
        Ok(owned)
    }

    fn writable_cell(
        &self,
        cell: MethodCell<'method>,
        provenance: Span,
    ) -> Result<&SolveCell, SolveActionConstructionError> {
        let owned = self.visible_cell(cell.id, provenance)?;
        if owned.access != SolveSlotAccess::ReadWrite {
            return Err(SolveActionConstructionError::ReadOnlyTarget { provenance });
        }
        if self.kind == SolveMethodKind::Stateless && owned.is_persistent() {
            return Err(
                SolveActionConstructionError::StatefulEffectInStatelessMethod { provenance },
            );
        }
        Ok(owned)
    }

    fn visible_cell(
        &self,
        id: SolveCellId,
        provenance: Span,
    ) -> Result<&SolveCell, SolveActionConstructionError> {
        let owned = self
            .cells
            .get(id.index())
            .filter(|cell| cell.id == id)
            .ok_or(SolveActionConstructionError::UnknownCell { provenance })?;
        if !self.scope_is_visible(owned.scope) {
            return Err(SolveActionConstructionError::CellOutOfScope { provenance });
        }
        Ok(owned)
    }

    fn visible_closure(
        &self,
        closure: MethodClosure<'method>,
        provenance: Span,
    ) -> Result<SolveSignalClosureId, SolveActionConstructionError> {
        let owned = self
            .closures
            .get(closure.id.index())
            .filter(|candidate| candidate.id == closure.id)
            .ok_or(SolveActionConstructionError::UnknownClosure { provenance })?;
        if !self.scope_is_visible(owned.scope) {
            return Err(SolveActionConstructionError::ClosureOutOfScope { provenance });
        }
        Ok(owned.id)
    }

    fn scope_is_visible(&self, scope: SolveScopeId) -> bool {
        let mut current = Some(self.current_block().scope);
        while let Some(id) = current {
            if id == scope {
                return true;
            }
            current = self.scopes[id.index()].parent;
        }
        false
    }

    fn issue_cell(
        &mut self,
        value_type: SolveValueType,
        storage: SolveStorageClass,
        access: SolveSlotAccess,
        range: Option<SolveValueRange>,
        provenance: Span,
    ) -> Result<MethodCell<'method>, SolveActionConstructionError> {
        require_provenance(provenance)?;
        if !value_type.belongs_to(self.context.arithmetic) {
            return Err(SolveActionConstructionError::ProfileMismatch { provenance });
        }
        if let Some(range) = &range
            && !range.limits(value_type.element_type())
        {
            return Err(SolveActionConstructionError::InvalidRange { provenance });
        }
        let index = u32::try_from(self.cells.len())
            .map_err(|_| SolveActionConstructionError::IdentityOverflow { provenance })?;
        let id = SolveCellId(index);
        let scope = self.current_block().scope;
        self.defined.push(!matches!(
            storage,
            SolveStorageClass::Output | SolveStorageClass::MethodLocal
        ));
        self.cells.push(SolveCell {
            id,
            scope,
            value_type,
            storage,
            access,
            range,
            provenance,
        });
        Ok(MethodCell {
            id,
            marker: PhantomData,
        })
    }

    fn issue_closure(
        &mut self,
        scope: SolveScopeId,
        caught: SolveSignalSet,
        provenance: Span,
    ) -> Result<MethodClosure<'method>, SolveActionConstructionError> {
        let index = u32::try_from(self.closures.len())
            .map_err(|_| SolveActionConstructionError::IdentityOverflow { provenance })?;
        let id = SolveSignalClosureId(index);
        self.closures.push(SolveSignalClosure {
            id,
            scope,
            caught,
            provenance,
        });
        Ok(MethodClosure {
            id,
            marker: PhantomData,
        })
    }

    fn open_scope(
        &mut self,
        provenance: Span,
    ) -> Result<SolveScopeId, SolveActionConstructionError> {
        require_provenance(provenance)?;
        let index = u32::try_from(self.scopes.len())
            .map_err(|_| SolveActionConstructionError::IdentityOverflow { provenance })?;
        let id = SolveScopeId(index);
        let parent = Some(self.current_block().scope);
        self.scopes.push(SolveScope {
            id,
            parent,
            provenance,
        });
        self.blocks.push(OpenBlock {
            scope: id,
            locals: Vec::new(),
            actions: Vec::new(),
            provenance,
        });
        Ok(id)
    }

    /// Closes exactly the block that `scope` opened.
    ///
    /// A different block on top means a nested constructor failed and its error
    /// was swallowed, so the abandoned block would otherwise be committed in
    /// place of this one.
    fn close_block(
        &mut self,
        scope: SolveScopeId,
        provenance: Span,
    ) -> Result<SolveActionBlock, SolveActionConstructionError> {
        if self.blocks.len() < 2 || self.current_block().scope != scope {
            return Err(SolveActionConstructionError::UnclosedBlock { provenance });
        }
        let open = self
            .blocks
            .pop()
            .ok_or(SolveActionConstructionError::UnclosedBlock { provenance })?;
        Ok(SolveActionBlock::issued(
            open.scope,
            open.locals,
            open.actions,
            open.provenance,
        ))
    }

    fn current_block(&self) -> &OpenBlock {
        self.blocks
            .last()
            .expect("the method root block is always open")
    }

    fn current_block_mut(&mut self) -> &mut OpenBlock {
        self.blocks
            .last_mut()
            .expect("the method root block is always open")
    }

    fn push_action(&mut self, action: SolveAction, provenance: Span) {
        self.current_block_mut()
            .actions
            .push(SolveSpannedAction::issued(action, provenance));
    }
}

fn cell_ids(cells: &[MethodCell<'_>]) -> Vec<SolveCellId> {
    cells.iter().map(|cell| cell.id).collect()
}

fn boolean_type() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::Boolean)
}

fn require_bounded_domain(
    domain: &StructuredIndexDomain,
    arithmetic: SolveArithmeticProfile,
    provenance: Span,
) -> Result<(), SolveActionConstructionError> {
    let count = domain
        .validate()
        .map_err(|_| SolveActionConstructionError::InvalidLoopDomain { provenance })?;
    let integers = arithmetic.integer_domain();
    let bounded = domain
        .binders
        .iter()
        .all(|binder| integers.contains(binder.lower) && integers.contains(binder.upper));
    if domain.binders.is_empty() || count == 0 || !bounded {
        return Err(SolveActionConstructionError::InvalidLoopDomain { provenance });
    }
    Ok(())
}
