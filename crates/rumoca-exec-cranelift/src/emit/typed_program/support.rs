//! Per-owner admission of the native typed-call adapter.
//!
//! Which forms of which owners have a native helper is decided once, when the
//! table is built: every form's body is lowered in a scratch module, and a form
//! whose own body has no native lowering, or whose callee form has none, is
//! declined. Rows consult the recorded fact instead of retrying compilation.

use super::{CompileError, Emission, TableCompiler};
use rumoca_ir_solve as solve;
use std::convert::Infallible;

/// Why the native adapter has no helper for one form of an owner.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NativeFormDecline {
    /// The form's own body has no native lowering.
    Lowering(String),
    /// A callee's form of the same kind has no native helper.
    Callee(solve::SolvePureCallOwnerId),
    /// A directional form is admitted only beside its primal form.
    PrimalDeclined,
}

/// Whether one form of an owner has a native helper.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NativeFormSupport {
    Native,
    Declined(NativeFormDecline),
}

impl NativeFormSupport {
    pub const fn is_native(&self) -> bool {
        matches!(self, Self::Native)
    }
}

/// The adapter's admission of one owner: its primal form and, when the owner
/// has one, its directional form.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeOwnerSupport {
    primal: NativeFormSupport,
    directional: Option<NativeFormSupport>,
    observation: NativeFormSupport,
}

impl NativeOwnerSupport {
    pub const fn primal(&self) -> &NativeFormSupport {
        &self.primal
    }

    pub const fn directional(&self) -> Option<&NativeFormSupport> {
        self.directional.as_ref()
    }

    pub(super) fn all_native(table: &solve::SolvePureCallTable) -> Vec<Self> {
        table
            .owners()
            .iter()
            .map(|owner| Self {
                primal: NativeFormSupport::Native,
                directional: owner.directional().map(|_| NativeFormSupport::Native),
                observation: NativeFormSupport::Native,
            })
            .collect()
    }

    /// The adapter's admission of the assertion-observation form: the form
    /// that stops at the first failed Error-level assertion (SOLVE-C25).
    pub const fn observation(&self) -> &NativeFormSupport {
        &self.observation
    }

    pub(super) fn observation_is_native(&self) -> bool {
        self.observation.is_native()
    }

    pub(super) fn primal_is_native(&self) -> bool {
        self.primal.is_native()
    }

    pub(super) fn directional_is_native(&self) -> bool {
        self.directional
            .as_ref()
            .is_some_and(NativeFormSupport::is_native)
    }
}

/// One form of an owner while admission is being decided.
enum Form {
    Absent,
    Admitted,
    Declined(NativeFormDecline),
}

impl Form {
    const fn is_admitted(&self) -> bool {
        matches!(self, Self::Admitted)
    }

    fn lowered(outcome: Result<(), CompileError>) -> Self {
        match outcome {
            Ok(()) => Self::Admitted,
            Err(error) => Self::Declined(NativeFormDecline::Lowering(error.to_string())),
        }
    }

    fn support(self) -> Option<NativeFormSupport> {
        match self {
            Self::Absent => None,
            Self::Admitted => Some(NativeFormSupport::Native),
            Self::Declined(decline) => Some(NativeFormSupport::Declined(decline)),
        }
    }
}

/// Decide every owner's admission by lowering each form in a scratch module.
pub(super) fn decide(
    table_id: usize,
    table: &solve::SolvePureCallTable,
) -> Result<Vec<NativeOwnerSupport>, CompileError> {
    let all = NativeOwnerSupport::all_native(table);
    let mut probe = TableCompiler::new(table_id, table, Emission::Probe, &all)?;
    let mut primal = Vec::with_capacity(table.owners().len());
    let mut directional = Vec::with_capacity(table.owners().len());
    let mut observation = Vec::with_capacity(table.owners().len());
    for owner in table.owners() {
        let primal_form = Form::lowered(probe.lower_primal(table, owner));
        directional.push(match owner.directional() {
            None => Form::Absent,
            Some(_) if !primal_form.is_admitted() => {
                Form::Declined(NativeFormDecline::PrimalDeclined)
            }
            Some(_) => Form::lowered(probe.lower_directional(table, owner)),
        });
        observation.push(observation_form(table, owner, &primal_form, &mut probe));
        primal.push(primal_form);
    }
    close_over_callees(
        &mut primal,
        table.owners().iter().map(|owner| callees(owner.body())),
    );
    for (primal, observation) in primal.iter().zip(&mut observation) {
        if !primal.is_admitted() && observation.is_admitted() {
            *observation = Form::Declined(NativeFormDecline::PrimalDeclined);
        }
    }
    close_over_callees(
        &mut observation,
        table.owners().iter().map(|owner| callees(owner.body())),
    );
    close_over_callees(
        &mut directional,
        table.owners().iter().map(|owner| {
            owner
                .directional()
                .map(|form| callees(form.body()))
                .unwrap_or_default()
        }),
    );
    Ok(primal
        .into_iter()
        .zip(directional)
        .zip(observation)
        .map(|((primal, directional), observation)| NativeOwnerSupport {
            observation: observation.support().unwrap_or(NativeFormSupport::Native),
            primal: primal.support().unwrap_or(NativeFormSupport::Native),
            directional: directional.support(),
        })
        .collect())
}

/// A form that calls a form without a native helper has none either, to a
/// fixed point so a decline crosses every call chain and recursive group.
fn close_over_callees(
    forms: &mut [Form],
    callees: impl Iterator<Item = Vec<solve::SolvePureCallOwnerId>>,
) {
    let callees = callees.collect::<Vec<_>>();
    loop {
        let mut changed = false;
        for (owner, called) in callees.iter().enumerate() {
            if !forms[owner].is_admitted() {
                continue;
            }
            let declined = called.iter().copied().find(|callee| {
                forms
                    .get(callee.index() as usize)
                    .is_some_and(|form| !form.is_admitted())
            });
            if let Some(callee) = declined {
                forms[owner] = Form::Declined(NativeFormDecline::Callee(callee));
                changed = true;
            }
        }
        if !changed {
            return;
        }
    }
}

/// Every owner a body calls, through its nested regions and assertion messages.
fn callees(program: &solve::TypedProgram) -> Vec<solve::SolvePureCallOwnerId> {
    struct Calls(Vec<solve::SolvePureCallOwnerId>);
    impl solve::SolveVisitor for Calls {
        type Error = Infallible;
        fn visit_typed_operation(
            &mut self,
            _program: &solve::TypedProgram,
            _index: usize,
            operation: &solve::SolveSpannedOperation,
        ) -> Result<(), Infallible> {
            if let solve::SolveOperation::Call { owner, .. } = operation.operation()
                && !self.0.contains(owner)
            {
                self.0.push(*owner);
            }
            Ok(())
        }
    }
    let mut calls = Calls(Vec::new());
    let Ok(()) = solve::SolveVisitor::visit_typed_program(&mut calls, program);
    calls.0
}

/// The assertion-observation form of `owner`: an owner that publishes no
/// predicate observes exactly as it runs, a recursive group has no observation
/// form, and any other owner is admitted when its observation body lowers.
fn observation_form(
    table: &solve::SolvePureCallTable,
    owner: &solve::SolvePureCallOwner,
    primal: &Form,
    probe: &mut TableCompiler,
) -> Form {
    if !super::publishes_predicates(owner) {
        return Form::Admitted;
    }
    if !primal.is_admitted() {
        return Form::Declined(NativeFormDecline::PrimalDeclined);
    }
    if table.recursive_group(owner.id()).is_some() {
        return Form::Declined(NativeFormDecline::Lowering(
            "a recursive group has no native assertion observation".into(),
        ));
    }
    Form::lowered(probe.lower_observation(owner))
}
