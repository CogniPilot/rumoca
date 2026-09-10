//! Finalized views of dynamic-quotient runtime owners.

use super::*;

impl<'dae> DaeView<'dae> {
    pub fn runtime_quotient_owner_count(self) -> usize {
        self.dae.storage.runtime_quotient_owners.len()
    }

    /// The recorded runtime owner of one dynamic quotient expression, if the
    /// expression is an owned dynamic quotient.
    pub fn runtime_quotient_owner(
        self,
        expression: ExprId<'dae>,
    ) -> Option<RuntimeQuotientOwnerView<'dae>> {
        self.dae
            .storage
            .runtime_quotient_owners
            .iter()
            .find(|entry| entry.quotient == expression.index())
            .map(runtime_quotient_owner_view)
    }

    pub fn runtime_quotient_owner_at(self, index: usize) -> Option<RuntimeQuotientOwnerView<'dae>> {
        self.dae
            .storage
            .runtime_quotient_owners
            .get(index)
            .map(runtime_quotient_owner_view)
    }
}

/// The finalized owner identity of one dynamic quotient.
#[derive(Clone, Copy)]
pub struct RuntimeQuotientOwnerView<'dae> {
    quotient: ExprId<'dae>,
    builtin: crate::PureBuiltin,
    kind: RuntimeQuotientOwnerKind<'dae>,
}

/// The typed owner kind of one dynamic quotient.
#[derive(Clone, Copy)]
pub enum RuntimeQuotientOwnerKind<'dae> {
    /// A model quotient with its generated state-event surface, in the
    /// canonical generated order: ratio, pi, phase, indicator, zero,
    /// relation expression.
    ModelEvent {
        generated: [ExprId<'dae>; 6],
        relation: RelationId<'dae>,
        activation: ConditionId<'dae>,
        root: RootId<'dae>,
    },
    /// An MLS §3.7.2 event-free quotient owned by its exact function body.
    FunctionBody { function: FunctionId<'dae> },
}

impl<'dae> RuntimeQuotientOwnerView<'dae> {
    pub const fn quotient(self) -> ExprId<'dae> {
        self.quotient
    }

    pub const fn builtin(self) -> crate::PureBuiltin {
        self.builtin
    }

    pub const fn kind(self) -> RuntimeQuotientOwnerKind<'dae> {
        self.kind
    }
}

fn runtime_quotient_owner_view(
    entry: &runtime_quotients::RuntimeQuotientOwnerEntry,
) -> RuntimeQuotientOwnerView<'_> {
    RuntimeQuotientOwnerView {
        quotient: ExprId::from_raw(entry.quotient),
        builtin: entry.builtin,
        kind: match &entry.kind {
            runtime_quotients::QuotientOwnerKind::ModelEvent {
                generated,
                relation,
                activation,
                root,
            } => RuntimeQuotientOwnerKind::ModelEvent {
                generated: generated.map(ExprId::from_raw),
                relation: RelationId::from_raw(*relation),
                activation: ConditionId::from_raw(*activation),
                root: RootId::from_raw(*root),
            },
            runtime_quotients::QuotientOwnerKind::FunctionBody { function } => {
                RuntimeQuotientOwnerKind::FunctionBody {
                    function: FunctionId::from_raw(*function),
                }
            }
        },
    }
}
