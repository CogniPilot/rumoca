use super::*;
use crate::ConditionInput;
use crate::expression::PureBuiltin;
use crate::{ConditionId, RelationId, RootId};

/// Finalized identity of one dynamic-quotient owner.
///
/// Construction appends an entry atomically with the owner it builds; the
/// registry is the ONLY authority that a dynamic quotient is owned. The
/// recorded ids are finalized-DAE metadata for views and structural replay;
/// the wire never accepts them as facts — replay regenerates and verifies
/// them through the same checked operations.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct RuntimeQuotientOwnerEntry {
    pub(crate) quotient: u32,
    pub(crate) builtin: PureBuiltin,
    pub(crate) kind: QuotientOwnerKind,
}

/// The two owner kinds a dynamic quotient can have.
///
/// A model quotient owns a state-event surface: six generated expression
/// nodes in canonical order (ratio, pi, phase, indicator, zero, relation
/// expression), one relation, one Always activation, one discontinuity root.
/// A function-body quotient is MLS §3.7.2 event-free: its owner is the exact
/// function whose open body proved the construction.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum QuotientOwnerKind {
    ModelEvent {
        generated: [u32; 6],
        relation: u32,
        activation: u32,
        root: u32,
    },
    FunctionBody {
        function: u32,
    },
}

/// The canonical generated batch of one model quotient owner.
struct QuotientBatch<'dae> {
    quotient: ExprId<'dae>,
    generated: [ExprId<'dae>; 6],
    generated_at: DaeProvenance,
}

/// One in-flight model-owner replay.
///
/// The token is construction-owned: only `begin_quotient_replay` mints it,
/// its fields are private, and each stage consumes exactly one owner-produced
/// fact in source order — relation, then activation definition, then root.
/// `finish_quotient_replay` verifies full consumption and records the
/// registry entry; a token that never finishes is caught at
/// `finish_construction`, so no partially consumed owner can reach a
/// finalized DAE on any path.
pub struct QuotientReplayToken<'dae> {
    quotient: ExprId<'dae>,
    generated: [ExprId<'dae>; 6],
    builtin: PureBuiltin,
    generated_at: DaeProvenance,
    relation: Option<RelationId<'dae>>,
    activation: Option<ConditionId<'dae>>,
    root: Option<RootId<'dae>>,
}

impl<'dae> QuotientReplayToken<'dae> {
    pub fn quotient(&self) -> ExprId<'dae> {
        self.quotient
    }

    pub fn generated(&self) -> [ExprId<'dae>; 6] {
        self.generated
    }

    /// The generated RuntimeDiscontinuity provenance anchoring this replay.
    pub fn provenance(&self) -> DaeProvenance {
        self.generated_at
    }
}

impl<'dae> DaeConstruction<'dae> {
    /// Construct the canonical seven-node expression batch of one dynamic
    /// quotient: the checked quotient itself and the six generated nodes of
    /// its continuous indicator `sin(pi * lhs / rhs) >= 0`, in fixed order.
    fn quotient_batch(
        &mut self,
        builtin: PureBuiltin,
        arguments: [ExprId<'dae>; 2],
        provenance: DaeProvenance,
    ) -> Result<QuotientBatch<'dae>, DaeConstructionError> {
        let generated_at =
            DaeProvenance::generated(DaeGeneration::RuntimeDiscontinuity, provenance.span())?;
        let quotient = self.expressions(|expressions| {
            expressions
                .at(provenance)
                .checked_runtime_quotient(builtin, arguments)
        })?;
        let generated = self.expressions(|expressions| {
            let ratio = expressions.at(generated_at).binary(
                BinaryOperator::Divide,
                arguments[0],
                arguments[1],
            )?;
            let pi = expressions
                .at(generated_at)
                .literal(DaeLiteral::Real(std::f64::consts::PI))?;
            let phase = expressions
                .at(generated_at)
                .binary(BinaryOperator::Multiply, pi, ratio)?;
            let indicator = expressions
                .at(generated_at)
                .builtin(PureBuiltin::Sin, [phase])?;
            let zero = expressions
                .at(generated_at)
                .literal(DaeLiteral::Real(0.0))?;
            let relation_expression = expressions.at(generated_at).binary(
                BinaryOperator::GreaterEqual,
                indicator,
                zero,
            )?;
            Ok([ratio, pi, phase, indicator, zero, relation_expression])
        })?;
        Ok(QuotientBatch {
            quotient,
            generated,
            generated_at,
        })
    }

    /// Construct a scalar runtime quotient together with its checked
    /// state-event surface, and record its owner identity.
    ///
    /// MLS discontinuities occur whenever `lhs / rhs` crosses an integer. The
    /// continuous indicator `sin(pi * lhs / rhs)` has exactly those integer
    /// quotient boundaries as zeros. Requiring a finite, nonzero static
    /// divisor keeps this compact owner both defined and scalar; a varying
    /// divisor or shaped quotient needs a different checked owner.
    pub fn runtime_quotient(
        &mut self,
        builtin: PureBuiltin,
        arguments: [ExprId<'dae>; 2],
        provenance: DaeProvenance,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let batch = self.quotient_batch(builtin, arguments, provenance)?;
        let generated_at = batch.generated_at;
        let relation =
            self.conditions(|conditions| conditions.relation(batch.generated[5], generated_at))?;
        let activation = self.conditions(|conditions| {
            let activation = conditions.reserve(generated_at)?;
            conditions.define(activation, ConditionInput::Always, generated_at)?;
            Ok(activation)
        })?;
        let root =
            self.conditions(|conditions| conditions.root(relation, activation, generated_at))?;
        self.storage.record_quotient_owner(
            RuntimeQuotientOwnerEntry {
                quotient: batch.quotient.index(),
                builtin,
                kind: QuotientOwnerKind::ModelEvent {
                    generated: batch.generated.map(ExprId::index),
                    relation: relation.index(),
                    activation: activation.index(),
                    root: root.index(),
                },
            },
            provenance,
        )?;
        Ok(batch.quotient)
    }

    /// Construct a runtime quotient inside one exact function body, and
    /// record its owner identity.
    ///
    /// MLS §3.7.2 exempts function bodies from event generation: the
    /// quotient keeps the same time-invariant divisor admission, but no
    /// discontinuity root exists to own — a root would smuggle a
    /// function-scope expression into the model's condition system. The
    /// `FunctionBody` capability is the SPEC_0036 proof that a body is
    /// open. Both operands are validated against that exact body before any
    /// node is inserted: a pure builtin contributes no scope or read facts
    /// beyond its operands, so operand prevalidation is the proof, and a
    /// rejected quotient leaves the expression arena untouched instead of
    /// stranding an eventless dynamic node behind a late `Err`.
    pub fn function_runtime_quotient(
        &mut self,
        body: &FunctionBody<'dae>,
        builtin: PureBuiltin,
        arguments: [ExprId<'dae>; 2],
        provenance: DaeProvenance,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        for argument in arguments {
            expect_function_body_expression(self.storage, body, argument, provenance)?;
            validate_function_value_reads(self.storage, body, argument, provenance)?;
        }
        let quotient = self.expressions(|expressions| {
            expressions
                .at(provenance)
                .checked_runtime_quotient(builtin, arguments)
        })?;
        self.storage.record_quotient_owner(
            RuntimeQuotientOwnerEntry {
                quotient: quotient.index(),
                builtin,
                kind: QuotientOwnerKind::FunctionBody {
                    function: body.function.index(),
                },
            },
            provenance,
        )?;
        Ok(quotient)
    }

    /// Begin replaying one model quotient owner: re-run the checked batch
    /// construction and return the staged token that the relation,
    /// activation, and root positions consume in source order.
    pub fn begin_quotient_replay(
        &mut self,
        builtin: PureBuiltin,
        arguments: [ExprId<'dae>; 2],
        provenance: DaeProvenance,
    ) -> Result<QuotientReplayToken<'dae>, DaeConstructionError> {
        let batch = self.quotient_batch(builtin, arguments, provenance)?;
        self.storage.pending_quotient_replays.push(provenance);
        Ok(QuotientReplayToken {
            quotient: batch.quotient,
            generated: batch.generated,
            builtin,
            generated_at: batch.generated_at,
            relation: None,
            activation: None,
            root: None,
        })
    }

    /// Re-issue the owner's relation from its own regenerated relation
    /// expression. First stage; consumable exactly once.
    pub fn replay_quotient_relation(
        &mut self,
        token: &mut QuotientReplayToken<'dae>,
    ) -> Result<RelationId<'dae>, DaeConstructionError> {
        if token.relation.is_some() {
            return Err(DaeConstructionError::InvalidQuotientReplayStage {
                stage: "relation",
                span: token.generated_at.span(),
            });
        }
        let relation = self
            .conditions(|conditions| conditions.relation(token.generated[5], token.generated_at))?;
        token.relation = Some(relation);
        Ok(relation)
    }

    /// Define the owner's exact pre-reserved activation as Always. Second
    /// stage; requires the relation stage and a still-undefined reservation.
    pub fn replay_quotient_activation(
        &mut self,
        token: &mut QuotientReplayToken<'dae>,
        activation: ConditionId<'dae>,
    ) -> Result<(), DaeConstructionError> {
        if token.relation.is_none() || token.activation.is_some() {
            return Err(DaeConstructionError::InvalidQuotientReplayStage {
                stage: "activation",
                span: token.generated_at.span(),
            });
        }
        let generated_at = token.generated_at;
        self.conditions(|conditions| {
            conditions.define(activation, ConditionInput::Always, generated_at)
        })?;
        token.activation = Some(activation);
        Ok(())
    }

    /// Re-issue the owner's root from ITS relation and activation. Third
    /// stage; requires both earlier stages.
    pub fn replay_quotient_root(
        &mut self,
        token: &mut QuotientReplayToken<'dae>,
    ) -> Result<RootId<'dae>, DaeConstructionError> {
        let (Some(relation), Some(activation)) = (token.relation, token.activation) else {
            return Err(DaeConstructionError::InvalidQuotientReplayStage {
                stage: "root",
                span: token.generated_at.span(),
            });
        };
        if token.root.is_some() {
            return Err(DaeConstructionError::InvalidQuotientReplayStage {
                stage: "root",
                span: token.generated_at.span(),
            });
        }
        let root = self
            .conditions(|conditions| conditions.root(relation, activation, token.generated_at))?;
        token.root = Some(root);
        Ok(root)
    }

    /// Verify the token is fully consumed and record the regenerated owner.
    pub fn finish_quotient_replay(
        &mut self,
        token: QuotientReplayToken<'dae>,
    ) -> Result<(), DaeConstructionError> {
        let (Some(relation), Some(activation), Some(root)) =
            (token.relation, token.activation, token.root)
        else {
            return Err(DaeConstructionError::UnconsumedQuotientReplay {
                span: token.generated_at.span(),
            });
        };
        self.storage.pending_quotient_replays.pop();
        self.storage.record_quotient_owner(
            RuntimeQuotientOwnerEntry {
                quotient: token.quotient.index(),
                builtin: token.builtin,
                kind: QuotientOwnerKind::ModelEvent {
                    generated: token.generated.map(ExprId::index),
                    relation: relation.index(),
                    activation: activation.index(),
                    root: root.index(),
                },
            },
            token.generated_at,
        )
    }
}

impl Storage {
    pub(crate) fn record_quotient_owner(
        &mut self,
        entry: RuntimeQuotientOwnerEntry,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let ordinal = checked_u32(
            self.runtime_quotient_owners.len(),
            "runtime quotient owner registry",
            at,
        )?;
        if self
            .runtime_quotient_owner_by_expression
            .insert(entry.quotient, ordinal)
            .is_some()
        {
            return Err(DaeConstructionError::DuplicateRuntimeQuotientOwner {
                expression: entry.quotient,
                span: at.span(),
            });
        }
        self.runtime_quotient_owners.push(entry);
        Ok(())
    }
}
