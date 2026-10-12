use super::*;
use crate::ConditionInput;
use crate::expression::{PureBuiltin, QuotientScope};
use crate::{ConditionId, DomainId, RelationId, RootId, StructuredRootId};

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

/// The checked model or function owner of one dynamic quotient.
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
    StructuredModelEvent {
        generated: [u32; 6],
        domain: u32,
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

/// Linear capability for one in-flight model-owner replay.
///
/// This is reconstruction capability, not a producer API: only
/// [`DaeConstruction::begin_quotient_replay`] mints it, its fields remain
/// opaque, and each stage consumes exactly one owner-produced fact in source
/// order — relation, then activation definition, then root.
/// `finish_quotient_replay` verifies full consumption and clears exactly
/// this token's construction-issued pending slot; a token that never
/// finishes is caught at `finish_construction`, so no partially consumed
/// owner can reach a finalized DAE on any path, and finishing several live
/// tokens in any order settles each exact slot rather than a stack.
pub struct QuotientReplayToken<'dae> {
    guard: Option<ExprId<'dae>>,
    quotient: ExprId<'dae>,
    generated: [ExprId<'dae>; 6],
    builtin: PureBuiltin,
    generated_at: DaeProvenance,
    pending_slot: usize,
    relation: Option<RelationId<'dae>>,
    activation: Option<ConditionId<'dae>>,
    root: Option<RootId<'dae>>,
    domain: Option<DomainId<'dae>>,
    structured_root: Option<StructuredRootId<'dae>>,
}

impl<'dae> QuotientReplayToken<'dae> {
    /// The regenerated quotient expression.
    pub fn quotient(&self) -> ExprId<'dae> {
        self.quotient
    }

    /// The regenerated indicator expressions in canonical owner order.
    pub fn generated(&self) -> [ExprId<'dae>; 6] {
        self.generated
    }

    /// The generated RuntimeDiscontinuity provenance anchoring this replay.
    pub fn provenance(&self) -> DaeProvenance {
        self.generated_at
    }
}

impl<'dae> DaeConstruction<'dae> {
    fn with_quotient_surface<T>(
        &mut self,
        provenance: DaeProvenance,
        build: impl FnOnce(&mut Self) -> Result<T, DaeConstructionError>,
    ) -> Result<T, DaeConstructionError> {
        let previous = self.storage.owned_quotient_surface;
        self.storage.owned_quotient_surface = Some(provenance);
        let result = build(self);
        self.storage.owned_quotient_surface = previous;
        result
    }
    /// Construct the canonical seven-node expression batch of one dynamic
    /// quotient: the checked quotient itself and the six generated nodes of
    /// its continuous indicator `sin(pi * lhs / rhs) >= 0`, in fixed order.
    fn quotient_batch(
        &mut self,
        builtin: PureBuiltin,
        arguments: [ExprId<'dae>; 2],
        provenance: DaeProvenance,
    ) -> Result<QuotientBatch<'dae>, DaeConstructionError> {
        // A model boundary cannot import an open function's values, even
        // when those values also carry a legal compact binder domain.
        for argument in arguments {
            if let Some(found_function) = self.storage.expr_function_scope(argument, provenance)? {
                return Err(DaeConstructionError::InvalidFunctionScope {
                    expected_function: None,
                    found_function,
                    span: provenance.span(),
                });
            }
        }
        let generated_at =
            DaeProvenance::generated(DaeGeneration::RuntimeDiscontinuity, provenance.span())?;
        let quotient = self.expressions(|expressions| {
            expressions.at(provenance).checked_runtime_quotient(
                builtin,
                arguments,
                QuotientScope::Model,
            )
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
        // MLS §3.7.2/§8.5 and SPEC_0032 §§1–3: a binder-dependent
        // boundary owns its exact compact domain, never a closed scalar root.
        let mut token = self.begin_quotient_replay(builtin, arguments, provenance)?;
        let quotient = token.quotient();
        if let Some(domain) = token.domain {
            self.replay_quotient_structured_root(&mut token, domain)?;
        } else {
            self.replay_quotient_relation(&mut token)?;
            let activation =
                self.conditions(|conditions| conditions.reserve(token.generated_at))?;
            self.replay_quotient_activation(&mut token, activation)?;
            self.replay_quotient_root(&mut token)?;
        }
        self.finish_quotient_replay(token)?;
        Ok(quotient)
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
    /// node is inserted. The exact body also supplies the quotient's function
    /// scope when literal/binder-only operands carry no scope of their own. A
    /// rejected quotient leaves the expression arena untouched instead of
    /// stranding an eventless dynamic node behind a late `Err`.
    pub fn function_runtime_quotient(
        &mut self,
        body: &FunctionBody<'dae>,
        builtin: PureBuiltin,
        arguments: [ExprId<'dae>; 2],
        provenance: DaeProvenance,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let quotient = self.expressions(|expressions| {
            expressions
                .at(provenance)
                .checked_function_runtime_quotient(body, builtin, arguments)
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
        // The slot index is the construction-issued identity finish must
        // settle; several live tokens each clear exactly their own slot.
        let pending_slot = self.storage.pending_quotient_replays.len();
        self.storage.pending_quotient_replays.push(Some(provenance));
        Ok(QuotientReplayToken {
            guard: self
                .storage
                .active_model_arm
                .map(|arm| ExprId::from_raw(self.storage.model_arms[arm as usize].guard)),
            quotient: batch.quotient,
            generated: batch.generated,
            builtin,
            generated_at: batch.generated_at,
            pending_slot,
            relation: None,
            activation: None,
            root: None,
            domain: self
                .storage
                .expr_binder_domain(batch.quotient, provenance)?
                .map(DomainId::from_raw),
            structured_root: None,
        })
    }

    /// Re-issue the owner's relation from its own regenerated relation
    /// expression. First stage; consumable exactly once.
    pub fn replay_quotient_relation(
        &mut self,
        token: &mut QuotientReplayToken<'dae>,
    ) -> Result<RelationId<'dae>, DaeConstructionError> {
        if token.domain.is_some() || token.relation.is_some() {
            return Err(DaeConstructionError::InvalidQuotientReplayStage {
                stage: "relation",
                span: token.generated_at.span(),
            });
        }
        let relation = self.with_quotient_surface(token.generated_at, |construction| {
            construction.conditions(|conditions| {
                conditions.relation(token.generated[5], token.generated_at)
            })
        })?;
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
        // The reservation is semantic input, but its stored provenance is an
        // owner-produced fact: it must be exactly this replay's canonical
        // generated provenance, or a foreign reservation would smuggle a
        // different finalized span through the omitted definition.
        let reserved = self
            .storage
            .conditions
            .get(activation.index() as usize)
            .map(|entry| entry.provenance);
        if reserved != Some(token.generated_at) {
            return Err(DaeConstructionError::InvalidQuotientReplayStage {
                stage: "activation",
                span: token.generated_at.span(),
            });
        }
        let generated_at = token.generated_at;
        self.with_quotient_surface(token.generated_at, |construction| {
            construction.conditions(|conditions| {
                conditions.define(
                    activation,
                    token
                        .guard
                        .map_or(ConditionInput::Always, ConditionInput::Discrete),
                    generated_at,
                )
            })
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

    /// Re-issue the exact structured boundary from a domain-scoped batch.
    /// The supplied identity must equal the batch's constructor-derived domain;
    /// scalar stages and a second structured-root stage are refused.
    pub fn replay_quotient_structured_root(
        &mut self,
        token: &mut QuotientReplayToken<'dae>,
        domain: DomainId<'dae>,
    ) -> Result<StructuredRootId<'dae>, DaeConstructionError> {
        if token.domain != Some(domain) || token.structured_root.is_some() {
            return Err(DaeConstructionError::InvalidQuotientReplayStage {
                stage: "structured root",
                span: token.generated_at.span(),
            });
        }
        let root = self.with_quotient_surface(token.generated_at, |construction| {
            construction.conditions(|conditions| {
                conditions.structured_root_activated(
                    domain,
                    token.generated[5],
                    token.guard,
                    token.generated_at,
                )
            })
        })?;
        token.structured_root = Some(root);
        Ok(root)
    }

    /// Verify the token is fully consumed, settle exactly its pending slot,
    /// and record the regenerated owner.
    pub fn finish_quotient_replay(
        &mut self,
        token: QuotientReplayToken<'dae>,
    ) -> Result<(), DaeConstructionError> {
        let kind = match (
            token.domain,
            token.structured_root,
            token.relation,
            token.activation,
            token.root,
        ) {
            (Some(domain), Some(root), None, None, None) => {
                QuotientOwnerKind::StructuredModelEvent {
                    generated: token.generated.map(ExprId::index),
                    domain: domain.index(),
                    root: root.index(),
                }
            }
            (None, None, Some(relation), Some(activation), Some(root)) => {
                QuotientOwnerKind::ModelEvent {
                    generated: token.generated.map(ExprId::index),
                    relation: relation.index(),
                    activation: activation.index(),
                    root: root.index(),
                }
            }
            _ => {
                return Err(DaeConstructionError::UnconsumedQuotientReplay {
                    span: token.generated_at.span(),
                });
            }
        };
        let settled = self
            .storage
            .pending_quotient_replays
            .get_mut(token.pending_slot)
            .and_then(Option::take);
        if settled.is_none() {
            return Err(DaeConstructionError::InvalidQuotientReplayStage {
                stage: "finish",
                span: token.generated_at.span(),
            });
        }
        self.storage.record_quotient_owner(
            RuntimeQuotientOwnerEntry {
                quotient: token.quotient.index(),
                builtin: token.builtin,
                kind,
            },
            token.generated_at,
        )
    }
}

impl<'dae> FunctionBody<'dae> {
    /// The open body is the authority for an event-free quotient's scope.
    /// Validate its exact operand domains and current SSA definitions before
    /// expression construction commits the function-owned arena row.
    pub(crate) fn checked_quotient_scope(
        &self,
        storage: &Storage,
        arguments: [ExprId<'dae>; 2],
        provenance: DaeProvenance,
    ) -> Result<FunctionId<'dae>, DaeConstructionError> {
        for argument in arguments {
            expect_function_body_expression(storage, self, argument, provenance)?;
            validate_function_value_reads(storage, self, argument, provenance)?;
        }
        Ok(self.function)
    }
}

impl Storage {
    /// Record one owner, keeping the registry canonical in
    /// quotient-expression order on EVERY recording path.
    ///
    /// Construction and replay reach this in different orders — replay
    /// records function owners during expression reconstruction but model
    /// owners only after their root markers — so ordered insertion (with
    /// exact ordinal repair of the expression lookup) is what makes the
    /// public view and the structural consumer independent of marker
    /// timing.
    pub(crate) fn record_quotient_owner(
        &mut self,
        entry: RuntimeQuotientOwnerEntry,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        checked_u32(
            self.runtime_quotient_owners.len(),
            "runtime quotient owner registry",
            at,
        )?;
        if self
            .runtime_quotient_owner_by_expression
            .contains_key(&entry.quotient)
        {
            return Err(DaeConstructionError::DuplicateRuntimeQuotientOwner {
                expression: entry.quotient,
                span: at.span(),
            });
        }
        let position = self
            .runtime_quotient_owners
            .partition_point(|existing| existing.quotient < entry.quotient);
        self.runtime_quotient_owners.insert(position, entry);
        for (ordinal, existing) in self
            .runtime_quotient_owners
            .iter()
            .enumerate()
            .skip(position)
        {
            self.runtime_quotient_owner_by_expression
                .insert(existing.quotient, ordinal as u32);
        }
        Ok(())
    }
}
