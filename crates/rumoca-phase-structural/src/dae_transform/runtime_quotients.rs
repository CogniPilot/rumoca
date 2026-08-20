//! Replay construction-owned dynamic quotients without generic builtin escape.
//!
//! The source registry is collected before target mutation into dense claims
//! over the expression, relation, condition, and root streams. Function owners
//! replay only inside their exact open body. Model owners regenerate their
//! expression batch when its quotient position is reached, then consume the
//! three event artifacts at their original stream positions. Finishing the
//! plan consumes every staged token, so an omitted or reordered claim is a
//! typed construction failure rather than a silently incomplete DAE.

use rumoca_ir_dae as dae;

use super::DirectStateConstraint;
use super::constraints::DifferentiationFacts;
use super::expressions::{ExpressionRebuilder, RebuiltIdentities};

#[derive(Clone, Copy)]
enum ExpressionClaim {
    Quotient(usize),
    Generated,
}

enum ReplayOwner<'target> {
    Function {
        quotient: u32,
        builtin: dae::PureBuiltin,
        arguments: [u32; 2],
        function: u32,
        provenance: dae::DaeProvenance,
        replayed: bool,
    },
    Model {
        quotient: u32,
        builtin: dae::PureBuiltin,
        arguments: [u32; 2],
        generated: [u32; 6],
        relation: u32,
        activation: u32,
        root: u32,
        provenance: dae::DaeProvenance,
        token: Option<dae::QuotientReplayToken<'target>>,
    },
}

/// Dense source-stream plan and the linear replay capabilities it stages.
pub(super) struct RuntimeQuotientReplayPlan<'target> {
    owners: Vec<ReplayOwner<'target>>,
    expressions: Vec<Option<ExpressionClaim>>,
    relations: Vec<Option<usize>>,
    conditions: Vec<Option<usize>>,
    roots: Vec<Option<usize>>,
}

#[derive(Clone, Copy)]
pub(super) struct QuotientExpressionContext<'source, 'borrow, 'target> {
    pub(super) source: dae::DaeView<'source>,
    pub(super) identities: RebuiltIdentities<'borrow, 'target>,
    pub(super) facts: &'borrow DifferentiationFacts,
    pub(super) candidate: Option<DirectStateConstraint>,
}

impl<'target> RuntimeQuotientReplayPlan<'target> {
    pub(super) fn collect(source: dae::DaeView<'_>) -> Result<Self, dae::DaeConstructionError> {
        let mut plan = Self {
            owners: Vec::with_capacity(source.runtime_quotient_owner_count()),
            expressions: vec![None; source.expression_count()],
            relations: vec![None; source.relation_count()],
            conditions: vec![None; source.condition_count()],
            roots: vec![None; source.root_count()],
        };
        for ordinal in 0..source.runtime_quotient_owner_count() {
            let owner = source
                .runtime_quotient_owner_at(ordinal)
                .expect("finalized runtime quotient owner ordinal resolves");
            plan.push(source, owner)?;
        }
        Ok(plan)
    }

    fn push<'source>(
        &mut self,
        source: dae::DaeView<'source>,
        owner: dae::RuntimeQuotientOwnerView<'source>,
    ) -> Result<(), dae::DaeConstructionError> {
        let quotient = owner.quotient();
        let expression = source
            .expression(quotient)
            .expect("finalized runtime quotient expression resolves");
        let provenance = expression.provenance();
        let dae::ExpressionOperation::Builtin { builtin, arguments } = expression.operation()
        else {
            return Err(dae::DaeConstructionError::InvalidExpressionForm {
                span: provenance.span(),
            });
        };
        if builtin != owner.builtin() || arguments.len() != 2 {
            return Err(dae::DaeConstructionError::InvalidExpressionForm {
                span: provenance.span(),
            });
        }
        let arguments = [
            arguments
                .get(0)
                .expect("checked quotient has two operands")
                .index(),
            arguments
                .get(1)
                .expect("checked quotient has two operands")
                .index(),
        ];
        let ordinal = self.owners.len();
        claim(
            &mut self.expressions,
            quotient.index(),
            ExpressionClaim::Quotient(ordinal),
            "runtime quotient expression",
            provenance,
        )?;
        let replay = match owner.kind() {
            dae::RuntimeQuotientOwnerKind::FunctionBody { function } => ReplayOwner::Function {
                quotient: quotient.index(),
                builtin,
                arguments,
                function: function.index(),
                provenance,
                replayed: false,
            },
            dae::RuntimeQuotientOwnerKind::ModelEvent {
                generated,
                relation,
                activation,
                root,
            } => {
                for generated in generated {
                    claim(
                        &mut self.expressions,
                        generated.index(),
                        ExpressionClaim::Generated,
                        "runtime quotient generated expression",
                        provenance,
                    )?;
                }
                claim(
                    &mut self.relations,
                    relation.index(),
                    ordinal,
                    "runtime quotient relation",
                    provenance,
                )?;
                claim(
                    &mut self.conditions,
                    activation.index(),
                    ordinal,
                    "runtime quotient activation",
                    provenance,
                )?;
                claim(
                    &mut self.roots,
                    root.index(),
                    ordinal,
                    "runtime quotient root",
                    provenance,
                )?;
                ReplayOwner::Model {
                    quotient: quotient.index(),
                    builtin,
                    arguments,
                    generated: generated.map(dae::ExprId::index),
                    relation: relation.index(),
                    activation: activation.index(),
                    root: root.index(),
                    provenance,
                    token: None,
                }
            }
        };
        self.owners.push(replay);
        Ok(())
    }

    /// Begin every model owner whose source quotient is at or before `through`.
    ///
    /// Canonical quotient order means each owner's operands precede it. Earlier
    /// owners are therefore mapped before a later owner's generic operand
    /// rebuild can encounter them, including operands needed while delay
    /// coordinates are reconstructed ahead of the ordinary expression pass.
    pub(super) fn replay_model_owners_through(
        &mut self,
        through: usize,
        target: &mut dae::DaeConstruction<'target>,
        context: QuotientExpressionContext<'_, '_, 'target>,
        rebuilt: &mut [Option<dae::ExprId<'target>>],
    ) -> Result<(), dae::DaeConstructionError> {
        for ordinal in 0..self.owners.len() {
            let should_replay = matches!(
                &self.owners[ordinal],
                ReplayOwner::Model {
                    quotient,
                    token: None,
                    ..
                } if *quotient as usize <= through
            );
            if should_replay {
                self.replay_model_owner(ordinal, target, context, rebuilt)?;
            }
        }
        Ok(())
    }

    fn replay_model_owner(
        &mut self,
        ordinal: usize,
        target: &mut dae::DaeConstruction<'target>,
        context: QuotientExpressionContext<'_, '_, 'target>,
        rebuilt: &mut [Option<dae::ExprId<'target>>],
    ) -> Result<(), dae::DaeConstructionError> {
        let ReplayOwner::Model {
            quotient,
            builtin,
            arguments,
            generated,
            provenance,
            token: None,
            ..
        } = &self.owners[ordinal]
        else {
            return Err(self.stage_error(ordinal, "begin"));
        };
        let (quotient, builtin, arguments, generated, provenance) =
            (*quotient, *builtin, *arguments, *generated, *provenance);
        let arguments = target.expressions(|expressions| {
            let mut rebuilder = ExpressionRebuilder::new(
                context.source,
                expressions,
                context.identities,
                context.facts,
                context.candidate,
                rebuilt,
            );
            Ok([
                rebuilder.rebuild(source_expression(context.source, arguments[0], provenance)?)?,
                rebuilder.rebuild(source_expression(context.source, arguments[1], provenance)?)?,
            ])
        })?;
        let token = target.begin_quotient_replay(builtin, arguments, provenance)?;
        let target_ids = std::iter::once(token.quotient()).chain(token.generated());
        let source_ids = std::iter::once(quotient).chain(generated);
        for (source, target) in source_ids.zip(target_ids) {
            let slot =
                rebuilt
                    .get_mut(source as usize)
                    .ok_or(dae::DaeConstructionError::UnknownId {
                        kind: "runtime quotient generated expression",
                        index: source,
                        span: provenance.span(),
                    })?;
            if slot.replace(target).is_some() {
                return Err(dae::DaeConstructionError::DuplicateRuntimeQuotientOwner {
                    expression: quotient,
                    span: provenance.span(),
                });
            }
        }
        let ReplayOwner::Model { token: slot, .. } = &mut self.owners[ordinal] else {
            unreachable!("the checked replay ordinal retains its model kind")
        };
        *slot = Some(token);
        Ok(())
    }

    pub(super) fn replay_function_owner<'source>(
        &mut self,
        source: dae::DaeView<'source>,
        target: &mut dae::DaeConstruction<'target>,
        body: Option<&dae::FunctionBody<'target>>,
        active_function: Option<usize>,
        quotient: dae::ExprId<'source>,
        rebuilt: &mut [Option<dae::ExprId<'target>>],
    ) -> Result<bool, dae::DaeConstructionError> {
        let Some(ExpressionClaim::Quotient(ordinal)) = self
            .expressions
            .get(quotient.index() as usize)
            .copied()
            .flatten()
        else {
            return Ok(false);
        };
        let ReplayOwner::Function {
            quotient: claimed,
            builtin,
            arguments,
            function,
            provenance,
            replayed,
        } = &mut self.owners[ordinal]
        else {
            return Ok(false);
        };
        let (claimed, builtin, arguments, function, provenance) =
            (*claimed, *builtin, *arguments, *function, *provenance);
        let found = active_function.ok_or(dae::DaeConstructionError::IncompleteDefinition {
            kind: "runtime quotient function body",
            index: claimed,
            span: provenance.span(),
        })? as u32;
        if found != function {
            return Err(dae::DaeConstructionError::InvalidFunctionScope {
                expected_function: Some(function),
                found_function: found,
                span: provenance.span(),
            });
        }
        if *replayed {
            return Err(dae::DaeConstructionError::DuplicateRuntimeQuotientOwner {
                expression: claimed,
                span: provenance.span(),
            });
        }
        let source_scope = source
            .expression(quotient)
            .and_then(|expression| expression.function_scope());
        let Some(source_scope) = source_scope else {
            return Err(dae::DaeConstructionError::IncompleteDefinition {
                kind: "runtime quotient source function owner",
                index: claimed,
                span: provenance.span(),
            });
        };
        if source_scope.index() != function {
            return Err(dae::DaeConstructionError::InvalidFunctionScope {
                expected_function: Some(function),
                found_function: source_scope.index(),
                span: provenance.span(),
            });
        }
        let body = body.ok_or(dae::DaeConstructionError::IncompleteDefinition {
            kind: "runtime quotient function body",
            index: claimed,
            span: provenance.span(),
        })?;
        let mapped = |argument| {
            rebuilt.get(argument as usize).copied().flatten().ok_or(
                dae::DaeConstructionError::IncompleteDefinition {
                    kind: "runtime quotient function operand",
                    index: argument,
                    span: provenance.span(),
                },
            )
        };
        let arguments = [mapped(arguments[0])?, mapped(arguments[1])?];
        let target_quotient =
            target.function_runtime_quotient(body, builtin, arguments, provenance)?;
        rebuilt[claimed as usize] = Some(target_quotient);
        *replayed = true;
        Ok(true)
    }

    pub(super) fn relation_owner(&self, index: usize) -> Option<usize> {
        self.relations.get(index).copied().flatten()
    }

    pub(super) fn condition_owner(&self, index: usize) -> Option<usize> {
        self.conditions.get(index).copied().flatten()
    }

    pub(super) fn root_owner(&self, index: usize) -> Option<usize> {
        self.roots.get(index).copied().flatten()
    }

    pub(super) fn replay_relation(
        &mut self,
        ordinal: usize,
        target: &mut dae::DaeConstruction<'target>,
    ) -> Result<dae::RelationId<'target>, dae::DaeConstructionError> {
        let ReplayOwner::Model {
            relation,
            token: Some(token),
            ..
        } = &mut self.owners[ordinal]
        else {
            return Err(self.stage_error(ordinal, "relation"));
        };
        let expected = *relation;
        let id = target.replay_quotient_relation(token)?;
        expect_ordinal(
            "runtime quotient relation",
            expected,
            id.index(),
            token.provenance(),
        )?;
        Ok(id)
    }

    pub(super) fn replay_activation(
        &mut self,
        ordinal: usize,
        activation: dae::ConditionId<'target>,
        target: &mut dae::DaeConstruction<'target>,
    ) -> Result<(), dae::DaeConstructionError> {
        let ReplayOwner::Model {
            activation: expected,
            token: Some(token),
            ..
        } = &mut self.owners[ordinal]
        else {
            return Err(self.stage_error(ordinal, "activation"));
        };
        expect_ordinal(
            "runtime quotient activation",
            *expected,
            activation.index(),
            token.provenance(),
        )?;
        target.replay_quotient_activation(token, activation)
    }

    pub(super) fn replay_root(
        &mut self,
        ordinal: usize,
        target: &mut dae::DaeConstruction<'target>,
    ) -> Result<dae::RootId<'target>, dae::DaeConstructionError> {
        let ReplayOwner::Model {
            root,
            token: Some(token),
            ..
        } = &mut self.owners[ordinal]
        else {
            return Err(self.stage_error(ordinal, "root"));
        };
        let expected = *root;
        let id = target.replay_quotient_root(token)?;
        expect_ordinal(
            "runtime quotient root",
            expected,
            id.index(),
            token.provenance(),
        )?;
        Ok(id)
    }

    pub(super) fn finish(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
    ) -> Result<(), dae::DaeConstructionError> {
        for owner in &mut self.owners {
            match owner {
                ReplayOwner::Function {
                    quotient,
                    provenance,
                    replayed: false,
                    ..
                } => {
                    return Err(dae::DaeConstructionError::IncompleteDefinition {
                        kind: "runtime quotient function owner",
                        index: *quotient,
                        span: provenance.span(),
                    });
                }
                ReplayOwner::Function { .. } => {}
                ReplayOwner::Model {
                    quotient,
                    provenance,
                    token,
                    ..
                } => {
                    let token =
                        token
                            .take()
                            .ok_or(dae::DaeConstructionError::IncompleteDefinition {
                                kind: "runtime quotient model owner",
                                index: *quotient,
                                span: provenance.span(),
                            })?;
                    target.finish_quotient_replay(token)?;
                }
            }
        }
        Ok(())
    }

    fn stage_error(&self, ordinal: usize, stage: &'static str) -> dae::DaeConstructionError {
        let provenance = match &self.owners[ordinal] {
            ReplayOwner::Function { provenance, .. } | ReplayOwner::Model { provenance, .. } => {
                *provenance
            }
        };
        dae::DaeConstructionError::InvalidQuotientReplayStage {
            stage,
            span: provenance.span(),
        }
    }
}

fn source_expression<'source>(
    source: dae::DaeView<'source>,
    raw: u32,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'source>, dae::DaeConstructionError> {
    source
        .expression_id(raw as usize)
        .ok_or(dae::DaeConstructionError::UnknownId {
            kind: "runtime quotient operand",
            index: raw,
            span: provenance.span(),
        })
}

fn claim<T: Copy>(
    claims: &mut [Option<T>],
    index: u32,
    owner: T,
    kind: &'static str,
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let slot = claims
        .get_mut(index as usize)
        .ok_or(dae::DaeConstructionError::UnknownId {
            kind,
            index,
            span: provenance.span(),
        })?;
    if slot.replace(owner).is_some() {
        return Err(dae::DaeConstructionError::DuplicateDefinition {
            kind,
            index,
            span: provenance.span(),
        });
    }
    Ok(())
}

fn expect_ordinal(
    kind: &'static str,
    expected: u32,
    found: u32,
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    if expected == found {
        return Ok(());
    }
    Err(dae::DaeConstructionError::IncompleteDefinition {
        kind,
        index: expected,
        span: provenance.span(),
    })
}
