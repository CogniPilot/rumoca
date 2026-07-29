use std::marker::PhantomData;

use crate::model::{Storage, check_provenance, checked_u32, duplicate, unknown};
use crate::{
    ClockId, ConditionId, DaeConstructionError, DaeProvenance, ExprId, RelationId, RootId,
    ScalarType,
};

#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ConditionNode {
    Initial,
    Relation(u32),
    Discrete(u32),
    Clock(u32),
    Not(u32),
    And { lhs: u32, rhs: u32 },
    Or { lhs: u32, rhs: u32 },
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct RelationEntry {
    pub(crate) expression: u32,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ConditionEntry {
    pub(crate) node: Option<ConditionNode>,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct RootEntry {
    pub(crate) relation: u32,
    pub(crate) activation: u32,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy)]
pub struct RelationView<'dae> {
    pub(crate) expression: ExprId<'dae>,
    pub(crate) provenance: DaeProvenance,
}

impl<'dae> RelationView<'dae> {
    pub const fn expression(self) -> ExprId<'dae> {
        self.expression
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ConditionOperation<'dae> {
    Initial,
    Relation(RelationId<'dae>),
    Discrete(ExprId<'dae>),
    Clock(ClockId<'dae>),
    Not(ConditionId<'dae>),
    And(ConditionId<'dae>, ConditionId<'dae>),
    Or(ConditionId<'dae>, ConditionId<'dae>),
}

#[derive(Debug, Clone, Copy)]
pub struct ConditionView<'dae> {
    pub(crate) operation: ConditionOperation<'dae>,
    pub(crate) provenance: DaeProvenance,
}

impl<'dae> ConditionView<'dae> {
    pub const fn operation(self) -> ConditionOperation<'dae> {
        self.operation
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RootView<'dae> {
    pub(crate) relation: RelationId<'dae>,
    pub(crate) activation: ConditionId<'dae>,
    pub(crate) provenance: DaeProvenance,
}

impl<'dae> RootView<'dae> {
    pub const fn relation(self) -> RelationId<'dae> {
        self.relation
    }

    pub const fn activation(self) -> ConditionId<'dae> {
        self.activation
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ConditionInput<'dae> {
    Initial,
    Relation(RelationId<'dae>),
    Discrete(ExprId<'dae>),
    Clock(ClockId<'dae>),
    Not(ConditionId<'dae>),
    And(ConditionId<'dae>, ConditionId<'dae>),
    Or(ConditionId<'dae>, ConditionId<'dae>),
}

pub struct Conditions<'storage, 'dae> {
    pub(crate) source_map: &'storage rumoca_core::SourceMap,
    pub(crate) storage: &'storage mut Storage,
    pub(crate) marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> Conditions<'_, 'dae> {
    pub fn relation(
        &mut self,
        expression: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<RelationId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.storage
            .expect_primitive_relation(expression, provenance)?;
        if let Some(index) = self
            .storage
            .relations
            .iter()
            .position(|entry| entry.expression == expression.index())
        {
            return Ok(RelationId::from_raw(index as u32));
        }
        let raw = checked_u32(self.storage.relations.len(), "relation arena", provenance)?;
        self.storage.relations.push(RelationEntry {
            expression: expression.index(),
            provenance,
        });
        Ok(RelationId::from_raw(raw))
    }

    pub fn reserve(
        &mut self,
        provenance: DaeProvenance,
    ) -> Result<ConditionId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let raw = checked_u32(self.storage.conditions.len(), "condition arena", provenance)?;
        self.storage.conditions.push(ConditionEntry {
            node: None,
            provenance,
        });
        self.storage.unfilled_conditions += 1;
        Ok(ConditionId::from_raw(raw))
    }

    pub fn define(
        &mut self,
        condition: ConditionId<'dae>,
        input: ConditionInput<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let node = self.checked_node(input, provenance)?;
        let Some(entry) = self.storage.conditions.get_mut(condition.index() as usize) else {
            return Err(unknown("condition", condition.index(), provenance));
        };
        if entry.node.is_some() {
            return Err(duplicate("condition", condition.index(), provenance));
        }
        entry.node = Some(node);
        self.storage.unfilled_conditions -= 1;
        Ok(())
    }

    pub fn root(
        &mut self,
        relation: RelationId<'dae>,
        activation: ConditionId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<RootId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.storage
            .relations
            .get(relation.index() as usize)
            .ok_or_else(|| unknown("relation", relation.index(), provenance))?;
        let condition = self
            .storage
            .conditions
            .get(activation.index() as usize)
            .ok_or_else(|| unknown("condition", activation.index(), provenance))?;
        if condition.node.is_none() {
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "root activation condition",
                index: activation.index(),
                span: provenance.span(),
            });
        }
        let raw = checked_u32(self.storage.roots.len(), "root arena", provenance)?;
        self.storage.roots.push(RootEntry {
            relation: relation.index(),
            activation: activation.index(),
            provenance,
        });
        Ok(RootId::from_raw(raw))
    }

    fn checked_node(
        &self,
        input: ConditionInput<'dae>,
        at: DaeProvenance,
    ) -> Result<ConditionNode, DaeConstructionError> {
        Ok(match input {
            ConditionInput::Initial => ConditionNode::Initial,
            ConditionInput::Relation(id) => {
                self.storage
                    .relations
                    .get(id.index() as usize)
                    .ok_or_else(|| unknown("relation", id.index(), at))?;
                ConditionNode::Relation(id.index())
            }
            ConditionInput::Discrete(expression) => {
                self.storage.expect_closed_expression(expression, at)?;
                let ty = self.storage.expr_type(expression, at)?;
                if !ty.is_scalar() || ty.scalar_type() != ScalarType::Boolean {
                    return Err(DaeConstructionError::TypeMismatch {
                        expected: ScalarType::Boolean,
                        found: ty.scalar_type(),
                        span: at.span(),
                    });
                }
                ConditionNode::Discrete(expression.index())
            }
            ConditionInput::Clock(id) => {
                self.storage
                    .clocks
                    .get(id.index() as usize)
                    .ok_or_else(|| unknown("clock", id.index(), at))?;
                ConditionNode::Clock(id.index())
            }
            ConditionInput::Not(id) => {
                self.expect_condition(id, at)?;
                ConditionNode::Not(id.index())
            }
            ConditionInput::And(lhs, rhs) => {
                self.expect_condition(lhs, at)?;
                self.expect_condition(rhs, at)?;
                ConditionNode::And {
                    lhs: lhs.index(),
                    rhs: rhs.index(),
                }
            }
            ConditionInput::Or(lhs, rhs) => {
                self.expect_condition(lhs, at)?;
                self.expect_condition(rhs, at)?;
                ConditionNode::Or {
                    lhs: lhs.index(),
                    rhs: rhs.index(),
                }
            }
        })
    }

    fn expect_condition(
        &self,
        id: ConditionId<'dae>,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let entry = self
            .storage
            .conditions
            .get(id.index() as usize)
            .ok_or_else(|| unknown("condition", id.index(), at))?;
        if entry.node.is_none() {
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "condition operand",
                index: id.index(),
                span: at.span(),
            });
        }
        Ok(())
    }
}
